//! Notification service implementation.
//!
//! This module provides functionality to send notifications through various channels
//! Supports variable substitution in message templates.

use async_trait::async_trait;

use std::{collections::HashMap, sync::Arc};

mod email;
mod error;
pub mod payload_builder;
mod pool;
mod script;
mod webhook;

use crate::{
	models::{MonitorMatch, ScriptLanguage, Trigger, TriggerType, TriggerTypeConfig},
	utils::{normalize_string, HttpRetryConfig},
};

pub use email::{EmailContent, EmailNotifier, SmtpConfig};
pub use error::NotificationError;
pub use payload_builder::{
	DiscordPayloadBuilder, GenericWebhookPayloadBuilder, SlackPayloadBuilder,
	TelegramPayloadBuilder, WebhookPayloadBuilder,
};
pub use pool::NotificationClientPool;
pub use script::ScriptNotifier;
pub use webhook::{WebhookConfig, WebhookNotifier};

/// A trait for trigger configurations that can be sent via webhook.
/// This abstracts away the specific details of each webhook provider.
trait Webhookable {
	/// Consolidates the logic for creating webhook components from a trigger config.
	/// It returns the generic `WebhookConfig`, HttpRetryConfig and the specific `WebhookPayloadBuilder`
	/// needed for the given trigger type.
	fn to_webhook_components(
		&self,
	) -> Result<
		(
			WebhookConfig,
			HttpRetryConfig,
			Box<dyn WebhookPayloadBuilder>,
		),
		NotificationError,
	>;
}

impl Webhookable for TriggerTypeConfig {
	fn to_webhook_components(
		&self,
	) -> Result<
		(
			WebhookConfig,
			HttpRetryConfig,
			Box<dyn WebhookPayloadBuilder>,
		),
		NotificationError,
	> {
		let (url, message, method, secret, headers, builder): (
			String,
			crate::models::NotificationMessage,
			Option<String>,
			Option<String>,
			Option<std::collections::HashMap<String, String>>,
			Box<dyn WebhookPayloadBuilder>,
		) = match self {
			TriggerTypeConfig::Webhook {
				url,
				message,
				method,
				secret,
				headers,
				..
			} => (
				url.as_ref().to_string(),
				message.clone(),
				method.clone(),
				secret.as_ref().map(|s| s.as_ref().to_string()),
				headers.clone(),
				Box::new(GenericWebhookPayloadBuilder),
			),
			TriggerTypeConfig::Discord {
				discord_url,
				message,
				..
			} => (
				discord_url.as_ref().to_string(),
				message.clone(),
				Some("POST".to_string()),
				None,
				None,
				Box::new(DiscordPayloadBuilder),
			),
			TriggerTypeConfig::Telegram {
				token,
				message,
				chat_id,
				disable_web_preview,
				..
			} => (
				format!("https://api.telegram.org/bot{}/sendMessage", token),
				message.clone(),
				Some("POST".to_string()),
				None,
				None,
				Box::new(TelegramPayloadBuilder {
					chat_id: chat_id.clone(),
					disable_web_preview: disable_web_preview.unwrap_or(false),
				}),
			),
			TriggerTypeConfig::Slack {
				slack_url, message, ..
			} => (
				slack_url.as_ref().to_string(),
				message.clone(),
				Some("POST".to_string()),
				None,
				None,
				Box::new(SlackPayloadBuilder),
			),
			_ => {
				return Err(NotificationError::config_error(
					format!("Trigger type is not webhook-compatible: {:?}", self),
					None,
					None,
				))
			}
		};

		// Construct the final WebhookConfig from the extracted parts.
		let config = WebhookConfig {
			url,
			title: message.title,
			body_template: message.body,
			method,
			secret,
			headers,
			url_params: None,
			payload_fields: None,
		};

		// Use the retry policy from the trigger config, or default if not specified.
		let retry_policy = self.get_retry_policy().ok_or_else(|| {
			NotificationError::config_error(
				"Webhook trigger config is unexpectedly missing a retry policy.",
				None,
				None,
			)
		})?;

		Ok((config, retry_policy, builder))
	}
}

/// Interface for executing scripts
///
/// This Interface is used to execute scripts for notifications.
/// It is implemented by the ScriptNotifier struct.
#[async_trait]
pub trait ScriptExecutor {
	/// Executes a script to send a custom notifications
	///
	/// # Arguments
	/// * `monitor_match` - The monitor match to send
	/// * `script_content` - The script content to execute
	///
	/// # Returns
	/// * `Result<(), NotificationError>` - Success or error
	async fn script_notify(
		&self,
		monitor_match: &MonitorMatch,
		script_content: &(ScriptLanguage, String),
	) -> Result<(), NotificationError>;
}

/// Service for managing notifications across different channels
pub struct NotificationService {
	/// Client pool for managing notification clients (HTTP, SMTP)
	client_pool: Arc<NotificationClientPool>,
}

impl NotificationService {
	/// Creates a new notification service instance
	pub fn new() -> Self {
		NotificationService {
			client_pool: Arc::new(NotificationClientPool::new()),
		}
	}

	/// Executes a notification based on the trigger configuration
	///
	/// # Arguments
	/// * `trigger` - Trigger containing the notification type and parameters
	/// * `variables` - Variables to substitute in message templates
	/// * `monitor_match` - Monitor match to send (needed for custom script trigger)
	/// * `trigger_scripts` - Contains the script content to execute (needed for custom script
	///   trigger)
	///
	/// # Returns
	/// * `Result<(), NotificationError>` - Success or error
	pub async fn execute(
		&self,
		trigger: &Trigger,
		variables: &HashMap<String, String>,
		monitor_match: &MonitorMatch,
		trigger_scripts: &HashMap<String, (ScriptLanguage, String)>,
	) -> Result<(), NotificationError> {
		match &trigger.trigger_type {
			// Match Webhook-based triggers
			TriggerType::Slack
			| TriggerType::Discord
			| TriggerType::Webhook
			| TriggerType::Telegram => {
				// Use the Webhookable trait to get config, retry policy and payload builder
				let (webhook_config, retry_policy, payload_builder) =
					trigger.config.to_webhook_components()?;

				// Get or create the HTTP client from the pool based on the retry policy
				let http_client = self
					.client_pool
					.get_or_create_http_client(&retry_policy)
					.await
					.map_err(|e| {
						NotificationError::execution_error(
							"Failed to get or create HTTP client from pool".to_string(),
							Some(e.into()),
							None,
						)
					})?;

				// Build the payload
				let payload = payload_builder.build_payload(
					&webhook_config.title,
					&webhook_config.body_template,
					variables,
				);

				// Create the notifier
				let notifier = WebhookNotifier::new(webhook_config, http_client)?;

				notifier.notify_json(&payload).await?;
			}
			TriggerType::Email => {
				// Extract SMTP configuration from the trigger
				let smtp_config = match &trigger.config {
					TriggerTypeConfig::Email {
						host,
						port,
						username,
						password,
						..
					} => SmtpConfig {
						host: host.clone(),
						port: port.unwrap_or(465),
						username: username.as_ref().to_string(),
						password: password.as_ref().to_string(),
					},
					_ => {
						return Err(NotificationError::config_error(
							"Invalid email configuration".to_string(),
							None,
							None,
						));
					}
				};

				// Get or create the SMTP client from the pool
				let smtp_client = self
					.client_pool
					.get_or_create_smtp_client(&smtp_config)
					.await
					.map_err(|e| {
						NotificationError::execution_error(
							"Failed to get SMTP client from pool".to_string(),
							Some(e.into()),
							None,
						)
					})?;

				let notifier = EmailNotifier::from_config(&trigger.config, smtp_client)?;
				let message = notifier.format_message(variables);
				notifier.notify(&message).await?;
			}
			TriggerType::Script => {
				let notifier = ScriptNotifier::from_config(&trigger.config)?;
				let monitor_name = match monitor_match {
					MonitorMatch::EVM(evm_match) => &evm_match.monitor.name,
					MonitorMatch::Stellar(stellar_match) => &stellar_match.monitor.name,
				};
				let script_path = match &trigger.config {
					TriggerTypeConfig::Script { script_path, .. } => script_path,
					_ => {
						return Err(NotificationError::config_error(
							"Invalid script configuration".to_string(),
							None,
							None,
						));
					}
				};
				let script = trigger_scripts
					.get(&format!(
						"{}|{}",
						normalize_string(monitor_name),
						script_path
					))
					.ok_or_else(|| {
						NotificationError::config_error(
							"Script content not found".to_string(),
							None,
							None,
						)
					});
				let script_content = match &script {
					Ok(content) => content,
					Err(e) => {
						return Err(NotificationError::config_error(e.to_string(), None, None));
					}
				};

				notifier
					.script_notify(monitor_match, script_content)
					.await?;
			}
		}
		Ok(())
	}
}

impl Default for NotificationService {
	fn default() -> Self {
		Self::new()
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use crate::{
		models::{
			AddressWithSpec, EVMMonitorMatch, EVMTransactionReceipt, EventCondition,
			FunctionCondition, MatchConditions, Monitor, MonitorMatch, ScriptLanguage,
			TransactionCondition, TriggerType,
		},
		utils::tests::{
			builders::{evm::monitor::MonitorBuilder, trigger::TriggerBuilder},
			evm::transaction::TransactionBuilder,
		},
	};
	use std::collections::HashMap;

	fn create_test_monitor(
		event_conditions: Vec<EventCondition>,
		function_conditions: Vec<FunctionCondition>,
		transaction_conditions: Vec<TransactionCondition>,
		addresses: Vec<AddressWithSpec>,
	) -> Monitor {
		let mut builder = MonitorBuilder::new()
			.name("test")
			.networks(vec!["evm_mainnet".to_string()]);

		// Add all conditions
		for event in event_conditions {
			builder = builder.event(&event.signature, event.expression);
		}
		for function in function_conditions {
			builder = builder.function(&function.signature, function.expression);
		}
		for transaction in transaction_conditions {
			builder = builder.transaction(transaction.status, transaction.expression);
		}

		// Add addresses
		for addr in addresses {
			builder = builder.address(&addr.address);
		}

		builder.build()
	}

	fn create_mock_monitor_match() -> MonitorMatch {
		MonitorMatch::EVM(Box::new(EVMMonitorMatch {
			monitor: create_test_monitor(vec![], vec![], vec![], vec![]),
			transaction: TransactionBuilder::new().build(),
			receipt: Some(EVMTransactionReceipt::default()),
			logs: Some(vec![]),
			network_slug: "evm_mainnet".to_string(),
			matched_on: MatchConditions {
				functions: vec![],
				events: vec![],
				transactions: vec![],
			},
			matched_on_args: None,
		}))
	}

	#[tokio::test]
	async fn test_slack_notification_invalid_config() {
		let service = NotificationService::new();

		let trigger = TriggerBuilder::new()
			.name("test_slack")
			.script("invalid", ScriptLanguage::Python)
			.trigger_type(TriggerType::Slack) // Intentionally wrong config type
			.build();

		let variables = HashMap::new();
		let result = service
			.execute(
				&trigger,
				&variables,
				&create_mock_monitor_match(),
				&HashMap::new(),
			)
			.await;
		assert!(result.is_err());
		match result {
			Err(NotificationError::ConfigError(ctx)) => {
				assert!(ctx
					.message
					.contains("Expected retry policy in trigger config"));
			}
			_ => panic!("Expected ConfigError"),
		}
	}

	#[tokio::test]
	async fn test_email_notification_invalid_config() {
		let service = NotificationService::new();

		let trigger = TriggerBuilder::new()
			.name("test_email")
			.script("invalid", ScriptLanguage::Python)
			.trigger_type(TriggerType::Email) // Intentionally wrong config type
			.build();

		let variables = HashMap::new();
		let result = service
			.execute(
				&trigger,
				&variables,
				&create_mock_monitor_match(),
				&HashMap::new(),
			)
			.await;
		assert!(result.is_err());
		match result {
			Err(NotificationError::ConfigError(ctx)) => {
				assert!(ctx.message.contains("Invalid email configuration"));
			}
			_ => panic!("Expected ConfigError"),
		}
	}

	#[tokio::test]
	async fn test_webhook_notification_invalid_config() {
		let service = NotificationService::new();

		let trigger = TriggerBuilder::new()
			.name("test_webhook")
			.script("invalid", ScriptLanguage::Python)
			.trigger_type(TriggerType::Webhook) // Intentionally wrong config type
			.build();

		let variables = HashMap::new();
		let result = service
			.execute(
				&trigger,
				&variables,
				&create_mock_monitor_match(),
				&HashMap::new(),
			)
			.await;
		assert!(result.is_err());
		match result {
			Err(NotificationError::ConfigError(ctx)) => {
				assert!(ctx
					.message
					.contains("Expected retry policy in trigger config"));
			}
			_ => panic!("Expected ConfigError"),
		}
	}

	#[tokio::test]
	async fn test_discord_notification_invalid_config() {
		let service = NotificationService::new();

		let trigger = TriggerBuilder::new()
			.name("test_discord")
			.script("invalid", ScriptLanguage::Python)
			.trigger_type(TriggerType::Discord) // Intentionally wrong config type
			.build();

		let variables = HashMap::new();
		let result = service
			.execute(
				&trigger,
				&variables,
				&create_mock_monitor_match(),
				&HashMap::new(),
			)
			.await;
		assert!(result.is_err());
		match result {
			Err(NotificationError::ConfigError(ctx)) => {
				assert!(ctx
					.message
					.contains("Expected retry policy in trigger config"));
			}
			_ => panic!("Expected ConfigError"),
		}
	}

	#[tokio::test]
	async fn test_telegram_notification_invalid_config() {
		let service = NotificationService::new();

		let trigger = TriggerBuilder::new()
			.name("test_telegram")
			.script("invalid", ScriptLanguage::Python)
			.trigger_type(TriggerType::Telegram) // Intentionally wrong config type
			.build();

		let variables = HashMap::new();
		let result = service
			.execute(
				&trigger,
				&variables,
				&create_mock_monitor_match(),
				&HashMap::new(),
			)
			.await;
		assert!(result.is_err());
		match result {
			Err(NotificationError::ConfigError(ctx)) => {
				assert!(ctx
					.message
					.contains("Expected retry policy in trigger config"));
			}
			_ => panic!("Expected ConfigError"),
		}
	}

	#[tokio::test]
	async fn test_script_notification_invalid_config() {
		let service = NotificationService::new();

		let trigger = TriggerBuilder::new()
			.name("test_script")
			.telegram("invalid", "invalid", false)
			.trigger_type(TriggerType::Script) // Intentionally wrong config type
			.build();

		let variables = HashMap::new();

		let result = service
			.execute(
				&trigger,
				&variables,
				&create_mock_monitor_match(),
				&HashMap::new(),
			)
			.await;

		assert!(result.is_err());
		match result {
			Err(NotificationError::ConfigError(ctx)) => {
				assert!(ctx.message.contains("Invalid script configuration"));
			}
			_ => panic!("Expected ConfigError"),
		}
	}
}
