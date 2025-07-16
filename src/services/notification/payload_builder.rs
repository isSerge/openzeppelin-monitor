//! Webhook payload builder implementation.
//!
//! This module provides functionality to build webhook payloads for different notification services (Telegram, Slack, Discord, etc.).

use serde_json::json;

/// Trait for building webhook payloads.
pub trait WebhookPayloadBuilder: Send + Sync {
	/// Builds a webhook payload.
	///
	/// # Arguments
	///
	/// * `title` - The title of the message.
	/// * `message` - The message content.
	///
	/// # Returns
	///
	/// A `serde_json::Value` representing the payload.
	fn build_payload(&self, title: &str, message: &str) -> serde_json::Value;
}

/// A payload builder for Slack.
pub struct SlackPayloadBuilder;

impl WebhookPayloadBuilder for SlackPayloadBuilder {
	fn build_payload(&self, title: &str, message: &str) -> serde_json::Value {
		let full_message = format!("*{}*\n\n{}", title, message);
		json!({
			"blocks": [
				{
					"type": "section",
					"text": {
						"type": "mrkdwn",
						"text": full_message
					}
				}
			]
		})
	}
}

/// A payload builder for Discord.
pub struct DiscordPayloadBuilder;

impl WebhookPayloadBuilder for DiscordPayloadBuilder {
	fn build_payload(&self, title: &str, message: &str) -> serde_json::Value {
		let full_message = format!("*{}*\n\n{}", title, message);
		json!({
			"content": full_message
		})
	}
}

/// A payload builder for Telegram.
pub struct TelegramPayloadBuilder {
	pub chat_id: String,
	pub disable_web_preview: bool,
}

impl WebhookPayloadBuilder for TelegramPayloadBuilder {
	fn build_payload(&self, title: &str, message: &str) -> serde_json::Value {
		let full_message = format!("*{}* \n\n{}", title, message);
		json!({
			"chat_id": self.chat_id,
			"text": full_message,
			"parse_mode": "MarkdownV2",
			"disable_web_page_preview": self.disable_web_preview
		})
	}
}

/// A payload builder for generic webhooks.
pub struct GenericWebhookPayloadBuilder;

impl WebhookPayloadBuilder for GenericWebhookPayloadBuilder {
	fn build_payload(&self, title: &str, message: &str) -> serde_json::Value {
		json!({
			"title": title,
			"body": message
		})
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use serde_json::json;

	#[test]
	fn test_slack_payload_builder() {
		let builder = SlackPayloadBuilder;
		let title = "Test Title";
		let message = "Test Message";
		let payload = builder.build_payload(title, message);
		assert_eq!(
			payload,
			json!({
				"blocks": [
					{
						"type": "section",
						"text": {
							"type": "mrkdwn",
							"text": "*Test Title*\n\nTest Message"
						}
					}
				]
			})
		);
	}

	#[test]
	fn test_discord_payload_builder() {
		let builder = DiscordPayloadBuilder;
		let title = "Test Title";
		let message = "Test Message";
		let payload = builder.build_payload(title, message);
		assert_eq!(
			payload,
			json!({
				"content": "*Test Title*\n\nTest Message"
			})
		);
	}

	#[test]
	fn test_telegram_payload_builder() {
		let builder = TelegramPayloadBuilder {
			chat_id: "12345".to_string(),
			disable_web_preview: true,
		};
		let title = "Test Title";
		let message = "Test Message";
		let payload = builder.build_payload(title, message);
		assert_eq!(
			payload,
			json!({
				"chat_id": "12345",
				"text": "*Test Title* \n\nTest Message",
				"parse_mode": "MarkdownV2",
				"disable_web_page_preview": true
			})
		);
	}

	#[test]
	fn test_generic_webhook_payload_builder() {
		let builder = GenericWebhookPayloadBuilder;
		let title = "Test Title";
		let message = "Test Message";
		let payload = builder.build_payload(title, message);
		assert_eq!(
			payload,
			json!({
				"title": "Test Title",
				"body": "Test Message"
			})
		);
	}
}
