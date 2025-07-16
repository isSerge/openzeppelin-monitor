//! Webhook payload builder implementation.
//!
//! This module provides functionality to build webhook payloads for different notification services (Telegram, Slack, Discord, etc.).

use regex::Regex;
use serde_json::json;
use std::collections::HashMap;

/// Trait for building webhook payloads.
pub trait WebhookPayloadBuilder: Send + Sync {
	/// Builds a webhook payload by formatting the template and applying channel-specific rules.
	///
	/// # Arguments
	///
	/// * `title` - The raw title of the message.
	/// * `body_template` - The message body template with variables like `${...}`.
	/// * `variables` - The map of variables to substitute into the template.
	///
	/// # Returns
	///
	/// A `serde_json::Value` representing the payload.
	fn build_payload(
		&self,
		title: &str,
		body_template: &str,
		variables: &HashMap<String, String>,
	) -> serde_json::Value;
}

/// Formats a message by substituting variables in the template.
pub fn format_template(template: &str, variables: &HashMap<String, String>) -> String {
	let mut message = template.to_string();
	for (key, value) in variables {
		message = message.replace(&format!("${{{}}}", key), value);
	}
	message
}

/// A payload builder for Slack.
pub struct SlackPayloadBuilder;

impl WebhookPayloadBuilder for SlackPayloadBuilder {
	fn build_payload(
		&self,
		title: &str,
		body_template: &str,
		variables: &HashMap<String, String>,
	) -> serde_json::Value {
		let message = format_template(body_template, variables);
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
	fn build_payload(
		&self,
		title: &str,
		body_template: &str,
		variables: &HashMap<String, String>,
	) -> serde_json::Value {
		let message = format_template(body_template, variables);
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

impl TelegramPayloadBuilder {
	/// Escape a full MarkdownV2 message, preserving entities and
	/// escaping *all* special chars inside link URLs too.
	fn escape_markdown_v2(text: &str) -> String {
		const SPECIAL: &[char] = &[
			'_', '*', '[', ']', '(', ')', '~', '`', '>', '#', '+', '-', '=', '|', '{', '}', '.',
			'!', '\\',
		];

		let re =
			Regex::new(r"(?s)```.*?```|`[^`]*`|\*[^*]*\*|_[^_]*_|~[^~]*~|\[([^\]]+)\]\(([^)]+)\)")
				.unwrap();

		let mut out = String::with_capacity(text.len());
		let mut last = 0;

		for caps in re.captures_iter(text) {
			let mat = caps.get(0).unwrap();

			for c in text[last..mat.start()].chars() {
				if SPECIAL.contains(&c) {
					out.push('\\');
				}
				out.push(c);
			}

			if let (Some(lbl), Some(url)) = (caps.get(1), caps.get(2)) {
				let mut esc_label = String::with_capacity(lbl.as_str().len() * 2);
				for c in lbl.as_str().chars() {
					if SPECIAL.contains(&c) {
						esc_label.push('\\');
					}
					esc_label.push(c);
				}
				let mut esc_url = String::with_capacity(url.as_str().len() * 2);
				for c in url.as_str().chars() {
					if SPECIAL.contains(&c) {
						esc_url.push('\\');
					}
					esc_url.push(c);
				}
				out.push('[');
				out.push_str(&esc_label);
				out.push(']');
				out.push('(');
				out.push_str(&esc_url);
				out.push(')');
			} else {
				out.push_str(mat.as_str());
			}

			last = mat.end();
		}

		for c in text[last..].chars() {
			if SPECIAL.contains(&c) {
				out.push('\\');
			}
			out.push(c);
		}

		out
	}
}

impl WebhookPayloadBuilder for TelegramPayloadBuilder {
	fn build_payload(
		&self,
		title: &str,
		body_template: &str,
		variables: &HashMap<String, String>,
	) -> serde_json::Value {
		// First, substitute variables.
		let message = format_template(body_template, variables);

		// Then, escape both the title and the formatted message for Telegram MarkdownV2.
		let escaped_title = Self::escape_markdown_v2(title);
		let escaped_message = Self::escape_markdown_v2(&message);

		let full_message = format!("*{}* \n\n{}", escaped_title, escaped_message);
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
	fn build_payload(
		&self,
		title: &str,
		body_template: &str,
		variables: &HashMap<String, String>,
	) -> serde_json::Value {
		let message = format_template(body_template, variables);
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
		let title = "Test Title";
		let message = "Test Message";
		let variables = HashMap::from([("value".to_string(), "42".to_string())]);
		let payload = SlackPayloadBuilder.build_payload(title, message, &variables);
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
		let title = "Test Title";
		let message = "Test Message";
		let variables = HashMap::from([("value".to_string(), "42".to_string())]);
		let payload = DiscordPayloadBuilder.build_payload(title, message, &variables);
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
		let variables = HashMap::from([("value".to_string(), "42".to_string())]);
		let payload = builder.build_payload(title, message, &variables);
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
		let title = "Test Title";
		let message = "Test Message";
		let variables = HashMap::from([("value".to_string(), "42".to_string())]);
		let payload = GenericWebhookPayloadBuilder.build_payload(title, message, &variables);
		assert_eq!(
			payload,
			json!({
				"title": "Test Title",
				"body": "Test Message"
			})
		);
	}
}
