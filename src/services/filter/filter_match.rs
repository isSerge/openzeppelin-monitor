//! Match handling and processing logic.
//!
//! This module implements the processing of matched transactions and events:
//! - Converts blockchain data to trigger-friendly format
//! - Prepares notification payloads by converting blockchain-specific data into a generic format
//! - Handles match execution through configured triggers
//! - Manages the transformation of complex blockchain data into template variables

use std::collections::HashMap;

use alloy::primitives::Address;

use crate::{
	models::{MonitorMatch, ScriptLanguage},
	services::{
		filter::{
			evm_helpers::{b256_to_string, h160_to_string},
			FilterError,
		},
		trigger::TriggerExecutionServiceTrait,
	},
};

/// Process a monitor match by executing associated triggers.
///
/// Takes a matched monitor event and processes it through the appropriate trigger service.
/// Converts blockchain-specific data into a standardized format that can be used in trigger
/// templates.
///
/// # Arguments
/// * `matching_monitor` - The matched monitor event containing transaction and trigger information
/// * `trigger_service` - Service responsible for executing triggers
/// * `trigger_scripts` - Scripts to be executed for each trigger
///
/// # Returns
/// Result indicating success or failure of trigger execution
///
/// # Example Template Variables
/// The function converts blockchain data into template variables like:
/// ```text
/// "monitor_name": "Transfer USDT Token"
/// "transaction_hash": "0x99139c8f64b9b939678e261e1553660b502d9fd01c2ab1516e699ee6c8cc5791"
/// "transaction_from": "0xf401346fd255e034a2e43151efe1d68c1e0f8ca5"
/// "transaction_to": "0x0000000000001ff3684f28c67538d4d072c22734"
/// "transaction_value": "24504000000000000"
/// "event_0_signature": "Transfer(address,address,uint256)"
/// "event_0_to": "0x70bf6634ee8cb27d04478f184b9b8bb13e5f4710"
/// "event_0_from": "0x2e8135be71230c6b1b4045696d41c09db0414226"
/// "event_0_value": "88248701"
/// ```
pub async fn handle_match<T: TriggerExecutionServiceTrait>(
	matching_monitor: MonitorMatch,
	trigger_service: &T,
	trigger_scripts: &HashMap<String, (ScriptLanguage, String)>,
) -> Result<(), FilterError> {
	match &matching_monitor {
		MonitorMatch::EVM(evm_monitor_match) => {
			let transaction = evm_monitor_match.transaction.clone();
			// If sender does not exist, we replace with 0x0000000000000000000000000000000000000000
			let sender = transaction.sender().unwrap_or(&Address::ZERO);
			// Convert transaction data to a HashMap
			let mut data = HashMap::new();
			data.insert(
				"transaction_hash".to_string(),
				b256_to_string(*transaction.hash()),
			);
			data.insert("transaction_from".to_string(), h160_to_string(*sender));
			data.insert(
				"transaction_value".to_string(),
				transaction.value().to_string(),
			);
			if let Some(to) = transaction.to() {
				data.insert("transaction_to".to_string(), h160_to_string(*to));
			}
			data.insert(
				"monitor_name".to_string(),
				evm_monitor_match.monitor.name.clone(),
			);

			let matched_on: HashMap<String, String> = {
				let matched_on = &evm_monitor_match.matched_on;
				let mut map = HashMap::new();
				for (idx, func) in matched_on.functions.iter().enumerate() {
					map.insert(
						format!("function_{}_signature", idx),
						func.signature.clone(),
					);
				}
				for (idx, event) in matched_on.events.iter().enumerate() {
					map.insert(format!("event_{}_signature", idx), event.signature.clone());
				}
				map
			};

			data.extend(matched_on);

			let matched_args: HashMap<String, String> =
				if let Some(args) = &evm_monitor_match.matched_on_args {
					let mut map = HashMap::new();
					if let Some(functions) = &args.functions {
						for (idx, func) in functions.iter().enumerate() {
							if let Some(func_args) = &func.args {
								for arg in func_args {
									map.insert(
										format!("function_{}_{}", idx, arg.name),
										arg.value.clone(),
									);
								}
							}
						}
					}
					if let Some(events) = &args.events {
						for (idx, event) in events.iter().enumerate() {
							if let Some(event_args) = &event.args {
								for arg in event_args {
									map.insert(
										format!("event_{}_{}", idx, arg.name),
										arg.value.clone(),
									);
								}
							}
						}
					}
					map
				} else {
					HashMap::new()
				};

			data.extend(matched_args);

			// Swallow any errors since it's logged in the trigger service and we want to continue
			// processing other matches
			let _ = trigger_service
				.execute(
					&evm_monitor_match
						.monitor
						.triggers
						.iter()
						.map(|s| s.to_string())
						.collect::<Vec<_>>(),
					data,
					&matching_monitor,
					trigger_scripts,
				)
				.await;
		}
		MonitorMatch::Stellar(stellar_monitor_match) => {
			let transaction = stellar_monitor_match.transaction.clone();
			// Convert transaction data to a HashMap
			let mut data = HashMap::new();
			data.insert(
				"transaction_hash".to_string(),
				transaction.hash().to_string(),
			);

			// TODO: Add sender and value to the data so it can be used in the body template of the
			// trigger data.insert(
			//     "transaction_from".to_string(),
			//     transaction.sender().to_string(),
			// );
			// data.insert(
			//     "transaction_value".to_string(),
			//     transaction.value().to_string(),
			// );
			// if let Some(to) = transaction.to() {
			//     data.insert("transaction_to".to_string(), to.to_string());
			// }
			data.insert(
				"monitor_name".to_string(),
				stellar_monitor_match.monitor.name.clone(),
			);

			let matched_on: HashMap<String, String> = {
				let matched_on = &stellar_monitor_match.matched_on;
				let mut map = HashMap::new();
				for (idx, func) in matched_on.functions.iter().enumerate() {
					map.insert(
						format!("function_{}_signature", idx),
						func.signature.clone(),
					);
				}
				for (idx, event) in matched_on.events.iter().enumerate() {
					map.insert(format!("event_{}_signature", idx), event.signature.clone());
				}
				map
			};

			data.extend(matched_on);

			let matched_args: HashMap<String, String> =
				if let Some(args) = &stellar_monitor_match.matched_on_args {
					let mut map = HashMap::new();
					if let Some(functions) = &args.functions {
						for (idx, func) in functions.iter().enumerate() {
							if let Some(func_args) = &func.args {
								for arg in func_args {
									map.insert(
										format!("function_{}_{}", idx, arg.name),
										arg.value.clone(),
									);
								}
							}
						}
					}
					if let Some(events) = &args.events {
						for (idx, event) in events.iter().enumerate() {
							if let Some(event_args) = &event.args {
								for arg in event_args {
									map.insert(
										format!("event_{}_{}", idx, arg.name),
										arg.value.clone(),
									);
								}
							}
						}
					}
					map
				} else {
					HashMap::new()
				};

			data.extend(matched_args);

			// Swallow any errors since it's logged in the trigger service and we want to continue
			// processing other matches
			let _ = trigger_service
				.execute(
					&stellar_monitor_match
						.monitor
						.triggers
						.iter()
						.map(|s| s.to_string())
						.collect::<Vec<_>>(),
					data,
					&matching_monitor,
					trigger_scripts,
				)
				.await;
		}
	}
	Ok(())
}

#[cfg(test)]
mod tests {
	use super::*;
	use crate::{
		models::{
			EVMMatchArguments, EVMMatchParamEntry, EVMMatchParamsMap, EVMMonitorMatch,
			FunctionCondition, MatchConditions, Monitor,
		},
		services::{
			filter::evm_test_helpers::{TestReceiptBuilder, TestTransactionBuilder},
			trigger::{TriggerError, TriggerExecutionServiceTrait},
		},
		utils::tests::evm::monitor::MonitorBuilder,
	};
	use std::collections::HashMap;
	use std::sync::{Arc, Mutex};

	// Mock trigger service for testing
	struct MockTriggerService {
		captured_data: std::sync::Arc<std::sync::Mutex<HashMap<String, String>>>,
	}

	#[async_trait::async_trait]
	impl TriggerExecutionServiceTrait for MockTriggerService {
		async fn execute(
			&self,
			_triggers: &[String],
			data: HashMap<String, String>,
			_matching_monitor: &MonitorMatch,
			_trigger_scripts: &HashMap<String, (ScriptLanguage, String)>,
		) -> Result<(), TriggerError> {
			let mut captured = self.captured_data.lock().unwrap();
			// Simulate capturing the data that would be used for templating
			*captured = data;
			Ok(())
		}

		async fn load_scripts(
			&self,
			_monitors: &[Monitor],
		) -> Result<HashMap<String, (ScriptLanguage, String)>, TriggerError> {
			// Return empty scripts for testing
			Ok(HashMap::new())
		}
	}

	/// Create a mock EVM monitor match for testing.
	fn create_mock_evm_monitor_match(monitor: Monitor) -> EVMMonitorMatch {
		let transaction = TestTransactionBuilder::new().build();
		let receipt = TestReceiptBuilder::new().build();
		EVMMonitorMatch {
			monitor,
			transaction,
			receipt,
			network_slug: "ethereum_mainnet".to_string(),
			// Initialize empty, will be filled in by the test
			matched_on: MatchConditions {
				functions: vec![],
				events: vec![],
				transactions: vec![],
			},
			// Initialize empty, will be filled in by the test
			matched_on_args: None,
		}
	}

	#[tokio::test]
	async fn test_key_collision_between_function_signature_and_arg_signature() {
		// Define elements for the test
		let function_signature_string = "dangerousFunc(bytes32 signature)".to_string();
		let argument_name_colliding = "signature".to_string();
		let argument_value_string = "0xarg_value_colliding_with_signature".to_string();
		let expected_colliding_key = "function_0_signature".to_string();

		// Create monitor with function that has "signature" parameter
		let mut monitor = MonitorBuilder::new().build();
		monitor.match_conditions.functions.push(FunctionCondition {
			signature: "dangerousFunc(bytes32 signature)".to_string(),
			expression: None,
		});

		// Create match with argument named "signature"
		let mut evm_match = create_mock_evm_monitor_match(monitor);

		// Set the `matched_on` conditions. This generates the *first* value for the key
		evm_match.matched_on = MatchConditions {
			functions: vec![FunctionCondition {
				signature: function_signature_string.clone(), // This value should be overwritten
				expression: None,
			}],
			events: vec![],
			transactions: vec![],
		};

		// Set the `matched_on_args` conditions. This generates the *second* value for the key
		evm_match.matched_on_args = Some(EVMMatchArguments {
			functions: Some(vec![EVMMatchParamsMap {
				signature: function_signature_string.clone(),
				args: Some(vec![EVMMatchParamEntry {
					name: argument_name_colliding.clone(), // The argument causing the collision
					value: argument_value_string.clone(), // The value that will overwrite
					kind: "bytes32".to_string(),
					indexed: false,
				}]),
				hex_signature: Some("0xdeadbeef".to_string()),
			}]),
			events: None,
		});

		let match_wrapper = MonitorMatch::EVM(Box::new(evm_match));

		// // Create mock execution service
		let captured_data = Arc::new(Mutex::new(HashMap::new()));
		let mock_service = MockTriggerService {
			captured_data: captured_data.clone(),
		};

		// Process the match
		handle_match(match_wrapper, &mock_service, &HashMap::new())
			.await
			.unwrap();

		let data = captured_data.lock().unwrap();

		// Verify key exists
		assert!(
			data.contains_key(&expected_colliding_key),
			"The key '{}' should exist in the final data map.",
			expected_colliding_key
		);

		// Verify value is the argument value, not the function signature
		let actual_value = data.get(&expected_colliding_key).unwrap();

		assert_eq!(
			actual_value,
			&argument_value_string,
			"Collision Confirmed! The value for key '{}' should be the argument value '{}', but it seems to be overwritten. Expected original value was '{}'.",
			expected_colliding_key,
			argument_value_string,
			function_signature_string
		);

		// Verify other expected keys are present (sanity check)
		assert!(data.contains_key("transaction_hash"));
		assert!(data.contains_key("monitor_name"));
	}
}
