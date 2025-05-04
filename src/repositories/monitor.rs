//! Monitor configuration repository implementation.
//!
//! This module provides storage and retrieval of monitor configurations, including
//! validation of references to networks and triggers. The repository loads monitor
//! configurations from JSON files and ensures all referenced components exist.

#![allow(clippy::result_large_err)]

use std::{collections::HashMap, marker::PhantomData, path::Path, sync::Arc};

use crate::{
	models::{ConfigLoader, Monitor, Network, ScriptLanguage, Trigger},
	repositories::{
		error::RepositoryError,
		network::{NetworkRepository, NetworkRepositoryTrait, NetworkService},
		trigger::{TriggerRepository, TriggerRepositoryTrait, TriggerService},
	},
};

/// Static mapping of script languages to their file extensions
const LANGUAGE_EXTENSIONS: &[(&ScriptLanguage, &str)] = &[
	(&ScriptLanguage::Python, "py"),
	(&ScriptLanguage::JavaScript, "js"),
	(&ScriptLanguage::Bash, "sh"),
];

/// Repository for storing and retrieving monitor configurations
#[derive(Clone)]
pub struct MonitorRepository<N: NetworkRepositoryTrait, T: TriggerRepositoryTrait> {
	/// Map of monitor names to their configurations
	pub monitors: HashMap<String, Arc<Monitor>>,
	_network_repository: PhantomData<N>,
	_trigger_repository: PhantomData<T>,
}

impl<N: NetworkRepositoryTrait, T: TriggerRepositoryTrait> MonitorRepository<N, T> {
	/// Create a new monitor repository from the given path
	///
	/// Loads all monitor configurations from JSON files in the specified directory
	/// (or default config directory if None is provided).
	pub fn new(
		path: Option<&Path>,
		network_service: Option<NetworkService<N>>,
		trigger_service: Option<TriggerService<T>>,
	) -> Result<Self, RepositoryError> {
		let monitors = Self::load_all(path, network_service, trigger_service)?;
		Ok(MonitorRepository {
			monitors,
			_network_repository: PhantomData,
			_trigger_repository: PhantomData,
		})
	}

	/// Create a new monitor repository from a list of monitors
	pub fn new_with_monitors(monitors: HashMap<String, Monitor>) -> Self {
		// Convert the input map's values to Arc<Monitor>
		let arc_monitors: HashMap<String, Arc<Monitor>> = monitors
			.into_iter()
			.map(|(key, monitor)| (key, Arc::new(monitor)))
			.collect();

		MonitorRepository {
			monitors: arc_monitors,
			_network_repository: PhantomData,
			_trigger_repository: PhantomData,
		}
	}

	/// Returns an error if any monitor references a non-existent network or trigger.
	pub fn validate_monitor_references(
		monitors: &HashMap<String, Monitor>,
		triggers: &HashMap<String, Trigger>,
		networks: &HashMap<String, Network>,
	) -> Result<(), RepositoryError> {
		let mut validation_errors = Vec::new();
		let mut metadata = HashMap::new();

		for (monitor_name, monitor) in monitors {
			// Validate trigger references
			for trigger_id in &monitor.triggers {
				if !triggers.contains_key(trigger_id) {
					validation_errors.push(format!(
						"Monitor '{}' references non-existent trigger '{}'",
						monitor_name, trigger_id
					));
					metadata.insert(
						format!("monitor_{}_invalid_trigger", monitor_name),
						trigger_id.clone(),
					);
				}
			}

			// Validate network references
			for network_slug in &monitor.networks {
				if !networks.contains_key(network_slug) {
					validation_errors.push(format!(
						"Monitor '{}' references non-existent network '{}'",
						monitor_name, network_slug
					));
					metadata.insert(
						format!("monitor_{}_invalid_network", monitor_name),
						network_slug.clone(),
					);
				}
			}

			// Validate custom trigger conditions
			for condition in &monitor.trigger_conditions {
				let script_path = Path::new(&condition.script_path);
				if !script_path.exists() {
					validation_errors.push(format!(
						"Monitor '{}' has a custom filter script that does not exist: {}",
						monitor_name, condition.script_path
					));
				}

				// Validate file extension matches the specified language
				let expected_extension = match LANGUAGE_EXTENSIONS
					.iter()
					.find(|(lang, _)| *lang == &condition.language)
					.map(|(_, ext)| *ext)
				{
					Some(ext) => ext,
					None => {
						validation_errors.push(format!(
							"Monitor '{}' uses unsupported script language {:?}",
							monitor_name, condition.language
						));
						continue;
					}
				};

				match script_path.extension().and_then(|ext| ext.to_str()) {
					Some(ext) if ext == expected_extension => (), // Valid extension
					_ => validation_errors.push(format!(
						"Monitor '{}' has a custom filter script with invalid extension - must be \
						 .{} for {:?} language: {}",
						monitor_name, expected_extension, condition.language, condition.script_path
					)),
				}

				if condition.timeout_ms == 0 {
					validation_errors.push(format!(
						"Monitor '{}' should have a custom filter timeout_ms greater than 0",
						monitor_name
					));
				}
			}
		}

		if !validation_errors.is_empty() {
			return Err(RepositoryError::validation_error(
				format!(
					"Configuration validation failed:\n{}",
					validation_errors.join("\n"),
				),
				None,
				Some(metadata),
			));
		}

		Ok(())
	}
}

/// Interface for monitor repository implementations
///
/// This trait defines the standard operations that any monitor repository must support,
/// allowing for different storage backends while maintaining a consistent interface.
pub trait MonitorRepositoryTrait<N: NetworkRepositoryTrait, T: TriggerRepositoryTrait>:
	Clone
{
	/// Create a new monitor repository from the given path
	fn new(
		path: Option<&Path>,
		network_service: Option<NetworkService<N>>,
		trigger_service: Option<TriggerService<T>>,
	) -> Result<Self, RepositoryError>
	where
		Self: Sized;

	/// Load all monitor configurations from the given path
	///
	/// If no path is provided, uses the default config directory.
	/// Also validates references to networks and triggers.
	/// This is a static method that doesn't require an instance.
	fn load_all(
		path: Option<&Path>,
		network_service: Option<NetworkService<N>>,
		trigger_service: Option<TriggerService<T>>,
	) -> Result<HashMap<String, Arc<Monitor>>, RepositoryError>;

	/// Load a monitor from a specific path
	///
	/// Loads a monitor configuration from a specific path and validates all network and trigger references.
	fn load_from_path(
		&self,
		path: Option<&Path>,
		network_service: Option<NetworkService<N>>,
		trigger_service: Option<TriggerService<T>>,
	) -> Result<Arc<Monitor>, RepositoryError>;

	/// Get a specific monitor by ID
	///
	/// Returns None if the monitor doesn't exist.
	fn get(&self, monitor_id: &str) -> Option<Arc<Monitor>>;

	/// Get all monitors
	///
	/// Returns a copy of the monitor map to prevent external mutation.
	fn get_all(&self) -> HashMap<String, Arc<Monitor>>;
}

impl<N: NetworkRepositoryTrait, T: TriggerRepositoryTrait> MonitorRepositoryTrait<N, T>
	for MonitorRepository<N, T>
{
	fn new(
		path: Option<&Path>,
		network_service: Option<NetworkService<N>>,
		trigger_service: Option<TriggerService<T>>,
	) -> Result<Self, RepositoryError> {
		MonitorRepository::new(path, network_service, trigger_service)
	}

	fn load_all(
		path: Option<&Path>,
		network_service: Option<NetworkService<N>>,
		trigger_service: Option<TriggerService<T>>,
	) -> Result<HashMap<String, Arc<Monitor>>, RepositoryError> {
		let owned_monitors = Monitor::load_all(path).map_err(|e| {
			RepositoryError::load_error(
				"Failed to load monitors",
				Some(Box::new(e)),
				Some(HashMap::from([(
					"path".to_string(),
					path.map_or_else(|| "default".to_string(), |p| p.display().to_string()),
				)])),
			)
		})?;

		let networks = match network_service {
			Some(service) => service.get_all(),
			None => {
				NetworkRepository::new(None)
					.map_err(|e| {
						RepositoryError::load_error(
							"Failed to load networks for monitor validation",
							Some(Box::new(e)),
							None,
						)
					})?
					.networks
			}
		};

		let triggers = match trigger_service {
			Some(service) => service.get_all(),
			None => {
				TriggerRepository::new(None)
					.map_err(|e| {
						RepositoryError::load_error(
							"Failed to load triggers for monitor validation",
							Some(Box::new(e)),
							None,
						)
					})?
					.triggers
			}
		};

		Self::validate_monitor_references(&owned_monitors, &triggers, &networks)?;

		// Convert to HashMap<String, Arc<Monitor>> *after* validation
		let arc_monitors: HashMap<String, Arc<Monitor>> = owned_monitors
			.into_iter()
			.map(|(name, monitor)| (name, Arc::new(monitor)))
			.collect();

		Ok(arc_monitors)
	}

	/// Load a monitor from a specific path
	///
	/// Loads a monitor configuration from a specific path and validates all network and trigger references.
	fn load_from_path(
		&self,
		path: Option<&Path>,
		network_service: Option<NetworkService<N>>,
		trigger_service: Option<TriggerService<T>>,
	) -> Result<Arc<Monitor>, RepositoryError> {
		match path {
			Some(path) => {
				let owned_monitor = Monitor::load_from_path(path).map_err(|e| {
					RepositoryError::load_error(
						"Failed to load monitors",
						Some(Box::new(e)),
						Some(HashMap::from([(
							"path".to_string(),
							path.display().to_string(),
						)])),
					)
				})?;

				let networks = match network_service {
					Some(service) => service.get_all(),
					None => NetworkRepository::new(None)?.networks,
				};

				let triggers = match trigger_service {
					Some(service) => service.get_all(),
					None => TriggerRepository::new(None)?.triggers,
				};
				let monitors_for_validation =
					HashMap::from([(owned_monitor.name.clone(), owned_monitor.clone())]);
				Self::validate_monitor_references(&monitors_for_validation, &triggers, &networks)?;
				// match monitors.values().next() {
				// 	Some(monitor) => Ok(monitor.clone()),
				// 	None => Err(RepositoryError::load_error("No monitors found", None, None)),
				// }

				Ok(Arc::new(owned_monitor))
			}
			None => Err(RepositoryError::load_error(
				"Failed to load monitors",
				None,
				None,
			)),
		}
	}

	fn get(&self, monitor_id: &str) -> Option<Arc<Monitor>> {
		self.monitors.get(monitor_id).map(Arc::clone)
	}

	fn get_all(&self) -> HashMap<String, Arc<Monitor>> {
		self.monitors.clone()
	}
}

/// Service layer for monitor repository operations
///
/// This type provides a higher-level interface for working with monitor configurations,
/// handling repository initialization and access through a trait-based interface.
/// It also ensures that all monitor references to networks and triggers are valid.
#[derive(Clone)]
pub struct MonitorService<
	M: MonitorRepositoryTrait<N, T>,
	N: NetworkRepositoryTrait,
	T: TriggerRepositoryTrait,
> {
	repository: M,
	_network_repository: PhantomData<N>,
	_trigger_repository: PhantomData<T>,
}

// Generic implementation for any repository type
impl<M: MonitorRepositoryTrait<N, T>, N: NetworkRepositoryTrait, T: TriggerRepositoryTrait>
	MonitorService<M, N, T>
{
	/// Create a new monitor service with the default repository implementation
	///
	/// Loads monitor configurations from the specified path (or default config directory)
	/// and validates all network and trigger references.
	pub fn new(
		path: Option<&Path>,
		network_service: Option<NetworkService<N>>,
		trigger_service: Option<TriggerService<T>>,
	) -> Result<MonitorService<M, N, T>, RepositoryError> {
		let repository = M::new(path, network_service, trigger_service)?;
		Ok(MonitorService {
			repository,
			_network_repository: PhantomData,
			_trigger_repository: PhantomData,
		})
	}

	/// Create a new monitor service with a specific configuration path
	///
	/// Similar to `new()` but makes the path parameter more explicit.
	pub fn new_with_path(path: Option<&Path>) -> Result<MonitorService<M, N, T>, RepositoryError> {
		let repository = M::new(path, None, None)?;
		Ok(MonitorService {
			repository,
			_network_repository: PhantomData,
			_trigger_repository: PhantomData,
		})
	}

	/// Create a new monitor service with a custom repository implementation
	///
	/// Allows for using alternative storage backends that implement the MonitorRepositoryTrait.
	pub fn new_with_repository(repository: M) -> Result<Self, RepositoryError> {
		Ok(MonitorService {
			repository,
			_network_repository: PhantomData,
			_trigger_repository: PhantomData,
		})
	}

	/// Get a specific monitor by ID
	///
	/// Returns None if the monitor doesn't exist.
	pub fn get(&self, monitor_id: &str) -> Option<Arc<Monitor>> {
		self.repository.get(monitor_id)
	}

	/// Get all monitors
	///
	/// Returns a copy of the monitor map to prevent external mutation.
	pub fn get_all(&self) -> HashMap<String, Arc<Monitor>> {
		self.repository.get_all()
	}

	/// Load a monitor from a specific path
	///
	/// Loads a monitor configuration from a specific path and validates all network and trigger references.
	pub fn load_from_path(
		&self,
		path: Option<&Path>,
		network_service: Option<NetworkService<N>>,
		trigger_service: Option<TriggerService<T>>,
	) -> Result<Arc<Monitor>, RepositoryError> {
		self.repository
			.load_from_path(path, network_service, trigger_service)
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use crate::models::{MatchConditions, Monitor, ScriptLanguage};
	use std::fs;
	use tempfile::TempDir;

	#[test]
	fn test_validate_custom_trigger_conditions() {
		let temp_dir = TempDir::new().unwrap();
		let script_path = temp_dir.path().join("test_script.py");
		fs::write(&script_path, "print('test')").unwrap();

		let mut monitors = HashMap::new();
		let triggers = HashMap::new();
		let networks = HashMap::new();

		// Test valid configuration
		let monitor = Monitor {
			name: "test_monitor".to_string(),
			match_conditions: MatchConditions::default(),
			trigger_conditions: vec![crate::models::TriggerConditions {
				script_path: script_path.to_str().unwrap().to_string(),
				language: ScriptLanguage::Python,
				timeout_ms: 1000,
				arguments: None,
			}],
			..Default::default()
		};
		monitors.insert("test_monitor".to_string(), monitor);

		assert!(
			MonitorRepository::<NetworkRepository, TriggerRepository>::validate_monitor_references(
				&monitors, &triggers, &networks
			)
			.is_ok()
		);

		// Test non-existent script
		let monitor_bad_path = Monitor {
			name: "test_monitor_bad_path".to_string(),
			trigger_conditions: vec![crate::models::TriggerConditions {
				script_path: "non_existent_script.py".to_string(),
				language: ScriptLanguage::Python,
				timeout_ms: 1000,
				arguments: None,
			}],
			..Default::default()
		};
		monitors.insert("test_monitor_bad_path".to_string(), monitor_bad_path);

		let err =
			MonitorRepository::<NetworkRepository, TriggerRepository>::validate_monitor_references(
				&monitors, &triggers, &networks,
			)
			.unwrap_err();
		assert!(err.to_string().contains("does not exist"));

		// Test wrong extension
		let wrong_ext_path = temp_dir.path().join("test_script.js");
		fs::write(&wrong_ext_path, "print('test')").unwrap();

		let monitor_wrong_ext = Monitor {
			name: "test_monitor_wrong_ext".to_string(),
			trigger_conditions: vec![crate::models::TriggerConditions {
				script_path: wrong_ext_path.to_str().unwrap().to_string(),
				language: ScriptLanguage::Python,
				timeout_ms: 1000,
				arguments: None,
			}],
			..Default::default()
		};
		monitors.clear();
		monitors.insert("test_monitor_wrong_ext".to_string(), monitor_wrong_ext);

		let err =
			MonitorRepository::<NetworkRepository, TriggerRepository>::validate_monitor_references(
				&monitors, &triggers, &networks,
			)
			.unwrap_err();
		assert!(err.to_string().contains(
			"Monitor 'test_monitor_wrong_ext' has a custom filter script with invalid extension - \
			 must be .py for Python language"
		));

		// Test zero timeout
		let monitor_zero_timeout = Monitor {
			name: "test_monitor_zero_timeout".to_string(),
			match_conditions: MatchConditions::default(),
			trigger_conditions: vec![crate::models::TriggerConditions {
				script_path: script_path.to_str().unwrap().to_string(),
				language: ScriptLanguage::Python,
				timeout_ms: 0,
				arguments: None,
			}],
			..Default::default()
		};
		monitors.clear();
		monitors.insert(
			"test_monitor_zero_timeout".to_string(),
			monitor_zero_timeout,
		);

		let err =
			MonitorRepository::<NetworkRepository, TriggerRepository>::validate_monitor_references(
				&monitors, &triggers, &networks,
			)
			.unwrap_err();
		assert!(err.to_string().contains("timeout_ms greater than 0"));
	}

	#[test]
	fn test_load_error_messages() {
		// Test with invalid path to trigger load error
		let invalid_path = Path::new("/non/existent/path");
		let result = MonitorRepository::<NetworkRepository, TriggerRepository>::load_all(
			Some(invalid_path),
			None,
			None,
		);

		assert!(result.is_err());
		let err = result.unwrap_err();
		match err {
			RepositoryError::LoadError(message) => {
				assert!(message.to_string().contains("Failed to load monitors"));
			}
			_ => panic!("Expected RepositoryError::LoadError"),
		}
	}

	#[test]
	fn test_network_validation_error() {
		// Create a monitor with a reference to a non-existent network
		let mut monitors = HashMap::new();
		let monitor = Monitor {
			name: "test_monitor".to_string(),
			networks: vec!["non_existent_network".to_string()],
			..Default::default()
		};
		monitors.insert("test_monitor".to_string(), monitor);

		// Empty networks and triggers
		let networks = HashMap::new();
		let triggers = HashMap::new();

		// Validate should fail due to non-existent network reference
		let result =
			MonitorRepository::<NetworkRepository, TriggerRepository>::validate_monitor_references(
				&monitors, &triggers, &networks,
			);

		assert!(result.is_err());
		let err = result.unwrap_err();
		assert!(err.to_string().contains("references non-existent network"));
	}

	#[test]
	fn test_trigger_validation_error() {
		// Create a monitor with a reference to a non-existent trigger
		let mut monitors = HashMap::new();
		let monitor = Monitor {
			name: "test_monitor".to_string(),
			triggers: vec!["non_existent_trigger".to_string()],
			..Default::default()
		};
		monitors.insert("test_monitor".to_string(), monitor);

		// Empty networks and triggers
		let networks = HashMap::new();
		let triggers = HashMap::new();

		// Validate should fail due to non-existent trigger reference
		let result =
			MonitorRepository::<NetworkRepository, TriggerRepository>::validate_monitor_references(
				&monitors, &triggers, &networks,
			);

		assert!(result.is_err());
		let err = result.unwrap_err();
		assert!(err.to_string().contains("references non-existent trigger"));
	}

	#[test]
	fn test_load_from_path_error_handling() {
		// Create a temporary directory for testing
		let temp_dir = TempDir::new().unwrap();
		let invalid_path = temp_dir.path().join("non_existent_monitor.json");

		// Create a repository instance
		let repository =
			MonitorRepository::<NetworkRepository, TriggerRepository>::new_with_monitors(
				HashMap::new(),
			);

		// Attempt to load from non-existent path
		let result = repository.load_from_path(Some(&invalid_path), None, None);

		// Verify error handling
		assert!(result.is_err());
		let err = result.unwrap_err();
		match err {
			RepositoryError::LoadError(message) => {
				assert!(message.to_string().contains("Failed to load monitors"));
				// Verify the error contains the path in its metadata
				assert!(message
					.to_string()
					.contains(&invalid_path.display().to_string()));
			}
			_ => panic!("Expected RepositoryError::LoadError"),
		}
	}
}
