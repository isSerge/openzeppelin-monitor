//! Manages the rotation of blockchain RPC endpoints
//!
//! Provides methods for rotating between multiple URLs and sending requests to the active endpoint
//! with automatic fallback to other URLs on failure.
use reqwest_middleware::ClientWithMiddleware;
use serde::Serialize;
use serde_json::Value;
use std::sync::Arc;
use tokio::sync::RwLock;

use crate::services::blockchain::transports::{
	RotatingTransport, TransportError, ROTATE_ON_ERROR_CODES,
};

/// Manages the rotation of blockchain RPC endpoints
///
/// Provides methods for rotating between multiple URLs and sending requests to the active endpoint
/// with automatic fallback to other URLs on failure.
///
/// # Fields
/// * `active_url` - The current active URL
/// * `fallback_urls` - A list of fallback URLs to rotate to
/// * `client` - The client to use for the endpoint manager
/// * `rotation_lock` - A lock for managing the rotation process
#[derive(Clone, Debug)]
pub struct EndpointManager {
	pub active_url: Arc<RwLock<String>>,
	pub fallback_urls: Arc<RwLock<Vec<String>>>,
	client: ClientWithMiddleware,
	rotation_lock: Arc<tokio::sync::Mutex<()>>,
}

/// Represents the outcome of a `EndpointManager::attempt_request_on_url` method call
/// Used within the `EndpointManager::send_raw_request` method to handle different paths of request execution
/// and response handling.
#[derive(Debug)]
enum SingleRequestAttemptOutcome {
	/// Successfully got a response (status might still be error)
	Success(reqwest::Response),
	/// Error during send (e.g., connection, timeout)
	NetworkError(reqwest_middleware::Error),
	/// Error serializing the request body
	SerializationError(TransportError),
}

impl EndpointManager {
	/// Creates a new rotating URL client
	///
	/// # Arguments
	/// * `client` - The client to use for the endpoint manager
	/// * `active_url` - The initial active URL
	/// * `fallback_urls` - A list of fallback URLs to rotate to
	///
	/// # Returns
	pub fn new(client: ClientWithMiddleware, active_url: &str, fallback_urls: Vec<String>) -> Self {
		Self {
			active_url: Arc::new(RwLock::new(active_url.to_string())),
			fallback_urls: Arc::new(RwLock::new(fallback_urls)),
			rotation_lock: Arc::new(tokio::sync::Mutex::new(())),
			client,
		}
	}

	/// Updates the client with a new client
	///
	/// Useful for updating the client with a new retry policy or strategy
	///
	/// # Arguments
	/// * `client` - The new client to use for the endpoint manager
	pub fn update_client(&mut self, client: ClientWithMiddleware) {
		self.client = client;
	}

	/// Rotates to the next available URL
	///
	/// # Arguments
	/// * `transport` - The transport client implementing the RotatingTransport trait
	///
	/// # Returns
	/// * `Result<String, TransportError>` - The result of the rotation attempt, containing the new active URL or an error
	pub async fn try_rotate_url<T: RotatingTransport>(
		&self,
		transport: &T,
	) -> Result<String, TransportError> {
		// Acquire the rotation lock to prevent concurrent rotations
		let _guard = self.rotation_lock.lock().await;
		let initial_active = self.active_url.read().await.clone();
		let initial_fallbacks = self.fallback_urls.read().await.clone();

		tracing::debug!(
			"Trying to rotate URL: Current Active: '{}', Fallbacks: {:?}",
			initial_active,
			initial_fallbacks
		);

		let current_active_url_snapshot = self.active_url.read().await.clone();

		// --- Select a new URL ---
		let new_url = {
			let mut fallback_urls_guard = self.fallback_urls.write().await;
			// Read active URL again to ensure we use the absolute latest if another thread somehow modified it
			let current_active_for_selection = self.active_url.read().await.clone();

			tracing::debug!(
					"URL rotation: Selecting candidate. Active for selection: '{}', Current Fallbacks: {:?}",
					current_active_for_selection,
					*fallback_urls_guard
			);

			if fallback_urls_guard.is_empty() {
				let msg = format!(
					"No fallback URLs available (list empty). Current active: {}",
					current_active_for_selection
				);
				return Err(TransportError::url_rotation(msg, None, None));
			}

			match fallback_urls_guard
				.iter()
				.position(|url| *url != current_active_for_selection)
			{
				Some(idx) => {
					let candidate = fallback_urls_guard.remove(idx);
					tracing::debug!(
						"try_rotate_url: Selected candidate '{}'. Fallbacks after removal: {:?}",
						candidate,
						*fallback_urls_guard
					);
					candidate
				}
				None => {
					let msg = format!(
						"No suitable (distinct) fallback URLs available. Current active: '{}', Fallbacks checked: {:?}",
						current_active_for_selection, *fallback_urls_guard
					);
					return Err(TransportError::url_rotation(msg, None, None));
				}
			}
		};

		// --- Attempt to connect and update the transport client ---
		tracing::debug!(
			"Attempting try_connect to new_url during rotation: '{}'",
			new_url
		);

		if let Err(connect_err) = transport.try_connect(&new_url).await {
			{
				let mut fallback_urls_guard = self.fallback_urls.write().await;
				// If the connection fails, we add the new URL to fallbacks
				fallback_urls_guard.push(new_url.clone());
			}

			return Err(TransportError::url_rotation(
				format!("Failed to connect to new URL '{}'", new_url),
				Some(connect_err.into()),
				None,
			));
		}

		tracing::debug!(
			"Attempting update_client with new_url during rotation: '{}'",
			new_url
		);

		if let Err(update_err) = transport.update_client(&new_url).await {
			{
				let mut fallback_urls_guard = self.fallback_urls.write().await;
				// If the update fails, we add the new URL to fallbacks
				fallback_urls_guard.push(new_url.clone());
			}

			return Err(TransportError::url_rotation(
				format!(
					"Failed to update transport client with new URL '{}'",
					new_url
				),
				Some(update_err.into()),
				None,
			));
		}

		// --- Successfully rotated URL ---
		{
			let mut active_url_guard = self.active_url.write().await;
			let mut fallback_urls_guard = self.fallback_urls.write().await;
			tracing::debug!(
				"Successful URL rotation - from: {}, to: {}",
				current_active_url_snapshot,
				new_url
			);
			fallback_urls_guard.push(current_active_url_snapshot);
			*active_url_guard = new_url.clone();
		}
		Ok(new_url)
	}

	/// Attempts to send a request to the specified URL
	/// # Arguments
	/// * `url` - The URL to send the request to
	/// * `transport` - The transport client implementing the RotatingTransport trait
	/// * `method` - The HTTP method to use for the request (e.g., "POST")
	/// * `params` - Optional parameters for the request, serialized to JSON
	///
	/// # Returns
	/// * `SingleRequestAttemptOutcome` - The outcome of the request attempt
	async fn try_request_on_url<P>(
		&self,
		url: &str,
		transport: &impl RotatingTransport,
		method: &str,
		params: Option<P>,
	) -> SingleRequestAttemptOutcome
	where
		P: Into<Value> + Send + Clone + Serialize,
	{
		// Create the request body using the transport's customization method
		let request_body = transport.customize_request(method, params).await;

		// Serialize the request body to JSON
		let request_body_str = match serde_json::to_string(&request_body) {
			Ok(body) => body,
			Err(e) => {
				tracing::error!("Failed to serialize request body: {}", e);
				return SingleRequestAttemptOutcome::SerializationError(
					TransportError::request_serialization(
						"Failed to serialize request JSON",
						Some(Box::new(e)),
						None,
					),
				);
			}
		};

		// Send the request to the specified URL
		let response_result = self
			.client
			.post(url)
			.header("Content-Type", "application/json")
			.body(request_body_str)
			.send()
			.await;

		// Handle the response
		match response_result {
			Ok(response) => SingleRequestAttemptOutcome::Success(response),
			Err(network_error) => {
				tracing::warn!("Network error while sending request: {}", network_error);
				SingleRequestAttemptOutcome::NetworkError(network_error)
			}
		}
	}

	/// Sends a raw request to the blockchain RPC endpoint with automatic URL rotation on failure
	///
	/// # Arguments
	/// * `transport` - The transport client implementing the RotatingTransport trait
	/// * `method` - The RPC method name to call
	/// * `params` - The parameters for the RPC method call as a JSON Value
	///
	/// # Returns
	/// * `Result<Value, TransportError>` - The JSON response from the RPC endpoint or an error
	///
	/// # Behavior
	/// - Automatically rotates to fallback URLs if the request fails with specific status codes
	///   (e.g., 429)
	/// - Retries the request with the new URL after rotation
	/// - Returns the first successful response or an error if all attempts fail
	pub async fn send_raw_request<
		T: RotatingTransport,
		P: Into<Value> + Send + Clone + Serialize,
	>(
		&self,
		transport: &T,
		method: &str,
		params: Option<P>,
	) -> Result<Value, TransportError> {
		loop {
			let current_url_snapshot = self.active_url.read().await.clone();

			tracing::debug!(
				"Attempting request on active URL: '{}'",
				current_url_snapshot
			);

			// Attempt to send the request to the current active URL
			let attempt_result = self
				.try_request_on_url(&current_url_snapshot, transport, method, params.clone())
				.await;

			match attempt_result {
				// Handle successful response
				SingleRequestAttemptOutcome::Success(response) => {
					let status = response.status();
					if status.is_success() {
						// Successful response, parse JSON
						return response.json().await.map_err(|e| {
							TransportError::response_parse(
								"Failed to parse JSON response".to_string(),
								Some(Box::new(e)),
								None,
							)
						});
					} else {
						// HTTP error
						let error_body = response.text().await.unwrap_or_default();
						tracing::warn!(
							"Request to {} failed with status {}: {}",
							current_url_snapshot,
							status,
							error_body
						);

						// Check if we should rotate based on status code
						if ROTATE_ON_ERROR_CODES.contains(&status.as_u16()) {
							tracing::debug!(
								"send_raw_request: HTTP status {} on '{}' triggers URL rotation attempt",
								status, current_url_snapshot
							);

							match self.try_rotate_url(transport).await {
								Ok(_new_url) => {
									continue; // Retry on the new active URL
								}
								Err(rotation_error) => {
									// Return the original HTTP error with rotation error context
									return Err(TransportError::http(
										status,
										current_url_snapshot.clone(),
										error_body,
										Some(Box::new(rotation_error)),
										None,
									));
								}
							}
						} else {
							// HTTP error that doesn't trigger rotation
							tracing::warn!(
								"HTTP error status {} on {} does not trigger rotation. Failing.",
								status,
								current_url_snapshot
							);
							return Err(TransportError::http(
								status,
								current_url_snapshot,
								error_body,
								None,
								None,
							));
						}
					}
				}
				// Handle network error, try rotation
				SingleRequestAttemptOutcome::NetworkError(network_error) => {
					tracing::warn!(
						"Network error for {}: {}",
						current_url_snapshot,
						network_error,
					);

					// Always attempt rotation on network errors
					match self.try_rotate_url(transport).await {
						Ok(new_url) => {
							tracing::debug!("Rotation successful after network error, retrying request on new URL: '{}'", new_url);
							continue; // Retry on the new active URL
						}
						Err(rotation_error) => {
							// Return network error with rotation error context
							return Err(TransportError::network(
								network_error.to_string(),
								Some(Box::new(rotation_error)),
								None,
							));
						}
					}
				}
				// Non-retryable serialization error
				SingleRequestAttemptOutcome::SerializationError(serialization_error) => {
					return Err(serialization_error);
				}
			}
		}
	}
}
