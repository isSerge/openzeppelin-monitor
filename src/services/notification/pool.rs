use crate::utils::client_storage::ClientStorage;
use reqwest::Client as ReqwestClient;
use std::sync::Arc;
use std::time::Duration;

/// Notification client pool that manages HTTP clients for sending notifications.
///
/// Provides a thread-safe way to access and create HTTP clients
/// for sending notifications. It uses a `ClientStorage` to hold the clients,
/// allowing for efficient reuse and management of HTTP connections.
pub struct NotificationClientPool {
	http_clients: ClientStorage<ReqwestClient>,
  // TODO: add SMTP clients
}

impl NotificationClientPool {
	pub fn new() -> Self {
		Self {
			http_clients: ClientStorage::new(),
      // TODO: add SMTP clients
		}
	}

	/// Method to get or create HTTP client
	///
	/// Uses a double-checked locking pattern:
	/// 1. Fast path with read lock to check for existing client
	/// 2. Slow path with write lock to create new client if needed
	///
	/// This ensures thread-safety while maintaining good performance
	/// for the common case of accessing existing clients.
	pub async fn get_or_create_http_client(&self) -> Result<Arc<ReqwestClient>, anyhow::Error> {
		const DEFAULT_HTTP_CLIENT_KEY: &str = "default_notification_http_client";

		// Fast path: Read lock
		if let Some(client) = self
			.http_clients
			.clients
			.read()
			.await
			.get(DEFAULT_HTTP_CLIENT_KEY)
		{
			return Ok(client.clone());
		}

		// Slow path: Write lock
		let mut clients = self.http_clients.clients.write().await;
		// Double-check: Another thread might have created it
		if let Some(client) = clients.get(DEFAULT_HTTP_CLIENT_KEY) {
			return Ok(client.clone());
		}

		// Create the new client
		let client = ReqwestClient::builder()
			.pool_max_idle_per_host(10)
			.pool_idle_timeout(Some(Duration::from_secs(90)))
			.connect_timeout(Duration::from_secs(10))
			.build()
			.map_err(anyhow::Error::from)?;

		let arc_client = Arc::new(client);
		clients.insert(DEFAULT_HTTP_CLIENT_KEY.to_string(), arc_client.clone());
		Ok(arc_client)
	}

  /// Get the number of active HTTP clients in the pool,
  /// only used for testing purposes since the pool currently
  /// has single default key for HTTP clients.
  #[cfg(test)]
	pub async fn get_active_http_client_count(&self) -> usize {
		self.http_clients.clients.read().await.len()
	}
}

impl Default for NotificationClientPool {
	fn default() -> Self {
		Self::new()
	}
}
