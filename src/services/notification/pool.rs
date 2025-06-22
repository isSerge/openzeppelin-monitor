use crate::utils::client_storage::ClientStorage;
use lettre::SmtpTransport;
use reqwest::Client as ReqwestClient;
use std::sync::Arc;
use std::time::Duration;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum NotificationPoolError {
	#[error("Failed to create HTTP client: {0}")]
	HttpClientBuildError(String),

	#[error("Failed to create SMTP client: {0}")]
	SmtpClientBuildError(String),
}

/// Notification client pool that manages HTTP and SMTP clients for sending notifications.
///
/// Provides a thread-safe way to access and create HTTP and SMTP clients
/// for sending notifications. It uses a `ClientStorage` to hold the clients,
/// allowing for efficient reuse and management of HTTP and SMTP connections.
pub struct NotificationClientPool {
	http_clients: ClientStorage<ReqwestClient>,
	smtp_clients: ClientStorage<SmtpTransport>,
}

impl NotificationClientPool {
	pub fn new() -> Self {
		Self {
			http_clients: ClientStorage::new(),
			smtp_clients: ClientStorage::new(),
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
	pub async fn get_or_create_http_client(
		&self,
	) -> Result<Arc<ReqwestClient>, NotificationPoolError> {
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
			.map_err(|e| NotificationPoolError::HttpClientBuildError(e.to_string()))?;

		let arc_client = Arc::new(client);
		clients.insert(DEFAULT_HTTP_CLIENT_KEY.to_string(), arc_client.clone());
		Ok(arc_client)
	}

	pub async fn get_or_create_smtp_client(
		&self,
	) -> Result<Arc<SmtpTransport>, NotificationPoolError> {
		unimplemented!("SMTP client creation is not implemented yet");
	}

	/// Get the number of active HTTP clients in the pool,
	/// only used for testing purposes since the pool currently
	/// has single default key for HTTP clients.
	#[cfg(test)]
	pub async fn get_active_http_client_count(&self) -> usize {
		self.http_clients.clients.read().await.len()
	}

	/// Method to get or create SMTP client
	#[cfg(test)]
	pub async fn get_active_smtp_client_count(&self) -> usize {
		self.smtp_clients.clients.read().await.len()
	}
}

impl Default for NotificationClientPool {
	fn default() -> Self {
		Self::new()
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	fn create_pool() -> NotificationClientPool {
		NotificationClientPool::new()
	}

	#[tokio::test]
	async fn test_pool_init_empty() {
		let pool = create_pool();
		let http_count = pool.get_active_http_client_count().await;
		let smtp_count = pool.get_active_smtp_client_count().await;

		assert_eq!(http_count, 0, "Pool should be empty initially");
		assert_eq!(smtp_count, 0, "Pool should be empty initially");
	}

	#[tokio::test]
	async fn test_pool_get_or_create_http_client() {
		let pool = create_pool();
		let client = pool.get_or_create_http_client().await;

		assert!(
			client.is_ok(),
			"Should successfully create or get HTTP client"
		);

		assert_eq!(
			pool.get_active_http_client_count().await,
			1,
			"Pool should have one active HTTP client"
		);
	}

	#[tokio::test]
	async fn test_pool_returns_same_client() {
		let pool = create_pool();
		let client1 = pool.get_or_create_http_client().await.unwrap();
		let client2 = pool.get_or_create_http_client().await.unwrap();

		assert!(
			Arc::ptr_eq(&client1, &client2),
			"Should return the same client instance"
		);
		assert_eq!(
			pool.get_active_http_client_count().await,
			1,
			"Pool should still have one active HTTP client"
		);
	}

	#[tokio::test]
	async fn test_pool_concurrent_access() {
		let pool = Arc::new(create_pool());

		let num_tasks = 10;
		let mut tasks = Vec::new();

		for _ in 0..num_tasks {
			let pool_clone = Arc::clone(&pool);
			tasks.push(tokio::spawn(async move {
				let client = pool_clone.get_or_create_http_client().await;
				assert!(
					client.is_ok(),
					"Should successfully create or get HTTP client"
				);
			}));
		}

		let results = futures::future::join_all(tasks).await;

		for result in results {
			assert!(result.is_ok(), "All tasks should complete successfully");
		}
	}

	#[tokio::test]
	async fn test_pool_default() {
		let pool = NotificationClientPool::default();

		assert_eq!(
			pool.get_active_http_client_count().await,
			0,
			"Default pool should be empty initially"
		);

		assert_eq!(
			pool.get_active_smtp_client_count().await,
			0,
			"Default pool should be empty initially"
		);

		let client = pool.get_or_create_http_client().await;

		assert!(
			client.is_ok(),
			"Default pool should successfully create or get HTTP client"
		);

		assert_eq!(
			pool.get_active_http_client_count().await,
			1,
			"Default pool should have one active HTTP client"
		);
	}
}
