//! Block filtering implementations.
//!
//! Provides trait definition and implementations for filtering blocks
//! across different blockchain types. Includes:
//! - Generic BlockFilter trait
//! - EVM-specific implementation
//! - Stellar-specific implementation

pub mod evm {
	pub mod filter;
	pub mod helpers;
}
pub mod stellar {
	pub mod filter;
	pub mod helpers;
}

use async_trait::async_trait;
use std::sync::Arc;

use crate::{
	models::{BlockType, Monitor, MonitorMatch, Network},
	services::{blockchain::BlockFilterFactory, filter::error::FilterError},
};
pub use evm::filter::EVMBlockFilter;
pub use stellar::filter::StellarBlockFilter;

/// Trait for filtering blockchain data
///
/// This trait must be implemented by all blockchain-specific clients to provide
/// a way to filter blockchain data.
#[async_trait]
pub trait BlockFilter {
	type Client;
	async fn filter_block(
		&self,
		client: &Self::Client,
		network: &Network,
		block: &BlockType,
		monitors: &[Arc<Monitor>],
	) -> Result<Vec<MonitorMatch>, FilterError>;
}

/// Service for filtering blockchain data
///
/// This service provides a way to filter blockchain data based on a set of monitors.
pub struct FilterService {}

impl FilterService {
	pub fn new() -> Self {
		FilterService {}
	}
}

impl Default for FilterService {
	fn default() -> Self {
		Self::new()
	}
}

impl FilterService {
	pub async fn filter_block<T: BlockFilterFactory<T>>(
		&self,
		client: &T,
		network: &Network,
		block: &BlockType,
		monitors: &[Arc<Monitor>],
	) -> Result<Vec<MonitorMatch>, FilterError> {
		let filter = T::filter();
		filter.filter_block(client, network, block, monitors).await
	}
}
