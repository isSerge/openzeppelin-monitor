use crate::models::{
	EVMBaseReceipt, EVMBaseTransaction, EVMReceiptLog, EVMTransaction, EVMTransactionReceipt,
};
use alloy::{
	primitives::{Address, Bytes, LogData, B256, U256, U64},
	rpc::types::Index,
};
use std::str::FromStr;

/// A builder for creating test EVM transactions with default values.
#[derive(Debug, Default)]
pub struct TestTransactionBuilder {
	hash: Option<B256>,
	from: Option<Address>,
	to: Option<Address>,
	value: Option<U256>,
	input: Option<Bytes>,
	gas_price: Option<U256>,
	max_fee_per_gas: Option<U256>,
	max_priority_fee_per_gas: Option<U256>,
	gas_limit: Option<U256>,
	nonce: Option<U256>,
}

impl TestTransactionBuilder {
	/// Creates a new TestTransactionBuilder instance.
	pub fn new() -> Self {
		Self::default()
	}

	/// Sets the hash of the transaction.
	pub fn hash(mut self, hash: B256) -> Self {
		self.hash = Some(hash);
		self
	}

	/// Sets the sender address of the transaction.
	pub fn from(mut self, from: Address) -> Self {
		self.from = Some(from);
		self
	}

	/// Sets the recipient address of the transaction.
	pub fn to(mut self, to: Address) -> Self {
		self.to = Some(to);
		self
	}

	/// Sets the transaction value (amount sent).
	pub fn value(mut self, value: U256) -> Self {
		self.value = Some(value);
		self
	}

	/// Sets the transaction input data.
	pub fn input(mut self, input: Bytes) -> Self {
		self.input = Some(input);
		self
	}

	/// Sets the gas price for legacy transactions.
	pub fn gas_price(mut self, gas_price: U256) -> Self {
		self.gas_price = Some(gas_price);
		self
	}

	/// Sets the max fee per gas for EIP-1559 transactions.
	pub fn max_fee_per_gas(mut self, max_fee_per_gas: U256) -> Self {
		self.max_fee_per_gas = Some(max_fee_per_gas);
		self
	}

	/// Sets the max priority fee per gas for EIP-1559 transactions.
	pub fn max_priority_fee_per_gas(mut self, max_priority_fee_per_gas: U256) -> Self {
		self.max_priority_fee_per_gas = Some(max_priority_fee_per_gas);
		self
	}

	/// Sets the gas limit for the transaction.
	pub fn gas_limit(mut self, gas_limit: U256) -> Self {
		self.gas_limit = Some(gas_limit);
		self
	}

	/// Sets the nonce for the transaction.
	pub fn nonce(mut self, nonce: U256) -> Self {
		self.nonce = Some(nonce);
		self
	}

	/// Builds the Transaction instance.
	pub fn build(self) -> EVMTransaction {
		let default_gas_limit = U256::from(21000);

		let base_tx = EVMBaseTransaction {
			hash: self.hash.unwrap_or_default(),
			from: self.from,
			to: self.to,
			gas_price: self.gas_price,
			max_fee_per_gas: self.max_fee_per_gas,
			max_priority_fee_per_gas: self.max_priority_fee_per_gas,
			gas: self.gas_limit.unwrap_or(default_gas_limit),
			nonce: self.nonce.unwrap_or_default(),
			value: self.value.unwrap_or_default(),
			input: self.input.unwrap_or_default(),
			..Default::default()
		};

		EVMTransaction(base_tx)
	}
}

/// A builder for creating test EVM transaction receipts with default values.
#[derive(Debug, Default)]
pub struct TestReceiptBuilder {
	transaction_hash: Option<B256>,
	status: Option<bool>,
	gas_used: Option<U256>,
	logs: Option<Vec<EVMReceiptLog>>,
	from: Option<Address>,
	to: Option<Address>,
	contract_address: Option<Address>,
	transaction_index: Option<Index>,
}

impl TestReceiptBuilder {
	/// Creates a new TestReceiptBuilder instance.
	pub fn new() -> Self {
		Self::default()
	}

	/// Sets the transaction hash of the receipt.
	pub fn transaction_hash(mut self, transaction_hash: B256) -> Self {
		self.transaction_hash = Some(transaction_hash);
		self
	}

	/// Sets the status of the transaction. Default is success.
	pub fn status(mut self, status: bool) -> Self {
		self.status = Some(status);
		self
	}

	/// Sets the gas used for the transaction.
	pub fn gas_used(mut self, gas_used: U256) -> Self {
		self.gas_used = Some(gas_used);
		self
	}

	/// Sets the transaction index in the block.
	pub fn transaction_index(mut self, transaction_index: usize) -> Self {
		self.transaction_index = Some(Index::from(transaction_index));
		self
	}

	/// Sets the logs associated with the transaction.
	pub fn logs(mut self, logs: Vec<EVMReceiptLog>) -> Self {
		self.logs = Some(logs);
		self
	}

	/// Sets the sender address of the transaction.
	pub fn from(mut self, from: Address) -> Self {
		self.from = Some(from);
		self
	}

	/// Sets the recipient address of the transaction
	pub fn to(mut self, to: Address) -> Self {
		self.to = Some(to);
		self
	}

	/// Sets the contract address for contract creation transactions
	pub fn contract_address(mut self, contract_address: Address) -> Self {
		self.contract_address = Some(contract_address);
		self
	}

	/// Set log with specified value transfer event
	pub fn value(mut self, value: U256) -> Self {
		let event_signature = "0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef";
		let contract_address = self.contract_address.unwrap_or_default();
		let from_address = self.from.unwrap_or_default();
		let to_address = self.to.unwrap_or_default();
		let value_hex = format!("{:064x}", value);

		let alloy_log = alloy::primitives::Log {
			address: contract_address,
			data: LogData::new_unchecked(
				vec![
					B256::from_str(event_signature).unwrap(),
					B256::from_slice(&[&[0u8; 12], from_address.as_slice()].concat()),
					B256::from_slice(&[&[0u8; 12], to_address.as_slice()].concat()),
				],
				Bytes(hex::decode(value_hex).unwrap().into()),
			),
		};

		let base_log = EVMReceiptLog::from(alloy_log);
		self.logs = Some(vec![base_log]);
		self
	}

	/// Builds the TransactionReceipt instance.
	pub fn build(self) -> EVMTransactionReceipt {
		let status_success = self.status.unwrap_or(true);
		let status_u64 = if status_success {
			U64::from(1)
		} else {
			U64::from(0)
		};

		let base = EVMBaseReceipt {
			transaction_hash: self.transaction_hash.unwrap_or_default(),
			status: Some(status_u64),
			gas_used: self.gas_used,
			logs: self.logs.unwrap_or_default(),
			from: self.from.unwrap_or_default(),
			to: self.to,
			contract_address: self.contract_address,
			transaction_index: self.transaction_index.unwrap_or_default(),
			..Default::default()
		};

		EVMTransactionReceipt::from(base)
	}
}
