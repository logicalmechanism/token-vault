#!/usr/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat testnet.magic)

# script
lock_path="../locking-contract/locking-contract.plutus"
stake_path="../stake-contract/stake-contract.plutus"
script_address=$(${cli} address build --payment-script-file ${lock_path} --stake-script-file ${stake_path} --testnet-magic ${testnet_magic})

#
staker_address=$(cat wallets/seller-wallet/payment.addr)
reference_address=$(cat wallets/reference-wallet/payment.addr)
collat_address=$(cat wallets/collat-wallet/payment.addr)

#
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file tmp/protocol.json
${cli} query tip --testnet-magic ${testnet_magic} | jq

#
echo
echo -e "\033[1;35m Script Address:" 
echo -e "\n${script_address}\n";
${cli} query utxo --address ${script_address} --testnet-magic ${testnet_magic}
echo -e "\033[0m"

#
echo
echo -e "\033[1;36m Staker Address:" 
echo -e "\n${staker_address}\n";
${cli} query utxo --address ${staker_address} --testnet-magic ${testnet_magic}
echo -e "\033[0m"
#
echo
echo -e "\033[1;34m Reference Address:" 
echo -e "\n \033[1;34m ${reference_address}\n";
${cli} query utxo --address ${reference_address} --testnet-magic ${testnet_magic}
echo -e "\033[0m"

#
echo
echo -e "\033[1;33m Collateral Address:" 
echo -e "\n${collat_address}\n";
${cli} query utxo --address ${collat_address} --testnet-magic ${testnet_magic}
echo -e "\033[0m"

