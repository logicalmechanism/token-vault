#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat testnet.magic)

# get params
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file tmp/protocol.json

# collat info
collat_address=$(cat wallets/collat-wallet/payment.addr)
collat_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/collat-wallet/payment.vkey)

# payee of the delegation
reward_address=$(cat wallets/seller-wallet/payment.addr)

# stake address
stake_address=$(cat ../stake-contract/stake.addr)

# find rewards
rewardBalance=$(${cli} query stake-address-info \
    --testnet-magic ${testnet_magic} \
    --address ${stake_address} | jq -r ".[0].rewardAccountBalance")

# exit if no rewards
if [ "${rewardBalance}" -eq "0" ]; then
   echo -e "\n \033[0;31m No Rewards Found At ${stake_address} \033[0m \n";
   exit;
fi

withdrawalString="${stake_address}+${rewardBalance}"
echo "Withdraw OUTPUT" $withdrawalString
#
# exit
#
echo -e "\033[0;36m Gathering Collateral UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${collat_address} \
    --out-file tmp/collat_utxo.json

TXNS=$(jq length tmp/collat_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${collat_address} \033[0m \n";
   exit;
fi
collat_utxo=$(jq -r 'keys[0]' tmp/collat_utxo.json)

# get reward info
echo -e "\033[0;36m Gathering Reward UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${reward_address} \
    --out-file tmp/reward_utxo.json

TXNS=$(jq length tmp/reward_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${reward_address} \033[0m \n";
   exit;
fi
alltxin=""
# select utxos with only ada
TXIN=$(jq -r --arg alltxin "" 'to_entries[] | select(.value.value | length < 2) | .key | . + $alltxin + " --tx-in"' tmp/reward_utxo.json)
reward_tx_in=${TXIN::-8}

# get the script reference tx
script_ref_utxo=$(${cli} transaction txid --tx-file tmp/tx-reference-utxo.signed)

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${reward_address} \
    --tx-in-collateral="${collat_utxo}" \
    --tx-in ${reward_tx_in} \
    --withdrawal ${withdrawalString} \
    --withdrawal-tx-in-reference="${script_ref_utxo}#2" \
    --withdrawal-plutus-script-v2 \
    --withdrawal-reference-tx-in-redeemer-file data/redeemer/withdraw-redeemer.json \
    --tx-out="${reward_address}+${rewardBalance}" \
    --required-signer-hash ${collat_pkh} \
    --testnet-magic ${testnet_magic})

IFS=':' read -ra VALUE <<< "${FEE}"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee: \033[0m" $FEE
#
# exit
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file wallets/seller-wallet/payment.skey \
    --signing-key-file wallets/collat-wallet/payment.skey \
    --tx-body-file tmp/tx.draft \
    --out-file tmp/tx.signed \
    --testnet-magic ${testnet_magic}
#    
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file tmp/tx.signed
