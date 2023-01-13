#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat testnet.magic)

# get params
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file tmp/protocol.json

# script
lock_path="../locking-contract/locking-contract.plutus"
stake_path="../stake-contract/stake-contract.plutus"
script_address=$(${cli} address build --payment-script-file ${lock_path} --stake-script-file ${stake_path} --testnet-magic ${testnet_magic})

# the staker
staker_address=$(cat wallets/seller-wallet/payment.addr)
staker_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/seller-wallet/payment.vkey)

# adjust the time lock here
startTime=$(echo `expr $(echo $(date +%s%3N)) + $(echo 0)`)
if [[ $# -eq 0 ]] ; then
    echo -e "\n \033[0;31m Please Supply A Time For Locking In Minutes \033[0m \n";
    exit
fi
# time difference in milliseconds
timeDiff=$((1000*60*${1}))
if [[ ${timeDiff} -eq 0 ]] ; then
    echo -e "\n \033[0;31m Locking Time Must Be Longer Than One Minute \033[0m \n";
    exit
fi
endTime=$((${startTime} + ${timeDiff}))

# update the starting lock time
variable=${startTime}; jq --argjson variable "$variable" '.fields[2].int=$variable' data/datum/stake-datum.json > data/datum/stake-datum-new.json
mv data/datum/stake-datum-new.json data/datum/stake-datum.json

# update the ending lock time
variable=${endTime}; jq --argjson variable "$variable" '.fields[3].int=$variable' data/datum/stake-datum.json > data/datum/stake-datum-new.json
mv data/datum/stake-datum-new.json data/datum/stake-datum.json

# token information
#
policy_id="df8e2ddd300b54a606611a8f20be1aef06dc38d560e00b73e57de5a1"
token_name="0104a79b1ff28d52381de95b82feb2e56bec6d9533eb005b8d711353d815d4a9"
amount=1

# asset to lock
asset="${amount} ${policy_id}.${token_name}"


# calculate the min ada
min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-inline-datum-file data/datum/stake-datum.json \
    --tx-out="${script_address} + 5000000 + ${asset}" | tr -dc '0-9')

script_address_out="${script_address} + ${min_utxo} + ${asset}"
echo "Stake OUTPUT: "${script_address_out}
#
# exit
#
echo -e "\033[0;36m Gathering Staker UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${staker_address} \
    --out-file tmp/staker_utxo.json

TXNS=$(jq length tmp/staker_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${staker_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/staker_utxo.json)
staker_tx_in=${TXIN::-8}

# get script reference tx
script_ref_utxo=$(${cli} transaction txid --tx-file tmp/tx-reference-utxo.signed)

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${staker_address} \
    --tx-in ${staker_tx_in} \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file data/datum/stake-datum.json \
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
