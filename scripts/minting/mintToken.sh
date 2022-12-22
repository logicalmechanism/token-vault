#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat ../testnet.magic)

# get params
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file ../tmp/protocol.json

# minting policy
mint_path="policy/policy.script"

# collat, seller, reference
seller_address=$(cat ../wallets/seller-wallet/payment.addr)
seller_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/seller-wallet/payment.vkey)

# increment the slot inside the policy
currentSlot=$(cat ./policy/policy.script | jq .scripts[0].slot)
nextSlot=$((${currentSlot} + 1))

# update the starting lock time
variable=${nextSlot}; jq --argjson variable "$variable" '.scripts[0].slot=$variable' ./policy/policy.script > ./policy/policy-new.script
mv ./policy/policy-new.script ./policy/policy.script

# create a new policy id
cardano-cli transaction policyid --script-file ./policy/policy.script > ./policy/policy.id

# pid and tkn
policy_id=$(cat policy/policy.id)
token_name=$(echo -n "ThisIsOneStarterTokenForTesting0" | od -A n -t x1 | sed 's/ *//g' | tr -d '\n')
amount=1

# assets
mint_asset="${amount} ${policy_id}.${token_name}"

# mint utxo
utxo_value=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out="${seller_address} + 5000000 + ${mint_asset}" | tr -dc '0-9')

seller_address_out="${seller_address} + ${utxo_value} + ${mint_asset}"
echo "Mint OUTPUT: "${seller_address_out}
#
# exit
#
echo -e "\033[0;36m Gathering Seller UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${seller_address} \
    --out-file ../tmp/seller_utxo.json

TXNS=$(jq length ../tmp/seller_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${seller_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'to_entries[] | select(.value.value | length < 2) | .key | . + $alltxin + " --tx-in"' ../tmp/seller_utxo.json)
seller_tx_in=${TXIN::-8}

slot=$(${cli} query tip --testnet-magic ${testnet_magic} | jq .slot)
current_slot=$((${slot} - 1))
final_slot=$((${slot} + 500))

# exit
echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --invalid-before ${current_slot} \
    --invalid-hereafter ${final_slot} \
    --change-address ${seller_address} \
    --tx-in ${seller_tx_in} \
    --tx-out="${seller_address_out}" \
    --required-signer-hash ${seller_pkh} \
    --mint-script-file policy/policy.script \
    --mint="${mint_asset}" \
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
    --signing-key-file ../wallets/seller-wallet/payment.skey \
    --tx-body-file ../tmp/tx.draft \
    --out-file ../tmp/tx.signed \
    --testnet-magic ${testnet_magic}
#    
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file ../tmp/tx.signed
