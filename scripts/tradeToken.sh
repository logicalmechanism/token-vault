#!/bin/bash
set -e

# SET UP VARS HERE
export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat testnet.magic)

# Addresses
sender_address=$(cat wallets/seller-wallet/payment.addr)
# receiver_address=$(cat wallets/staker-wallet/base.addr)
receiver_address="addr_test1qrvnxkaylr4upwxfxctpxpcumj0fl6fdujdc72j8sgpraa9l4gu9er4t0w7udjvt2pqngddn6q4h8h3uv38p8p9cq82qav4lmp"

# Define Asset to be printed here
asset="9223372036854775807 6effa18e41008cd0b13f3959a5a4af40b92ca936bb7669f40d3b1f81.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6732 + 9223372036854775807 c34332d539bb554707a2d8826f2057bc628ac433a779c2f43d4a5b5c.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6731"

min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out="${receiver_address} + 5000000 + ${asset}" | tr -dc '0-9')

token_to_be_traded="${receiver_address} + ${min_utxo} + ${asset}"

echo -e "\nTrading A Token:\n" ${token_to_be_traded}
#
# exit
#
echo -e "\033[0;36m Gathering UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${sender_address} \
    --out-file tmp/sender_utxo.json

TXNS=$(jq length tmp/sender_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${sender_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/sender_utxo.json)
HEXTXIN=${TXIN::-8}

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${sender_address} \
    --tx-in ${HEXTXIN} \
    --tx-out="${token_to_be_traded}" \
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