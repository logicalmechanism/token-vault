#!/usr/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat testnet.magic)

# script
lock_path="../locking-contract/locking-contract.plutus"
stake_path="../stake-contract/stake-contract.plutus"
script_address=$(${cli} address build --payment-script-file ${lock_path} --stake-script-file ${stake_path} --testnet-magic ${testnet_magic})

# get script info
echo -e "\033[0;36m Gathering Script UTxO Information  \033[0m"
${cli} query utxo \
    --address ${script_address} \
    --testnet-magic ${testnet_magic} \
    --out-file tmp/script_utxo.json
# transaction variables
TXNS=$(jq length tmp/script_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${script_address} \033[0m \n";
   exit;
fi

echo -e "\033[0;36m UTxO End Time \033[0m"

endTime=$(cat tmp/script_utxo.json | jq -r to_entries[].value.inlineDatum.fields[3].int)

currentTime=$(echo `expr $(echo $(date +%s%3N)) + $(echo 0)`)

timeDiff=$((${endTime} - ${currentTime}))

if [[ ${timeDiff} -gt 0 ]] ; then
    echo -e "\n \033[0;31m UTxO Is Still Locked \033[0m \n";
else
    echo -e "\033[0;35m UTxO Is Unlockable \033[0m"
    exit
fi

seconds=$((${timeDiff} / 1000))

if [[ ${seconds} -gt 60 ]] ; then
    minutes=$((${seconds} / 60))
    if [[ ${minutes} -gt 60 ]] ; then
        hours=$((${minutes} / 60))
        if [[ ${hours} -gt 24 ]] ; then
            days=$((${hours} / 24))
            # add in years if required
            echo $days Days
        else
            echo $hours Hours
        fi
    else
        echo $minutes Minutes
    fi
else
    echo $seconds Seconds
fi
