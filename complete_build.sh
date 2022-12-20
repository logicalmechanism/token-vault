#!/bin/bash

echo -e "\033[1;35m\nStarting Complete Build... \033[0m" 

# get info
poolId=$(cat start_info.json | jq -r .poolId)
rewardPkh=$(cat start_info.json | jq -r .rewardPkh)
rewardSc=$(cat start_info.json | jq -r .rewardSc)

echo -e "\033[1;35m\nUpdating Staking Contract \033[0m"

# store extra stuff in info folder
mkdir -p info

# starter nft data
python3 -c "import binascii;a='${poolId}';s=binascii.unhexlify(a);print([x for x in s])"    > info/pool.id
python3 -c "import binascii;a='${rewardPkh}';s=binascii.unhexlify(a);print([x for x in s])" > info/reward.pkh
python3 -c "import binascii;a='${rewardSc}';s=binascii.unhexlify(a);print([x for x in s])"  > info/reward.sc

# change the pool id
python3 -c "from update_contracts import changePoolId;changePoolId('./stake-contract/src/StakeContract.hs', './stake-contract/src/StakeContract.hs-new.hs', $(cat info/pool.id))"
mv ./stake-contract/src/StakeContract.hs-new.hs ./stake-contract/src/StakeContract.hs

# change payout pkh
python3 -c "from update_contracts import changeRewardPkh;changeRewardPkh('./stake-contract/src/StakeContract.hs', './stake-contract/src/StakeContract.hs-new.hs', $(cat info/reward.pkh))"
mv ./stake-contract/src/StakeContract.hs-new.hs ./stake-contract/src/StakeContract.hs

# change payout sc
python3 -c "from update_contracts import changeRewardSc;changeRewardSc('./stake-contract/src/StakeContract.hs', './stake-contract/src/StakeContract.hs-new.hs', $(cat info/reward.sc))"
mv ./stake-contract/src/StakeContract.hs-new.hs ./stake-contract/src/StakeContract.hs

# build
cd stake-contract

echo -e "\033[1;35m\nBuilding Staking Contract\n\033[0m"

# remove old data
rm stake-contract.plutus
rm stake.addr
rm stake.hash
rm stake.bytes
rm stake.cert
rm deleg.cert

# build
cabal build -w ghc-8.10.7
cabal run stake-contract

# get stake data
cardano-cli stake-address build --stake-script-file stake-contract.plutus --testnet-magic $(cat ../scripts/testnet.magic) --out-file stake.addr
cardano-cli transaction policyid --script-file stake-contract.plutus > stake.hash
python3 -c "import binascii;a='$(cat stake.hash)';s=binascii.unhexlify(a);print([x for x in s])" > stake.bytes
cardano-cli stake-address registration-certificate --stake-script-file stake-contract.plutus  --out-file stake.cert
cardano-cli stake-address delegation-certificate --stake-script-file stake-contract.plutus --stake-pool-id ${poolId} --out-file deleg.cert

echo -e "\033[1;36m\nStake Addr: $(cat stake.addr) \033[0m"
echo -e "\033[1;36mStake Hash: $(cat stake.hash) \033[0m"
echo -e "\033[1;36mStake Byte: $(cat stake.bytes) \033[0m"
echo -e "\nStake Cert";cat stake.cert | jq
echo -e "\nDeleg Cert";cat deleg.cert | jq

# update the withdraw and delegate redeemer to put the stake key on chain
variable=$(cat stake.hash); jq --arg variable "$variable" '.fields[0].fields[0].bytes=$variable' ../scripts/data/redeemer/withdraw-redeemer.json > ../scripts/data/redeemer/withdraw-redeemer-new.json
mv ../scripts/data/redeemer/withdraw-redeemer-new.json ../scripts/data/redeemer/withdraw-redeemer.json

# update the register redeemer to put the stake key on chain
variable=$(cat stake.hash); jq --arg variable "$variable" '.fields[0].fields[0].bytes=$variable' ../scripts/data/redeemer/delegate-redeemer.json > ../scripts/data/redeemer/delegate-redeemer-new.json
mv ../scripts/data/redeemer/delegate-redeemer-new.json ../scripts/data/redeemer/delegate-redeemer.json

cd ../locking-contract

echo -e "\033[1;35m\nBuilding Locking Contract\n\033[0m"

# remove old data
rm locking-contract.plutus
rm validator.addr
rm validator.hash
rm validator.bytes

# build
cabal build -w ghc-8.10.7
cabal run locking-contract

cardano-cli address build --payment-script-file locking-contract.plutus --testnet-magic $(cat ../scripts/testnet.magic) --out-file validator.addr
cardano-cli transaction policyid --script-file locking-contract.plutus > validator.hash
python3 -c "import binascii;a='$(cat validator.hash)';s=binascii.unhexlify(a);print([x for x in s])" > validator.bytes

echo -e "\033[1;36m\nValidator Addr: $(cat validator.addr) \033[0m"
echo -e "\033[1;36mValidator Hash: $(cat validator.hash) \033[0m"
echo -e "\033[1;36mValidator Byte: $(cat validator.bytes) \033[0m"

cd ..

echo -e "\033[1;35m\nCompute SHA256SUM \033[0m"

find ./*-contract/ -name '*.hash' -type f -exec sha256sum {} \; > info/hash.hashes
echo -e "\033[1;36m\nIndividual sha256sum\n\033[0m"
echo -e "\033[1;33m$(cat info/hash.hashes) \033[0m"

find . -name '*.hashes' -type f -exec sha256sum {} \; > info/final.check
echo -e "\033[1;35m\nCombined sha256sum\n\033[0m"
echo -e "\033[1;32m$(sha256sum info/final.check) \033[0m"
