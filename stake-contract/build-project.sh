cabal build -w ghc-8.10.7
cabal run stake-contract

cardano-cli stake-address build --stake-script-file stake-contract.plutus --testnet-magic 123 --out-file stake.addr
cardano-cli transaction policyid --script-file stake-contract.plutus > stake.hash
python3 -c "import binascii;a='$(cat stake.hash)';s=binascii.unhexlify(a);print([x for x in s])" > stake.bytes
cardano-cli stake-address registration-certificate --stake-script-file stake-contract.plutus  --out-file stake.cert
cardano-cli stake-address delegation-certificate --stake-script-file stake-contract.plutus --stake-pool-id 5a849bd6a495d0630f6ba6a367ba4e2b3ccc7a53515812105560c152 --out-file deleg.cert

echo -e "\nStake Testnet Address:" $(cat stake.addr)
echo -e "\nStake Hash:" $(cat stake.hash)
echo -e "\nStake Bytes:" $(cat stake.bytes)
echo -e "\nStake Cert";cat stake.cert | jq
echo -e "\nDeleg Cert";cat deleg.cert | jq

variable=$(cat stake.hash); jq --arg variable "$variable" '.fields[0].fields[0].bytes=$variable' ../scripts/data/reg_redeemer.json > ../scripts/data/reg_redeemer-new.json
mv ../scripts/data/reg_redeemer-new.json ../scripts/data/reg_redeemer.json

echo -e "\nDONE\n"

