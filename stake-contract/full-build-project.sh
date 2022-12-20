cabal clean
cabal update
cabal build -w ghc-8.10.7 -O2
cabal run stake-contract

cardano-cli stake-address build --stake-script-file stake-contract.plutus --testnet-magic 123 --out-file stake.addr
cardano-cli transaction policyid --script-file stake-contract.plutus > stake.hash
python3 -c "import binascii;a='$(cat stake.hash)';s=binascii.unhexlify(a);print([x for x in s])" > stake.bytes
cardano-cli stake-address registration-certificate --stake-script-file stake-contract.plutus  --out-file stake.cert
cardano-cli stake-address delegation-certificate --stake-script-file stake-contract.plutus --stake-pool-id 07ac7dee6c82177096b70ccf21cfb8965c1fb08e079f9ca4af4b2b3e --out-file deleg.cert

echo -e "\nStake Testnet Address:" $(cat stake.addr)
echo -e "\nStake Hash:" $(cat stake.hash)
echo -e "\nStake Bytes:" $(cat stake.bytes)
echo -e "\nStake Cert";cat stake.cert | jq
echo -e "\nDeleg Cert";cat deleg.cert | jq
echo -e "\nDONE\n"