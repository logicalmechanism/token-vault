# The Token Vault

A smart contract for locking a UTxO for some specified amount of time and staking ADA collected inside the contract.

The locking contract allows for a utxo to be unlocked with a correct signature of the wallet, returning value, and tx validity range. The staking contract is designed for a single specific pool id where the rewards can only be withdrawn to a specific wallet known at compile time.

## Building

Inside the folder is the start_info.json file and the complete_build.sh script. The start information is used to hold the staking pool and reward address information. The example below is for a payment only type of address that does not contain a stake key.

```json
{
  "__comment1__": "This is the stake pool id",
  "poolId": "8a77ce4ffc0c690419675aa5396df9a38c9cd20e36483d2d2465ce86",
  "__comment2__": "This is the reward address",
  "rewardPkh": "a2108b7b1704f9fe12c906096ea1634df8e089c9ccfd651abae4a439",
  "rewardSc": ""
}
```

The reward public key hash and stake credential is obtained from the hex encoding of the bech32 address.

The complete_build.sh script will auto compile the set of smart contracts based off the information provided inside the start_info.json file. The auto build function will produce the correct redeemers for staking but the datums for the off-chain code will need to adjusted to a users own test wallets.

### Getting The Payment And Stake Hex

A bech32 payment address:
```
addr_test1vz3ppzmmzuz0nlsjeyrqjm4pvdxl3cyfe8x06eg6htj2gwgv02qjt
```

The hex encoding is:
```
60a2108b7b1704f9fe12c906096ea1634df8e089c9ccfd651abae4a439
```

The prefix 60 is the testnet network flag. The resulting public key hash is

```
a2108b7b1704f9fe12c906096ea1634df8e089c9ccfd651abae4a439
```

This type of address does not contain a stake key so that field is left blank at compile time.

A bech32 staking address:
```
addr_test1qrvnxkaylr4upwxfxctpxpcumj0fl6fdujdc72j8sgpraa9l4gu9er4t0w7udjvt2pqngddn6q4h8h3uv38p8p9cq82qav4lmp
```

The hex encoding is:
```
00d9335ba4f8ebc0b8c9361613071cdc9e9fe92de49b8f2a4782023ef4bfaa385c8eab7bbdc6c98b50413435b3d02b73de3c644e1384b801d4
```

This type of address has a public key hash and a stake credential.

The resulting public key hash is
```
d9335ba4f8ebc0b8c9361613071cdc9e9fe92de49b8f2a4782023ef4
```
and the stake credential is
```
bfaa385c8eab7bbdc6c98b50413435b3d02b73de3c644e1384b801d4
```

## Usage

The test off-chain code inside the scripts folder is designed to be used sequentially, 00 then 01 etc. The test code expects test wallets to exist inside a folder called wallets. The examples provided assume a reference, collateral, reward, and staker wallet. Please use the testnet faucet to obtain test ADA.

The first step is creating the reference scripts used in the following off-chain code. The next two steps involve registering and delegating the stake address. The third step requires the stake address to have withdrawable rewards. The forth and fifth step involves the time locking and removal of a UTxO from the smart contract.

The time lock script, 04_lockIntoVault.sh, assumes an input in minutes. It will use the function

```bash
$(echo `expr $(echo $(date +%s%3N)) + $(echo 0)`)
```

to calculate the current time in Unix time. It will then use the user input to determine the end time. This information will be placed into the datum file for later use.

The locking script assumes some token. Please update the script for whichever token you wish to lock.

```bash
# token information
policy_id="f61e1c1d38fc4e5b0734329a4b7b820b76bb8e0729458c153c4248ea"
token_name="5468697349734f6e6553746172746572546f6b656e466f7254657374696e6731"
amount=1

# asset to lock
asset="${amount} ${policy_id}.${token_name}"
```

### Creating The Lock Datum

The locking contract requires a specific datum to work. It requires the public key hash and an optional stake key from a wallet. It also assumes a start and end time.

```hs
data CustomDatumType = CustomDatumType
  { cdtPkh       :: PlutusV2.PubKeyHash
  -- ^ A payment public key hash.
  , cdtSc        :: PlutusV2.PubKeyHash
  -- ^ A payment staking credential.
  , cdtStartTime :: Integer
  -- ^ The starting lock time.
  , cdtEndTime   :: Integer
  -- ^ The ending lock time.
  }
```

An example datum is below:

```json
{
  "constructor": 0,
  "fields": [
    {
      "bytes": "a2108b7b1704f9fe12c906096ea1634df8e089c9ccfd651abae4a439"
    },
    {
      "bytes": ""
    },
    {
      "int": 1671502388771
    },
    {
      "int": 1671502688771
    }
  ]
}
```

In this example, the wallet does not have a stake key and the UTxO was locked for 5 minutes.

UTxos attempted to be unlocked before the time lock is finished will result in a failure with the error message of 

```bash
Script debugging logs: 
    Time Locking
    Remove Error
```


## Notes

This contract is meant to display the simplicity of a token vault where a user may be rewarding for time locking some UTxO inside the contract. The collection of ADA associated with the tokens is then staked to a staking pool where one predefined wallet may be rewarded the staking reward. Any wallet may spend the staking reward but the only destination is the reward address.


# token-vault
