#!/bin/bash

assets=/workspace/code/Week02/assets
keypath=/workspace/keys
name="$1"
collateral="$2"
txin="$3"
redeemer="$4"
body="$assets/collect-Homework1.txbody"
tx="$assets/collect-Homework1.tx"


# Build the transaction
cardano-cli conway transaction build \
    --testnet-magic 2 \
    --tx-in "$txin" \
    --tx-in-script-file "$assets/Homework1.plutus" \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file "$redeemer" \
    --tx-in-collateral "$collateral" \
    --change-address "$(cat "$keypath/$name.addr")" \
    --out-file "$body"
    
# Sign the transaction
cardano-cli transaction sign \
    --tx-body-file "$body" \
    --signing-key-file "$keypath/$name.skey" \
    --testnet-magic 2 \
    --out-file "$tx"

# Submit the transaction
cardano-cli transaction submit \
    --testnet-magic 2 \
    --tx-file "$tx"

tid=$(cardano-cli transaction txid --tx-file "$tx")
echo "transaction id: $tid"
echo "Cardanoscan: https://preview.cardanoscan.io/transaction/$tid"