#!/bin/bash

ASSETS=${WORKSPACE}/code/Week02/assets
KEYS=${WORKSPACE}/keys
CLAIMANT_NAME="$1"
COLLATERAL="$2"
IN_UTXO="$3"

BODY="$ASSETS/collect-gift.txbody"
COLLECT_TX_FILE="$ASSETS/collect-gift.tx"


# Build the transaction
cardano-cli conway transaction build \
    --testnet-magic 2 \
    --tx-in "$IN_UTXO" \
    --tx-in-script-file "$ASSETS/gift.plutus" \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file "$ASSETS/unit.json" \
    --tx-in-collateral "$COLLATERAL" \
    --change-address "$(cat "$KEYS/$CLAIMANT_NAME.addr")" \
    --out-file "$BODY"
    
# Sign the transaction
cardano-cli transaction sign \
    --tx-body-file "$BODY" \
    --signing-key-file "$KEYS/$CLAIMANT_NAME.skey" \
    --testnet-magic 2 \
    --out-file "$COLLECT_TX_FILE"

# Submit the transaction
cardano-cli transaction submit \
    --testnet-magic 2 \
    --tx-file "$COLLECT_TX_FILE"

TX_ID=$(cardano-cli transaction txid --tx-file "$COLLECT_TX_FILE")
echo "transaction id: $TX_ID"
echo "Cardanoscan: https://preview.cardanoscan.io/transaction/$TX_ID"
