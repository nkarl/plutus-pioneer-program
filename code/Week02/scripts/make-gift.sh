#!/bin/bash

ASSETS=${WORKSPACE}/code/Week02/assets
KEYS=${WORKSPACE}/keys
SENDER_NAME="$1"
IN_UTXO="$2"
BODY="$ASSETS/gift.txbody"
GIFT_TX_FILE="$ASSETS/gift.tx"

# Build gift address 
cardano-cli address build \
    --payment-script-file "$ASSETS/gift.plutus" \
    --testnet-magic 2 \
    --out-file "$ASSETS/gift.addr"

# Build the transaction
cardano-cli conway transaction build \
    --testnet-magic 2 \
    --tx-in "$IN_UTXO" \
    --tx-out "$(< "$ASSETS/gift.addr") + 3000000 lovelace" \
    --tx-out-inline-datum-file "$ASSETS/unit.json" \
    --change-address "$(< "$KEYS/$SENDER_NAME.addr")" \
    --out-file "$BODY"
    
# Sign the transaction
cardano-cli transaction sign \
    --tx-body-file "$BODY" \
    --signing-key-file "$KEYS/$SENDER_NAME.skey" \
    --testnet-magic 2 \
    --out-file "$GIFT_TX_FILE"

# Submit the transaction
cardano-cli transaction submit \
    --testnet-magic 2 \
    --tx-file "$GIFT_TX_FILE"

TX_ID=$(cardano-cli transaction txid --tx-file "$GIFT_TX_FILE")
echo "transaction id: $TX_ID"
echo "Cardanoscan: https://preview.cardanoscan.io/transaction/$TX_ID"
