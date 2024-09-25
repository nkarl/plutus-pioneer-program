#!/bin/bash

ASSETS=/workspace/code/Week02/assets
KEYS=/workspace/keys
NAME="$1"
TXIN="$2"
BODY="$ASSETS/gift.txbody"
TX="$ASSETS/gift.tx"

# Build gift address 
cardano-cli address build \
    --payment-script-file "$ASSETS/gift.plutus" \
    --testnet-magic 2 \
    --out-file "$ASSETS/gift.addr"

# Build the transaction
cardano-cli conway transaction build \
    --testnet-magic 2 \
    --tx-in "$TXIN" \
    --tx-out "$(cat "$ASSETS/gift.addr") + 3000000 lovelace" \
    --tx-out-inline-datum-file "$ASSETS/unit.json" \
    --change-address "$(cat "$KEYS/$NAME.addr")" \
    --out-file "$BODY"
    
# Sign the transaction
cardano-cli transaction sign \
    --tx-body-file "$BODY" \
    --signing-key-file "$KEYS/$NAME.skey" \
    --testnet-magic 2 \
    --out-file "$TX"

# Submit the transaction
cardano-cli transaction submit \
    --testnet-magic 2 \
    --tx-file "$TX"

TXID=$(cardano-cli transaction txid --tx-file "$TX")
echo "transaction id: $TXID"
echo "Cardanoscan: https://preview.cardanoscan.io/transaction/$TXID"