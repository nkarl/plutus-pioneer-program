#!/bin/bash

ASSETS=/workspace/code/Week02/assets
KEYS=/workspace/keys
NAME="$1"
COLLATERAL="$2"
TXIN="$3"

BODY="$ASSETS/collect-gift.txbody"
GIFT="$ASSETS/collect-gift.tx"


# Build the transaction
cardano-cli conway transaction build \
    --testnet-magic 2 \
    --tx-in "$GIFTIN" \
    --tx-in-script-file "$ASSETS/gift.plutus" \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file "$ASSETS/unit.json" \
    --tx-in-collateral "$COLLATERAL" \
    --change-address "$(cat "$KEYS/$NAME.addr")" \
    --out-file "$BODY"
    
# Sign the transaction
cardano-cli transaction sign \
    --tx-body-file "$BODY" \
    --signing-key-file "$KEYS/$NAME.skey" \
    --testnet-magic 2 \
    --out-file "$GIFT"

# Submit the transaction
cardano-cli transaction submit \
    --testnet-magic 2 \
    --tx-file "$GIFT"

TXID=$(cardano-cli transaction txid --tx-file "$GIFT")
echo "transaction id: $TXID"
echo "Cardanoscan: https://preview.cardanoscan.io/transaction/$TXID"