#!/bin/bash

ERA=$1
SENDER_KEY=$2
IN_UTXO=$3
RECIPIENT_ADDR=$4
AMOUNT=$5

cardano-cli $ERA transaction build \
  --testnet-magic 2 \
  --tx-in $IN_UTXO \
  --change-address "$RECIPIENT_ADDR+$AMOUNT" \
  --out-file "tx.raw"

#cardano-cli $ERA transaction sign \
  #--testnet-magic 2 \
  #--tx-body-file "tx.raw" \
  #--signing-key-file $SENDER_KEY \
  #--out-file "tx.signed"

#cardano-cli $ERA transaction submit \
  #--testnet-magic 2 \
  #--tx-file "tx.signed"
