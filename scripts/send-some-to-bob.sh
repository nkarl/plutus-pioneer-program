#!/bin/bash

IN_UTXO=$1
AMOUNT=$2

ALICE_ADDR=$(< $WORKSPACE/keys/alice.addr)
BOB_ADDR=$(< $WORKSPACE/keys/bob.addr)
#echo $BOB_ADDR
ALICE_KEY_FILE=$WORKSPACE/keys/alice.skey
#echo $ALICE_KEY_FILE

cardano-cli conway transaction build \
  --testnet-magic 2 \
  --tx-in $IN_UTXO \
  --tx-out "$BOB_ADDR+$AMOUNT" \
  --change-address "$ALICE_ADDR" \
  --out-file "tx.raw"

#cardano-cli conway transaction sign \
  #--testnet-magic 2 \
  #--tx-body-file "tx.raw" \
  #--signing-key-file $ALICE_KEY_FILE \
  #--out-file "tx.signed"

#cardano-cli conway transaction submit \
  #--testnet-magic 2 \
  #--tx-file "tx.signed"
