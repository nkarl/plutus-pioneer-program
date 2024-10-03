#!/bin/bash

ERA=$1
SENDER=$2
IN_UTXO=$3
FAUCET_ADDR=addr_test1vqeux7xwusdju9dvsj8h7mca9aup2k439kfmwy773xxc2hcu7zy99

cardano-cli $ERA transaction build \
  --testnet-magic 2 \
  --tx-in $IN_UTXO \
  --change-address "$FAUCET_ADDR" \
  --out-file tx.raw

cardano-cli $ERA transaction sign \
  --testnet-magic 2 \
  --tx-body-file tx.raw \
  --signing-key-file ./keys/$SENDER.skey \
  --out-file tx.signed

cardano-cli $ERA transaction submit \
  --testnet-magic 2 \
  --tx-file tx.signed
