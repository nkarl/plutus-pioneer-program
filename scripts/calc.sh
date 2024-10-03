TX_RAW=$1
NET_PARAMS=$2
cardano-cli transaction calculate-min-fee \
 --testnet-magic 2 \
 --tx-body-file $TX_RAW \
 --tx-in-count 1 \
 --tx-out-count 1 \
 --witness-count 1 \
 --byron-witness-count 0 \
 --protocol-params-file $NET_PARAMS
