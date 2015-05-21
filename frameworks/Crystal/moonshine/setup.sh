export CRYSTAL_HOME=${IROOT}/crystal-0.7.1-1
crystal=${IROOT}/crystal

$crystal deps install

$crystal server.cr &

exit 0