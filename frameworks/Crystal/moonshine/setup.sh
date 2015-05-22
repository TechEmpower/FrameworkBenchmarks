source ${IROOT}/crystal.installed

$crystal deps install

$crystal server.cr &
