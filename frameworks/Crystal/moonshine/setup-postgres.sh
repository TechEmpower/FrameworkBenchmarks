#!/bin/bash

fw_depends crystal

crystal deps install

crystal server-postgres.cr &
