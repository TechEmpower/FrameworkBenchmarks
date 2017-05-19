#!/bin/bash

fw_depends h2o

"${H2O_HOME}/bin/h2o" -c "$TROOT/h2o.conf" -m daemon
