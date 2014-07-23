#!/bin/bash

#echo "WARN: Weber is not working"
#return 1
fw_exists weber
[ $? -ne 0 ] || { return 0; }

git clone https://github.com/elixir-web/weber.git

# To get the two make commands working, we need to hard code the path for elixir's "mix"
cd weber
make
bash -i -c 'sudo make test'
