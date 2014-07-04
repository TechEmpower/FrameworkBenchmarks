#!/bin/bash

echo "WARN: Weber is not working"
#fw_exists weber
#[ $? -ne 0 ] || { return 0; }

#git clone https://github.com/elixir-web/weber.git

# To get the two make commands working, we need to hard code the path for elixir's "mix"
#cd weber
#sed -i 's:$(MIX):/home/tfb/FrameworkBenchmarks/installs/elixir-0.13.3/bin/mix:' Makefile
#make
#bash -i -c 'sudo make test'