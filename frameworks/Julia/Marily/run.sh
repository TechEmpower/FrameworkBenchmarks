for i in $(seq 0 $(($(nproc --all)-1)));
	do julia --project=. --threads auto julia_server.jl &
	done

while : ; do sleep 1 ; done
