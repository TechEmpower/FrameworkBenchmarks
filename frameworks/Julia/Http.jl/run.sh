for i in $(seq 0 $(($(nproc --all)-1)));
	# https://stackoverflow.com/a/14388707/3757815
	do julia --project=. --threads=2,2 julia_server.jl &
done

while : ; do sleep 1 ; done
