REBAR=./rebar

all:
	@$(REBAR) -r get-deps compile

clean:
	@$(REBAR) clean

run:
	@./start.sh

stop:
	@killall beam.smp
