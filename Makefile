REBAR_CONFIG := $(PWD)/rebar.config
EBIN_DIR := $(PWD)/ebin
ARGS_FILE := $(PWD)/etc/vm.args
CONFIG_FILE := $(PWD)/etc/rehc.config
LOGS_DIR := /var/log/
PIPES_DIR := /tmp/
RUN_ERL := /usr/local/bin/run_erl
ERL := /usr/local/bin/erl
TO_ERL := /usr/local/bin/to_erl

all: compile start

compile:
	rebar compile

clean:
	@echo -n "Cleaning REHC....."
	@rebar clean
	@rm -rf erl_crash.dump
	@echo "[OK]"

.PHONY: test

test:
	@echo -n "Testing REHC....."
	@rebar compile eunit

start:
	@echo -n "Starting REHC....."
	@$(RUN_ERL) -daemon $(PIPES_DIR) $(LOGS_DIR) "exec $(ERL) -config $(CONFIG_FILE) -args_file $(ARGS_FILE) -pa $(EBIN_DIR)/ -eval 'application:start(rehc).'"
	@echo "[OK]"

attach:
	@$(TO_ERL) $(PIPES_DIR)
