REBAR = $(shell which rebar3 || echo ./rebar3)

clean:
	@$(REBAR) clean -a

co:
	@$(REBAR) compile

dialyze:
	@$(REBAR) dialyzer

ct:
	@$(REBAR) ct
