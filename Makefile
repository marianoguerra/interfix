.PHONY: examples

script:
	rebar3 escriptize

examples:
	for d in examples/*.ifx; do echo $$d; echo; ./interfix erl $$d; done
