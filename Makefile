all: app  | erl.mk

erl.mk:
	curl -fsSLo $@ 'https://raw.github.com/fenollp/erl-mk/master/erl.mk' || rm $@

-include erl.mk
# Your targets after this line.
.PHONY: distclean clean debug test

clean: clean-ebin
distclean: clean clean-deps
	$(if $(wildcard erl.mk), rm erl.mk)

test: ERLCFLAGS += +debug_info +export_all
test: app
	erl -pz ebin/ -noshell -eval '$(APP):main().' -eval 'halt().'

debug: debug-app
