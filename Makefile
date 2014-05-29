PROJECT = elli_machine

REBAR := $(shell which rebar 2>/dev/null || echo ./rebar)
REBAR_URL := https://github.com/downloads/basho/rebar/rebar

DRAKON_URL := http://downloads.sourceforge.net/project/drakon-editor/drakon_editor1.22.zip

DIALYZER = dialyzer

all: compile

./rebar:
	erl -noshell -s inets start -s ssl start \
        -eval '{ok, saved_to_file} = httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "./rebar"}])' \
        -s inets stop -s init stop
	chmod +x ./rebar

drakon:
	mkdir -p drakon

drakon_editor: drakon
	erl -noshell -s inets start -s ssl start \
        -eval '{ok, saved_to_file} = httpc:request(get, {"$(DRAKON_URL)", []}, [], [{stream, "./drakon/drakon_editor1.22.zip"}])' \
        -s inets stop -s init stop
	unzip -u drakon/drakon_editor1.22.zip -d drakon
	echo tclsh8.6 drakon/drakon_editor.tcl $$\@ \& > drakon_editor
	chmod +x drakon_editor
	
drakon_gen: drakon_editor
	echo tclsh8.6 drakon/drakon_gen.tcl $$\@  > drakon_gen
	chmod +x drakon_gen

src/elli_machine_flow.erl: drn/elli_machine_flow.drn drakon_gen
	./drakon_gen -in drn/elli_machine_flow.drn -out src
	
compile: rebar src/elli_machine_flow.erl
	$(REBAR) compile

eunit: rebar
	$(REBAR) get-dep compile
	$(REBAR) eunit -v skip_deps=true

clean: rebar
	$(REBAR) clean

distclean: 
	rm $(REBAR)
	rm -rf drakon
	rm -f drakon_editor
	rm -f drakon_gen


# dializer 

build-plt:
	@$(DIALYZER) --build_plt --output_plt .$(PROJECT).plt \
		--apps erts kernel stdlib crypto public_key ssl -r deps

dialyze:
	@$(DIALYZER) -pa deps/*/ebin --src src --plt .$(PROJECT).plt --no_native \
		-Werror_handling -Wrace_conditions -Wunmatched_returns 
