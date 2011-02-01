SHELL := /bin/bash
CPPFLAGS =  -O2
OCAMLFLAGS = -annot -unsafe

.PHONY: clean hwrun %.threadlog bench-all

all: bench-all


http-baseconn.o: http-baseconn.cc
	g++ $(CPPFLAGS) -c -I lib/ $^ -o $@

####
#### Compile bpac.cmxa
####

lib_b/%.o: lib_b/%.cc
	g++ $(CPPFLAGS) -c -I lib_b/ $^ -o $@

bpac.cmxa: http-baseconn.o lib_b/binpac_buffer.o lib_b/http_pac.o lib_b/libbinpac.a lib_b/bpac_stubs.o anypac.cmx
	ocamlmklib -custom -o bpac $^

####
#### Compile upac.cmxa
####

lib_u/%.o: lib_u/%.cc
	g++ $(CPPFLAGS) -c -I lib_u/ $^ -o $@

upac.cmxa: lib_u/binpac.o lib_u/http_pac_fast.o lib_u/http_matcher.o lib_u/libubinpac.a lib_u/upac_stubs.o anypac.cmx
	ocamlmklib -custom -o upac $^

####
#### Compile flow.cmxa
####

FLOW=hashtbl_param.cmx ean_std.cmx pcregex.cmx minreg.cmx PCFG.cmx ns_types.cmx simplify.cmx ns_yac.cmx ns_lex.cmx ruleset.cmx tcam.cmx decider.cmx fdd.cmx bdd.cmx optimizers.cmx regex_dfa.cmx disj_set.cmx d2fa.cmx vsdfa.cmx ns_parse.cmx prog_parse.cmx

ns_yac.ml: ns_yac.mly
	menhir ns_yac.mly

ns_lex.ml: ns_lex.mll
	ocamllex ns_lex.mll

ns_yac.cmi: ns_yac.ml
	ocamlc -annot -g -c ns_yac.mli

ns_yac.cmx: ns_yac.cmi ns_yac.ml
	ocamlfind ocamlopt -package batteries -thread $(OCAMLFLAGS) -c ns_yac.ml

ns_yac.cmo: ns_yac.cmi ns_yac.ml
	ocamlfind ocamlc -package batteries -thread $(OCAMLFLAGS) -c ns_yac.ml

pcap.cmo: pcap.ml
	ocamlfind ocamlc $(OCAMLFLAGS) -c -syntax camlp4o -package batteries,bitstring.syntax,bitstring pcap.ml -o pcap.cmo

%.cmx: %.ml
	ocamlfind ocamlopt -package batteries -thread $(OCAMLFLAGS) -c $^

%.cmo: %.ml
	ocamlfind ocamlc -package batteries -thread $(OCAMLFLAGS) -c $^

bench-bpac: bpac.cmxa $(FLOW) pcap.cmx bench.ml
	ocamlfind ocamlopt -package batteries,camlp4.macro -syntax camlp4o -thread -annot -c bench.ml
	ocamlfind ocamlopt -annot -package batteries,bitstring -thread -linkpkg -I . -cclib -lstdc++ -cclib -lpcre bpac.cmxa libocamlviz.cmxa $(FLOW) pcap.cmx bench.cmx -o $@

bench-upac: upac.cmxa $(FLOW) pcap.cmx bench.ml
	ocamlfind ocamlopt -package batteries,camlp4.macro -syntax camlp4o -thread -annot -c bench.ml
	ocamlfind ocamlopt -annot -package batteries,bitstring -thread -linkpkg -I . -cclib -lstdc++ -cclib -lpcre upac.cmxa libocamlviz.cmxa $(FLOW) pcap.cmx bench.cmx -o $@

clean:
	rm -f *.a *.o *.cmi *.cmx *.cmo *.cmxa *.annot

pcap.cmx: pcap.ml
	ocamlfind ocamlopt $(OCAMLFLAGS) -c -syntax camlp4o -package batteries,bitstring.syntax,bitstring pcap.ml -o pcap.cmx

pcap: pcap.ml
	ocamlbuild -no-hygiene pcap.native
	mv pcap.native pcap

hwrun:
	ocamlbuild -no-hygiene hwrun.native
	mv hwrun.native hwrun

#join -j 1 -o 1.1 1.2 2.2 null.20-timelog null.50-timelog | join -j 1 -o 1.1 1.2 1.3 2.2 - null.100-timelog | join -j 1 -o 1.1 1.2 1.3 1.4 2.2 - null.150-timelog | join -j 1 -o 1.1 1.2 1.3 1.4 1.5 2.2 - null.250-timelog 

####
#### targets for statistics
####


TRACE=traces/Forensic_challenge_4-http.pcap # 99K trace, mostly headers

TRACE2=traces/jub-http.pcap # 345M, mostly content

%.threadlog: bench-% 
	./$^ -1 $(TRACE) & ( echo -n $$a; ./kill_when_flat.sh $$! ) | tee -a $@; sleep 1; echo >> $@

%.perf: bench-%
	for a in $(THREADNUMS); do ./$^ -$$a $(TRACE) | tee -a $@; done; echo >> $@

%.perf2: bench-%
	./$^ -5 $(TRACE2) | tee -a $@
	echo >> $@

MODES=bpac upac 
MEM_MODES=bpac flow null

bench-all: $(patsubst %,bench-%,$(MODES))

threadlog-all: $(patsubst %, %.threadlog, $(MEM_MODES))

perf-all: $(patsubst %, %.perf, $(MODES))

perf2-all: $(patsubst %, %.perf2, $(MODES))




####
#### Junkyard of old code
####

#bpac: http-main.cc
#	g++ -DPARSER=0 -I lib/ http-baseconn.cc lib/http_pac.cc http-main.cc lib/libbinpac.a  -lpcre -g -o bpac

#upac: http-main.cc
#	g++ -DPARSER=1 -I ulib/ http-baseconn.cc ulib/binpac.cc ulib/http_pac_fast.cc ulib/http_matcher.cc http-main.cc ulib/libbinpac.a -lpcre -g -o upac

#flow: flib/flow.o http-main.cc
#	g++ -DPARSER=2 -I flib/ -g -o flow http-baseconn.cc http-main.cc flib/flow.o \
#	flib/PCFG.o flib/ns_types.o flib/simplify.o flib/ns_parse.o flib/prog_parse.o \
#	-L/usr/lib/ocaml -lasmrun -lm -ldl 


#-L/usr/local/lib/ocaml/3.11.2/batteries/ -lbatteries_uni -lbatteries \

define timelog
	set -m; ./$^ $1 $(TRACE) & for((i=0;;++i)) { echo $$i `grep VmRSS /proc/\`pidof $^\`/status | grep -o '[0-9]*'`; sleep 1 || break; } | tee $@ & sleep 20; kill %1; kill %2
endef

%.20-timelog: bench-%
	$(call timelog, 20)

%.50-timelog: bench-%
	$(call timelog, 50)

%.100-timelog: bench-%
	$(call timelog, 100)

%.150-timelog: bench-%
	$(call timelog, 150)

%.250-timelog: bench-%
	$(call timelog, 250)

timelog-all: $(patsubst %, %.20-timelog, $(MODES)) $(patsubst %, %.50-timelog, $(MODES)) $(patsubst %, %.100-timelog, $(MODES)) $(patsubst %, %.150-timelog, $(MODES)) $(patsubst %, %.250-timelog, $(MODES))

flib/flow.o: flib/flow.c
	gcc -static -o flib/flow.o -c flib/flow.c 

include mldeps
