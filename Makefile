SHELL := /bin/bash
DEBUG = #-g
CPPFLAGS =  -O2 $(DEBUG) -I .
OCAMLFLAGS = -annot -w Z $(DEBUG)
PACKAGES = batteries

all: bench-all

.PHONY: clean hwrun %.threadlog bench-all runlog.% outliers rectest

clean:
	rm -f *.a *.o *.cmi *.cmx *.cmo *.cmxa *.annot lib_b/*.o lib_u/*.o

http-baseconn.o: http-baseconn.cc
	g++ $(CPPFLAGS) -c $^ -o $@

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

FLOW=hashtbl_param.cmx ean_std.cmx pcregex.cmx minreg.cmx PCFG.cmx ns_types.cmx simplify.cmx ns_yac.cmx ns_lex.cmx ruleset.cmx tcam.cmx decider.cmx fdd.cmx bdd.cmx optimizers.cmx regex_dfa.cmx ns_parse.cmx ns_run.cmx prog_parse.cmx arg2.cmx genrec.cmx

ns_yac.ml: ns_yac.mly
	menhir ns_yac.mly

ns_lex.ml: ns_lex.mll
	ocamllex ns_lex.mll

ns_yac.cmi: ns_yac.ml
	ocamlc -annot -g -c ns_yac.mli

ns_yac.cmx: ns_yac.cmi ns_yac.ml
	ocamlfind ocamlopt -package $(PACKAGES) $(OCAMLFLAGS) -c ns_yac.ml

ns_yac.cmo: ns_yac.cmi ns_yac.ml
	ocamlfind ocamlc -package $(PACKAGES) $(OCAMLFLAGS) -c ns_yac.ml

pcap.cmo: pcap.ml
	ocamlfind ocamlc $(OCAMLFLAGS) -c -syntax camlp4o -package $(PACKAGES),bitstring.syntax,bitstring pcap.ml -o pcap.cmo

%.cmx: %.ml
	ocamlfind ocamlopt -package $(PACKAGES) $(OCAMLFLAGS) -c $^

%.cmo: %.ml
	ocamlfind ocamlc -package $(PACKAGES) $(OCAMLFLAGS) -c $^

OLIBS = #libocamlviz.cmxa 

bench-bpac: bpac.cmxa pcap.cmx bench.cmx
	ocamlfind ocamlopt -annot -package $(PACKAGES),bitstring -linkpkg -I . -cclib -lstdc++ -cclib -lpcre bpac.cmxa $(LIBS) $(FLOW) pcap.cmx bench.cmx -o $@

bench-upac: upac.cmxa pcap.cmx bench.cmx
	ocamlfind ocamlopt -annot -package $(PACKAGES),bitstring -linkpkg -I . -cclib -lstdc++ -cclib -lpcre upac.cmxa $(LIBS) $(FLOW) pcap.cmx bench.cmx -o $@

pcap.cmx: pcap.ml
	ocamlfind ocamlopt $(OCAMLFLAGS) -c -syntax camlp4o -package $(PACKAGES),bitstring.syntax,bitstring pcap.ml -o pcap.cmx

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


mldeps: 
	ocamlfind ocamldep -package bitstring.syntax -syntax camlp4o *.ml *.mll *.mly > mldeps

include mldeps

ns_yac.cmi: ns_types.ml

##########################
# CREATE BENCHMARK FILES #
##########################
RUNS =  98w1-mon 98w1-tue 98w1-wed 98w1-thu 98w1-fri \
	98w2-monday 98w2-tuesday 98w2-wednesday 98w2-thursday 98w2-friday \
	                         98w3-wednesday                           \
	                                                                  \
	                                                                  \
	            98w6-tuesday 98w6-wednesday 98w6-thursday 98w6-friday \
	98w7-monday 98w7-tuesday 98w7-wednesday 98w7-thursday 98w7-friday \
	99w1-monday 99w1-tuesday 99w1-wednesday 99w1-thursday 99w1-friday \
	99w2-monday 99w2-tuesday 99w2-wednesday 99w2-thursday 99w2-friday \
	99w3-monday 99w3-tuesday 99w3-wednesday 99w3-thursday 99w3-friday \
	99w4-monday 99w4-tuesday 99w4-wednesday 99w4-thursday 99w4-friday \
	99w5-monday 99w5-tuesday 99w5-wednesday 99w5-thursday 99w5-friday \

MEM_PRE = LD_PRELOAD="/usr/lib/libtcmalloc.so.0" 
HEADER = "runid\tparser\titers\ttime\tgbit\tgbps\tmem\tflows\tevents\tpct_parsed"

rundata100: bench-bpac bench-upac
	-mv -b $@ $@.bkp
	echo -e $(HEADER) > $@
	time for a in $(RUNS); do \
	    $(MEM_PRE) ./bench-bpac -n 100 ~/traces/http/use/$$a* | tee -a $@; \
	    $(MEM_PRE) ./bench-upac -n 100 ~/traces/http/use/$$a* | tee -a $@; \
	done

rundata1: bench-bpac bench-upac
	-mv -b $@ $@.bkp
	echo -e $(HEADER) > $@
	time for a in $(RUNS); do \
	    $(MEM_PRE) ./bench-bpac ~/traces/http/use/$$a* | tee -a $@; \
	    $(MEM_PRE) ./bench-upac ~/traces/http/use/$$a* | tee -a $@; \
	done

FLOWS = 10000
rectest: bench-bpac
	-mv -b $@ $@.bkp
	echo -e $(HEADER) > $@
	time for a in 3 4 5; do \
	    ./bench-upac --seed 230 -x e-soap.ca -s -g $$a $(FLOWS)|tee -a $@; \
	    ./bench-upac --seed 231 -x e-soap.ca -s -g $$a $(FLOWS)|tee -a $@; \
	    ./bench-upac --seed 232 -x e-soap.ca -s -g $$a $(FLOWS)|tee -a $@; \
	    ./bench-upac --seed 233 -x e-soap.ca -s -g $$a $(FLOWS)|tee -a $@; \
	    ./bench-upac --seed 234 -x e-soap.ca -s -g $$a $(FLOWS)|tee -a $@; \
	    ./bench-upac --seed 235 -x e-soap.ca -s -g $$a $(FLOWS)|tee -a $@; \
	    ./bench-upac --seed 236 -x e-soap.ca -s -g $$a $(FLOWS)|tee -a $@; \
	    ./bench-upac --seed 237 -x e-soap.ca -s -g $$a $(FLOWS)|tee -a $@; \
	    ./bench-upac --seed 238 -x e-soap.ca -s -g $$a $(FLOWS)|tee -a $@; \
	    ./bench-upac --seed 239 -x e-soap.ca -s -g $$a $(FLOWS)|tee -a $@; \
	done

figures: rundata1 memory.R
	R --save < memory.R

outliers: bench-upac
	./bench-upac ~/traces/http/use/98w3-wednesday.pcap 
