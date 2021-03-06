SHELL := /bin/bash
DEBUG = -g -fno-omit-frame-pointer
CFLAGS += -O3 -I. -march=native -lrt
CPPFLAGS += $(CFLAGS) -std=c++0x
PACKAGES = batteries,bitstring
OCAMLFLAGS += -annot -w Z -package $(PACKAGES)
OCAMLOPT = ocamlfind ocamlopt $(OCAMLFLAGS)
OCAMLC = ocamlfind ocamlc $(OCAMLFLAGS)
OCAMLBUILD = ocamlbuild -no-hygiene -use-ocamlfind -j 0

all: ns_compile.native bench-siftc

#all: bench-all

.PHONY: clean hwrun %.threadlog bench-all runlog.% outliers rectest

dist-clean: clean

clean:
	$(RM) *.a *.o *.cmi *.cmx *.cmo *.cmxa *.annot lib_b/*.o lib_u/*.o *.ml.d
	$(RM) ns_lex.ml ns_yac.ml ns_yac.mli
	rm -rf _build/

http-baseconn.o: http-baseconn.cc
	$(CXX) $(CPPFLAGS) -c $^ -o $@

####
#### Compile bpac.cmxa
####

lib_b/http_pac.cc lib_b/http_pac.h: lib_b/http.pac  lib_b/binpac-lib.pac
	cd lib_b; ./binpac http.pac; cd ..

lib_b/%.o: lib_b/%.cc
	$(CXX) $(CPPFLAGS) -c -I lib_b/ $^ -o $@

bpac.cmxa: http-baseconn.o lib_b/binpac_buffer.o lib_b/http_pac.o lib_b/bpac_stubs.o anypac.cmx
	ocamlmklib -custom -o bpac $^

####
#### Compile upac.cmxa
####

lib_u/http_pac_fast.cc lib_u/http_pac.h lib_u/http_pac_fast.h: lib_u/http.pac lib_u/http-analyzer.pac lib_u/http-protocol.pac
	cd lib_u; ./ultrapac http.pac; cd ..

lib_u/%.o: lib_u/%.cc
	$(CXX) $(CPPFLAGS) -c -I lib_u/ $^ -o $@

upac.cmxa: lib_u/binpac.o lib_u/http_pac_fast.o lib_u/http_matcher.o lib_u/upac_stubs.o anypac.cmx
	ocamlmklib -custom -o upac $^

####
#### Compile siftc.cmxa
####


siftc.o: siftc.cpp fs_lib.h
	$(CXX) $(CPPFLAGS) -c $< -o $@ -g -lpcap -DFSLIB='"fs_lib.h"'

siftc_stubs.o: siftc_stubs.c
	$(CC) $(CFLAGS) -c $< -o $@ -g

siftc.cmxa: siftc_stubs.o siftc.o anypac.cmx
	ocamlmklib -custom -o siftc $^

siftc_soap.o: siftc.cpp fs_lib_soap.h
	$(CXX) $(CPPFLAGS) -c $< -o $@ -g -lpcap -DFSLIB='"fs_lib_soap.h"'

siftc_soap.cmxa: siftc_stubs.o siftc_soap.o anypac.cmx
	ocamlmklib -custom -o siftc_soap $^

siftc_dns.o: siftc.cpp fs_lib_dns.h
	$(CXX) $(CPPFLAGS) -c $< -o $@ -g -lpcap -DFSLIB='"fs_lib_dns.h"'

siftc_dns.cmxa: siftc_stubs.o siftc_dns.o anypac.cmx
	ocamlmklib -custom -o siftc_dns $^

####
#### Compile flow.cmxa
####

tcmalloc_stubs.o: tcmalloc_stubs.c
	ocamlc -c $^ -o $@


ML_SOURCES=hashtbl_param.ml ean_std.ml pcregex.ml minreg.ml PCFG.ml ns_types.ml simplify.ml ns_yac.ml ns_lex.ml ruleset.ml tcam.ml decider.ml fdd.ml bdd.ml optimizers.ml regex_dfa.ml nfa.ml ns_parse.ml disj_set.ml d2fa.ml vsdfa.ml ns_run.ml prog_parse.ml arg2.ml genrec.ml prog_parse_vs.ml pcap_parser.ml

FLOWSIFT=$(ML_SOURCES:.ml=.cmx)

ns_yac.ml: ns_yac.mly
	menhir ns_yac.mly

ns_lex.ml: ns_lex.mll
	ocamllex ns_lex.mll

ns_yac.cmi: ns_yac.ml
	ocamlc -annot -g -c ns_yac.mli

ns_yac.cmx: ns_yac.cmi ns_yac.ml
	$(OCAMLOPT) -c ns_yac.ml

ns_yac.cmo: ns_yac.cmi ns_yac.ml
	$(OCAMLC) -c ns_yac.ml

pcap_parser.cmo: pcap_parser.ml
	$(OCAMLC) -c -syntax camlp4o -package bitstring.syntax,bitstring pcap_parser.ml -o pcap_parser.cmo

%.cmx: %.ml
	$(OCAMLOPT) -c $^

%.cmo: %.ml
	ocamlfind ocamlc -package $(PACKAGES) $(OCAMLFLAGS) -c $^

OLIBS = #libocamlviz.cmxa

bench-bpac: bpac.cmxa tcmalloc_stubs.o $(FLOWSIFT) bench.cmx
	$(OCAMLOPT) -package bitstring -linkpkg -I . -cclib -lstdc++ -cclib -lpcre -cclib -ltcmalloc $^ -o $@

bench-upac: upac.cmxa tcmalloc_stubs.o $(FLOWSIFT) bench.cmx
	$(OCAMLOPT) -package bitstring -linkpkg -I . -cclib -lstdc++ -cclib -lpcre -cclib -ltcmalloc $^ -o $@

bench-siftc: siftc.cmxa tcmalloc_stubs.o $(FLOWSIFT) bench.cmx
	$(OCAMLOPT) -package bitstring -linkpkg -I . -cclib -lstdc++ -cclib -lpcre -cclib -ltcmalloc $^ -o $@

bench-siftc-soap: siftc_soap.cmxa tcmalloc_stubs.o $(FLOWSIFT) bench.cmx
	$(OCAMLOPT) -package bitstring -linkpkg -I . -cclib -lstdc++ -cclib -lpcre -cclib -ltcmalloc $^ -o $@

bench-siftc-dns: siftc_dns.cmxa tcmalloc_stubs.o $(FLOWSIFT) bench.cmx
	$(OCAMLOPT) -package bitstring -linkpkg -I . -cclib -lstdc++ -cclib -lpcre -cclib -ltcmalloc $^ -o $@


pcap_parser.cmx: pcap_parser.ml
	ocamlfind ocamlopt $(OCAMLFLAGS) -c -syntax camlp4o -package $(PACKAGES),bitstring.syntax,bitstring pcap_parser.ml -o pcap_parser.cmx

pcap_parser: pcap_parser.ml
	$(OCAMLBUILD) pcap_parser.native
	mv pcap_parser.native pcap_parser

hwrun:
	$(OCAMLBUILD) hwrun.native
	mv hwrun.native hwrun

demo: *.ml
	$(OCAMLBUILD) demo.native

##
## Extraction grammar generation tool
##

#demo.cmx: demo.ml
#	$(OCAMLOPT) -package pcap -c $< -o $@
#
#gen_extr.cmx: gen_extr.ml demo.cmx
#	$(OCAMLOPT) -package lablgtk2 -c $< -o $@
#
#gen_extr: $(FLOWSIFT) demo.cmx gen_extr.cmx
#	$(OCAMLOPT) -package lablgtk2,pcap -linkpkg $^ -o $@
#
demo.cmx: demo.ml
	$(OCAMLOPT) -c $< -o $@

gen_extr.cmx: gen_extr.ml demo.cmx
	$(OCAMLOPT) -package lablgtk2 -c $< -o $@

gen_extr: $(FLOWSIFT) demo.cmx gen_extr.cmx
	$(OCAMLOPT) -package lablgtk2 -linkpkg $^ -o $@

FlowSifter: gen_extr
	mv gen_extr dist/FlowSifter

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

MODES=bpac upac #siftc
MEM_MODES=bpac flow null

bench-all: $(patsubst %,bench-%,$(MODES))

threadlog-all: $(patsubst %, %.threadlog, $(MEM_MODES))

perf-all: $(patsubst %, %.perf, $(MODES))

perf2-all: $(patsubst %, %.perf2, $(MODES))

%.ml.d: %.ml
	ocamlfind ocamldep -package bitstring.syntax -syntax camlp4o $< > $@

include $(ML_SOURCES:.ml=.ml.d)
include bench.ml.d pcap_parser.ml.d ns_compile.ml.d

ns_yac.cmi: ns_types.ml PCFG.cmi

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
	99w5-monday 99w5-tuesday 	   	99w5-thursday 99w5-friday
#99w5-wednesday breaks something in sift related to resume during function


HEADER = "runid\tparser\titers\ttime\tgbit\tgbps\tmem\tflows\tevents\tparsed\tdropped"
COUNT ?= 1


### HTTP TRACES FROM LL
rundata: bench-bpac bench-upac bench-siftc
	@mkdir -p old/
	-mv -b $@ old/$@.bkp
	echo -e $(HEADER) > $@
	time for a in $(RUNS); do \
	    ./bench-bpac     -n $(COUNT) ~/traces/http/use/$$a* | tee -a $@; \
	    ./bench-upac  -p -n $(COUNT) ~/traces/http/use/$$a* | tee -a $@; \
	    ./bench-siftc -p -n $(COUNT) ~/traces/http/use/$$a* | tee -a $@; \
	done

### HTTP TRACES FROM WEB SPIDERING
harvdata: bench-bpac bench-upac bench-siftc
	@mkdir -p old/
	-mv -b $@ old/$@.bkp
	echo -e $(HEADER) > $@
	time for a in ~/traces/http/use/harvest*; do \
	    ./bench-bpac     -n $(COUNT) $$a | tee -a $@; \
	    ./bench-upac  -p -n $(COUNT) $$a | tee -a $@; \
	    ./bench-siftc -p -n $(COUNT) $$a | tee -a $@; \
	done

harvest.pdf: harvest.R harvdata
	R CMD BATCH $<

### HTTP TRACES FROM GENERAL USAGE
dumpdata: bench-bpac bench-upac bench-siftc
	@mkdir -p old/
	-mv -b $@ old/$@.bkp
	echo -e $(HEADER) > $@
	time for a in ~/traces/http/use/norige-dump*; do \
	    ./bench-bpac     -n $(COUNT) $$a | tee -a $@; \
	    ./bench-upac  -p -n $(COUNT) $$a | tee -a $@; \
	    ./bench-siftc -p -n $(COUNT) $$a | tee -a $@; \
	done

norigedump.pdf: norigedump.R dumpdata
	R CMD BATCH $<

### UNFILTERED NORIGE TRACES
dumpdata2: bench-bpac bench-upac bench-siftc
	@mkdir -p old/
	-mv -b $@ old/$@.bkp
	echo -e $(HEADER) > $@
	time for a in ~/traces/fullpayload/Norige/dump.2011*; do \
	    ./bench-bpac     -n $(COUNT) $$a | tee -a $@; \
	    ./bench-upac  -p -n $(COUNT) $$a | tee -a $@; \
	    ./bench-siftc -p -n $(COUNT) $$a | tee -a $@; \
	done

norigedump2.pdf: norigedump2.R dumpdata2
	R CMD BATCH $<


### SIMULATED XML_RPC TRACES
FLOWS ?= 10000
rectest: bench-siftc-soap
	@mkdir -p old/
	-mv -b $@ old/$@.bkp
	( echo -e $(HEADER); \
	time for a in 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16; do \
	    ./bench-siftc-soap --seed 230 -n $(COUNT) -g $$a $(FLOWS) -x e-soap.ca; \
	    ./bench-siftc-soap --seed 231 -n $(COUNT) -g $$a $(FLOWS) -x e-soap.ca; \
	    ./bench-siftc-soap --seed 232 -n $(COUNT) -g $$a $(FLOWS) -x e-soap.ca; \
	    ./bench-siftc-soap --seed 233 -n $(COUNT) -g $$a $(FLOWS) -x e-soap.ca; \
	    ./bench-siftc-soap --seed 234 -n $(COUNT) -g $$a $(FLOWS) -x e-soap.ca; \
	    ./bench-siftc-soap --seed 235 -n $(COUNT) -g $$a $(FLOWS) -x e-soap.ca; \
	    ./bench-siftc-soap --seed 236 -n $(COUNT) -g $$a $(FLOWS) -x e-soap.ca; \
	    ./bench-siftc-soap --seed 237 -n $(COUNT) -g $$a $(FLOWS) -x e-soap.ca; \
	    ./bench-siftc-soap --seed 238 -n $(COUNT) -g $$a $(FLOWS) -x e-soap.ca; \
	    ./bench-siftc-soap --seed 239 -n $(COUNT) -g $$a $(FLOWS) -x e-soap.ca; \
	done ) | tee $@

rectest2: bench-siftc-soap
	@mkdir -p old/
	-mv -b $@ old/$@.bkp
	( echo -e $(HEADER); \
	time for a in 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16; do \
	    ./bench-siftc-soap --seed 230 -n $(COUNT) -g $$a $(FLOWS) -x e-soap2.ca; \
	    ./bench-siftc-soap --seed 231 -n $(COUNT) -g $$a $(FLOWS) -x e-soap2.ca; \
	    ./bench-siftc-soap --seed 232 -n $(COUNT) -g $$a $(FLOWS) -x e-soap2.ca; \
	    ./bench-siftc-soap --seed 233 -n $(COUNT) -g $$a $(FLOWS) -x e-soap2.ca; \
	    ./bench-siftc-soap --seed 234 -n $(COUNT) -g $$a $(FLOWS) -x e-soap2.ca; \
	    ./bench-siftc-soap --seed 235 -n $(COUNT) -g $$a $(FLOWS) -x e-soap2.ca; \
	    ./bench-siftc-soap --seed 236 -n $(COUNT) -g $$a $(FLOWS) -x e-soap2.ca; \
	    ./bench-siftc-soap --seed 237 -n $(COUNT) -g $$a $(FLOWS) -x e-soap2.ca; \
	    ./bench-siftc-soap --seed 238 -n $(COUNT) -g $$a $(FLOWS) -x e-soap2.ca; \
	    ./bench-siftc-soap --seed 239 -n $(COUNT) -g $$a $(FLOWS) -x e-soap2.ca; \
	done ) | tee $@

figures: rundata rectest memory.R
	R CMD BATCH memory.R

#	    ./bench-bpac-dns     -n $(COUNT) $$a | tee -a $@;
#	    ./bench-upac-dns  -p -n $(COUNT) $$a | tee -a $@;
DNSCOUNT=100
### DNS TRACES FROM LL
dnsdata:  bench-bpac bench-upac bench-siftc-dns
	@mkdir -p old/
	-mv -b $@ old/$@.bkp
	echo -e $(HEADER) > $@
	time for a in ~/traces/ll/*/dns*; do \
	    echo $$a;\
	    ./bench-siftc-dns -n $(DNSCOUNT) $$a | tee -a $@; \
	done

outliers: bench-upac
	./bench-upac ~/traces/http/use/98w3-wednesday.pcap

#	./bench-bpac -f -d ~/traces/http/use/harvest.pcap* > log; tail -n 1 log
#	./bench-upac -f -d ~/traces/http/use/harvest.pcap* >> log; tail -n 1 log
#	./bench-siftc -f -d ~/traces/http/use/harvest.pcap* >> log; tail -n 1 log
#	./bench-bpac -f -d ~/traces/http/use/harvest1.pcap* >> log; tail -n 1 log
#	./bench-upac -f -d ~/traces/http/use/harvest1.pcap* >> log; tail -n 1 log
#	./bench-siftc -f -d ~/traces/http/use/harvest1.pcap* >> log; tail -n 1 log

diff-test: bench-bpac bench-upac bench-siftc
	./bench-bpac -f -d ~/traces/http/use/98w3-wednesday.pcap > log; tail -n 1 log
	./bench-upac -f -d ~/traces/http/use/98w3-wednesday.pcap >> log; tail -n 1 log
	./bench-siftc -f -d ~/traces/http/use/98w3-wednesday.pcap >> log; tail -n 1 log
	./bench-bpac -f -d ~/traces/http/use/99w2-tuesday.inside* >> log; tail -n 1 log
	./bench-upac -f -d ~/traces/http/use/99w2-tuesday.inside* >> log; tail -n 1 log
	./bench-siftc -f -d ~/traces/http/use/99w2-tuesday.inside* >> log; tail -n 1 log

ns_compile.native: $(FLOWSIFT) ns_compile.cmx
	$(OCAMLOPT) $^ -linkpkg -o $@

fs_lib.h: ns_compile.native http.pro extr.ca
	./ns_compile.native http.pro extr.ca "$@"

fs_lib_soap.h: ns_compile.native http.pro e-soap2.ca
	./ns_compile.native http.pro e-soap2.ca "$@"

fs_lib_dns.h: ns_compile.native dns.pro dns.ext
	./ns_compile.native dns.pro dns.ext "$@"

fs: fs_main.cpp fs_lib.h
	g++ $< -o $@ $(CPPFLAGS) -lpcap -DFSLIB='"fs_lib.h"'

fs_single: fs_single.cpp fs_lib.h
	g++ $< -o $@ $(CPPFLAGS)

fs.p: fs_main.cpp fs_lib.h
	g++ $< -o $@ $(CPPFLAGS) -lpcap -pg

fs-test: fs
	./fs ~/traces/http/use/98w2-monday.pcap
	./fs ~/traces/http/use/99w5-friday.outside.tcpdump.gz.pcap

dyck_lib.h: dyck.pro dyck.ext ns_compile.native
	./ns_compile.native dyck.pro dyck.ext "$@"

fs_single_dyck: fs_single.cpp dyck_lib.h
	cp dyck_lib.h fs_lib.h
	g++ $< -o $@ $(CPPFLAGS)

dyck-test: fs_single_dyck
	./fs_single_dyck dyckTest.txt

tcam.native:
	$(OCAMLBUILD) -no-hygiene tcam.native
