OCAML_OPTS=-w,+a-3-4-32..39,-warn-error,+a,-strict-sequence,-noautolink

PROGRAM=caradoc

ML= $(shell find $(pwd) -name "*.ml")
MLI= $(shell find $(pwd) -name "*.mli")
INDENTML= $(ML:.ml=.indentml)
INDENTMLI= $(MLI:.mli=.indentmli)

PDFPOS= $(shell find test_files/positive -name "*.pdf")
PDFNEG= $(shell find test_files/negative -name "*.pdf")
PDFCLEANUP= $(shell find test_files/cleanup -name "*.pdf")
TRAILERNEG= $(shell find test_files/negative -name "*.trailer")
XREFNEG= $(shell find test_files/negative -name "*.xref")
CLEANNEG= $(shell find test_files/negative -name "*.clean")
TYPESNEG= $(shell find test_files/negative -name "*.types")
TESTSTATS= $(PDFPOS:.pdf=.teststats) $(PDFNEG:.pdf=.teststats)
TESTSTATS_STRICT= $(PDFPOS:.pdf=.teststats_strict) $(PDFNEG:.pdf=.teststats_strict)
TESTTRAILER= $(PDFPOS:.pdf=.testtrailer) $(TRAILERNEG:.trailer=.testtrailer)
TESTXREF= $(PDFPOS:.pdf=.testxref) $(XREFNEG:.xref=.testxref)
TESTCLEANUP= $(PDFPOS:.pdf=.testcleanup) $(CLEANNEG:.clean=.testcleanup)
TESTCLEANUP_OPTIONS= $(PDFCLEANUP:.pdf=.testcleanup_options)
TESTTYPES= $(PDFPOS:.pdf=.testtypes) $(TYPESNEG:.types=.testtypes)
TESTS= $(TESTSTATS) $(TESTSTATS_STRICT) $(TESTTRAILER) $(TESTXREF) $(TESTCLEANUP) $(TESTCLEANUP_OPTIONS) $(TESTTYPES)

BINDIR ?= /usr/bin


all: $(PROGRAM)

test: unit.byte unit.native $(TESTS)
	./unit.byte
	./unit.native

indent: $(INDENTML) $(INDENTMLI)


$(PROGRAM): main.native
	cp -P main.native $(PROGRAM)

debug-menhir:
	ocamlbuild -use-ocamlfind -menhir "menhir --dump --trace" src/main.native

main.native:
	ocamlbuild -cflags $(OCAML_OPTS) -use-ocamlfind src/main.native
	rm -f $@
	ln -s _build/src/$@

main.byte:
	ocamlbuild -cflags $(OCAML_OPTS) -use-ocamlfind src/main.byte
	rm -f $@
	ln -s _build/src/$@

unit.native:
	ocamlbuild -cflags $(OCAML_OPTS) -build-dir "_build.test" -tag-line "true: package(oUnit)" -use-ocamlfind test/unit.native
	rm -f $@
	ln -s _build.test/test/$@

unit.byte:
	ocamlbuild -cflags $(OCAML_OPTS) -build-dir "_build.test" -tag-line "true: package(oUnit)" -use-ocamlfind test/unit.byte
	rm -f $@
	ln -s _build.test/test/$@

%.indentml: %.ml
	ocp-indent -i $<

%.indentmli: %.mli
	ocp-indent -i $<

tmpfolder:
	mkdir -p tmp

%.teststats: %.pdf %.stats $(PROGRAM)
	./$(PROGRAM) stats $< 2>&1 | cmp -s $*.stats -

%.teststats_strict: %.pdf %.stats $(PROGRAM)
	./$(PROGRAM) stats --strict $< 2>&1 | cmp -s $*.stats_strict -

%.testtrailer: %.pdf %.trailer $(PROGRAM)
	./$(PROGRAM) trailer --sort-dicts $< | cmp -s $*.trailer -

%.testxref: %.pdf %.xref $(PROGRAM) tmpfolder
	./$(PROGRAM) xref $< | cmp -s $*.xref -;
	./$(PROGRAM) extract --xref tmp/xref $< > /dev/null;
	cmp -s $*.xref tmp/xref

%.testcleanup: %.pdf %.clean $(PROGRAM) tmpfolder
	./$(PROGRAM) cleanup --merge-content-streams --out tmp/clean $<;
	cmp -s $*.clean tmp/clean
	./$(PROGRAM) cleanup --merge-content-streams --out tmp/clean $*.clean;
	cmp -s $*.clean tmp/clean

%.testcleanup_options: %.pdf %.options %.clean $(PROGRAM) tmpfolder
	./$(PROGRAM) cleanup --merge-content-streams --options $*.options --out tmp/clean $<;
	cmp -s $*.clean tmp/clean
	./$(PROGRAM) cleanup --merge-content-streams --options $*.options --out tmp/clean $*.clean;
	cmp -s $*.clean tmp/clean

%.testtypes: %.pdf %.types $(PROGRAM) tmpfolder
	./$(PROGRAM) extract --types tmp/types $< > /dev/null;
	cmp -s $*.types tmp/types


.PHONY: main.native unit.native main.byte unit.byte all test clean indent

clean:
	rm -f $(PROGRAM) *.native *.byte
	rm -Rf _build _build.test
	rm -f oUnit-anon.cache
	rm -Rf tmp

install: $(PROGRAM)
	install -D -m 0755 "$(PROGRAM)" "$(BINDIR)/$$(basename $(PROGRAM))"
