MODULES=reader features finder main catalog publisher
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind -plugin-tag \
	'package(bisect_ppx-ocamlbuild)'
PKGS=ounit2,ANSITerminal,core,yojson,str,lwt,cohttp-lwt-unix,cohttp

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag debug $(TEST) && ./$(TEST)

bisect-test:
	BISECT_COVERAGE=YES $(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

bisect: clean bisect-test
	bisect-ppx-report html

read:
	$(OCAMLBUILD) -tag 'debug' $(MAIN) && OCAMLRUNPARAM=b ./$(MAIN)

zip:
	zip project.zip *.ml* *.json *.sh _tags .merlin .ocamlformat .ocamlinit books/*/* *.md .json flashcard/* notes/*/* originals/* sticky_notes/*/* LICENSE Makefile	

docs: docs-public

docs-public: build
	mkdir -p _doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d _doc.public $(MLIS)

clean:
	ocamlbuild -clean
	rm -rf _doc.public _doc.private project.zip _coverage bisect*.coverage