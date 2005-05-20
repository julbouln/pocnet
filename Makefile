OCAMLMAKEFILE = OCamlMakefile

PACKS=pocvalue unix cryptokit num str

LIBINSTALL_FILES=*.cmi *.cmo *.cmx *.a pocnet.cma pocnet.cmxa

SOURCES = net_socket.ml net_message.ml net_conn.ml net_server.ml

THREADS=true

RESULT  = pocnet

OCAMLDOC=ocamlfind ocamldoc -package "$(PACKS)"
DOC_FILES=$(SOURCES)

all : ncl bcl

include $(OCAMLMAKEFILE)