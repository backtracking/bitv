#########################################
# Configuration part : where to install
#########################################

LIBDIR = /usr/local/lib/ocaml

#########################################
# End of configuration part
#########################################

CMO = bitv.cmo
CMI = $(CMO:.cmo=.cmi)
MLI = $(CMO:.cmo=.mli)
CMX = $(CMO:.cmo=.cmx)
OBJS = $(CMO:.cmo=.o)

BYTEFILES = $(CMO) $(CMI) $(MLI)
OPTFILES = $(CMX) $(OBJS) 

all: byte opt

byte: $(CMO)
opt:  $(CMX)

install: install-byte install-opt

install-byte:
	cp -f $(BYTEFILES) $(LIBDIR)

install-opt:
	cp -f $(OPTFILES) $(LIBDIR)

# l.p.
######

doc: bitv.dvi

bitv.tex: bitv.mli
	ocamlweb -s --no-index bitv.mli -o $@

bitv.dvi: bitv.tex
	latex bitv

bitv.ps.gz: bitv.dvi
	dvips bitv.dvi -o
	gzip --best bitv.ps

# export
########

MAJORVN=0
MINORVN=1
VERSION=$(MAJORVN).$(MINORVN)
NAME=bitv-$(VERSION)

SOURCES = bitv.mli bitv.ml Makefile .depend README COPYING LGPL

FTP=$(HOME)/WWW/ftp/ocaml/bitv

export:: bitv.ps.gz
	mkdir -p export/$(NAME)
	cp $(SOURCES) export/$(NAME)
	(cd export; tar cf $(NAME).tar $(NAME); \
	gzip -f --best $(NAME).tar)
	cp -f README COPYING LGPL $(MLI) export/$(NAME).tar.gz $(FTP)
	cp -f bitv.ps.gz $(FTP)

# generic rules :
#################

CAMLC = ocamlc
CAMLCOPT = ocamlopt
FLAGS=

.SUFFIXES: .mli .ml .cmi .cmo .cmx

.mli.cmi:
	$(CAMLC) -c $(FLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(FLAGS) $<

.ml.o:
	$(CAMLCOPT) -c $(FLAGS) $<

.ml.cmx:
	$(CAMLCOPT) -c $(FLAGS) $<


# clean and depend
##################

clean:
	rm -f *~ *.cm[iox] *.o
	rm -f bitv.tex *.aux *.log *.dvi *.ps *.ps.gz

depend:
	rm -f .depend
	ocamldep *.mli *.ml > .depend

include .depend

