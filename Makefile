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

# export
########

MAJORVN=0
MINORVN=1
VERSION=$(MAJORVN).$(MINORVN)
NAME=bitv-$(VERSION)

SOURCES = bitv.mli bitv.ml Makefile

FTP=$(HOME)/WWW/ftp/soft/bitv

export:
	mkdir -p $(NAME)
	cp $(SOURCES) $(NAME)
	tar cf $(NAME).tar $(NAME)
	gzip -f --best $(NAME).tar
	cp -f README COPYING LGPL CHANGES $(NAME).tar.gz $(FTP)

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

depend:
	rm -f .depend
	ocamldep *.mli *.ml > .depend

include .depend

