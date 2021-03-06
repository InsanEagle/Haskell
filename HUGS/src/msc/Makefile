# Generated automatically from Makefile.in by configure.
# --------------------------------------------------------------------------
# Makefile for Hugs
#
# The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
# Yale Haskell Group, and the Oregon Graduate Institute of Science and
# Technology, 1994-1999, All rights reserved.  It is distributed as
# free software under the license in the file "License", which is
# included in the distribution.
# --------------------------------------------------------------------------

# Targets:
#
# <default>:   	 make hugs.exe and runhugs.exe
# install:	 make and install programs/libraries
# hugs.exe:    	 make minimal working system
# runhugs.exe:	 make batch-mode version of Hugs
# clean:       	 delete files not required in running system
# distclean:     delete files that can be regenerated using C compiler
# veryclean:     delete all machine generated files 
#                  (you need perl, bison/yacc, etc to rebuild these files)
# check:	 run regression tests
# TAGS:          build emacs TAGS table

# This rule goes first to make it the default choice
default		:: hugs.exe runhugs.exe

CC		= cl /nologo
CFLAGS		= -ML 
OPTFLAGS	= -O2

LD		= ld
LDFLAGS    	=   
DLL_FLAGS	= /LD
PIC_FLAGS	= 
LIBS		=  kernel32.lib advapi32.lib

YACC		= byacc
RM		= -del
CP		= copy



.SUFFIXES	:
.SUFFIXES	: .c .h .obj
HFILES		= prelude.h config.h options.h storage.h connect.h \
		  errors.h command.h server.h
CFILES		= hugs.c storage.c input.c static.c type.c subst.c \
		  output.c compiler.c machine.c interp.c builtin.c \
		  server.c ffi.c
INCFILES	= parser.c preds.c bignums.c scc.c machdep.c timer.c \
		  printer.c iomonad.c interns.c stmonad.c array.c
YFILES		= parser.y
SOURCES		= $(HFILES) $(CFILES) $(INCFILES) $(YFILES) 
OBJECTS		= storage.obj input.obj static.obj type.obj subst.obj \
          compiler.obj plugin.obj builtin.obj machine.obj output.obj ffi.obj
IOBJECTS	= hugs.obj $(OBJECTS)
PRELUDE		= config.h options.h prelude.h

################################################################
# Hugs interpreter and standalone evaluator
################################################################

hugs.exe	: $(IOBJECTS)  
		  $(CC) $(LDFLAGS) $(IOBJECTS)  $(LIBS) -o hugs.exe 
		  

SERVER_OBJECTS	= server.obj $(OBJECTS) 
runhugs.obj	: $(PRELUDE) hugs.c machdep.c timer.c runhugs.c
runhugs.exe	: runhugs.obj $(SERVER_OBJECTS)
		  $(CC) $(LDFLAGS) runhugs.obj $(SERVER_OBJECTS) $(LIBS) -o runhugs.exe

################################################################
# Clean, distclean, veryclean, TAGS
################################################################

clean		::
		  $(RM) *.o
		  $(RM) *.O
		  $(RM) *.obj
		  $(RM) *.OBJ
		  $(RM) *.LIB
		  $(RM) *.DEF
		  $(RM) *.RES
		  $(RM) *.EXP
		  $(RM) *.ILK
		  $(RM) *.PDB
		  $(RM) *.TD2
		  $(RM) *.MAP
		  $(RM) *.CSM
		  $(RM) *.TR2
		  $(RM) *.DSW
		  $(RM) *.aux
		  $(RM) *.hp
distclean	:: clean
distclean	::
		  $(RM) hugs.exe
		  $(RM) runhugs.exe
		  $(RM) *.pdf
		  $(RM) TAGS
		  $(RM) *~
veryclean	:: distclean
veryclean	::
		  $(RM) options.h
		  $(RM) config.h
TAGS		::
		  etags *.[ych]

################################################################
# Dependencies
################################################################

.c.obj	:
		  $(CC) -c $(CFLAGS) $(OPTFLAGS) $<

# These are compiled with less optimisation to avoid optimisation bugs in
# certain compilers.  This may be overly conservative on some compilers.
compiler.obj	: compiler.c
		  $(CC) -c $(CFLAGS) compiler.c
static.obj	: static.c
		  $(CC) -c $(CFLAGS) static.c

# parser.c	: parser.y
# 		  -$(YACC) parser.y
# 		  mv y.tab.c parser.c
# veryclean	::
# 		  $(RM) parser.c

server.obj	: $(PRELUDE) storage.h connect.h errors.h version.h \
		  command.h machdep.c timer.c hugs.c server.c server.h
hugs.obj	: $(PRELUDE) storage.h connect.h errors.h version.h \
		  command.h machdep.c timer.c
ffi.obj		: $(PRELUDE) storage.h connect.h errors.h
storage.obj	: $(PRELUDE) storage.h connect.h errors.h
input.obj	: $(PRELUDE) storage.h connect.h errors.h \
		  parser.c command.h
subst.obj	: $(PRELUDE) storage.h connect.h errors.h subst.h
static.obj	: $(PRELUDE) storage.h connect.h errors.h subst.h \
           scc.c
type.obj	: $(PRELUDE) storage.h connect.h errors.h subst.h \
		  scc.c preds.c
output.obj	: $(PRELUDE) storage.h connect.h errors.h
compiler.obj	: $(PRELUDE) storage.h connect.h errors.h 
machine.obj	: $(PRELUDE) storage.h connect.h errors.h
plugin.obj	: $(PRELUDE) storage.h connect.h errors.h
builtin.obj	: $(PRELUDE) storage.h connect.h errors.h \
		  bignums.c printer.c iomonad.c interns.c stmonad.c array.c

################################################################
# Regression tests (none supplied)
################################################################

check		:


################################################################
# End of Makefile
################################################################
