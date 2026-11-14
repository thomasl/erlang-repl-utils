# Build the beam files in this directory.  ~/.erlang loads them from
# here (see .erlang).
#
#   make                 compile
#   make clean           remove the beam files
#   make export-default  copy the beam files to ~/defaults, if desired

ERLC ?= erlc
ERLC_OPTS ?= +debug_info

SRC := $(wildcard *.erl)
OBJ := $(SRC:.erl=.beam)

.PHONY: all clean export-default

all: $(OBJ)

%.beam: %.erl
	$(ERLC) $(ERLC_OPTS) $<

clean:
	rm -f *.beam

export-default: all
	mkdir -p ~/defaults
	cp *.beam ~/defaults
