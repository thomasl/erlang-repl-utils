# Note: we just dump the defaults in this directory, then 
#   make export-default
# to put them in the ~/defaults directory, if desired

ERLC=erlc
ERLC_OPTS=+nowarn_unused_vars +export_all

SRC := $(wildcard ./*.erl)
INC := $(wildcard ./*.hrl)
OBJ := $(SRC:./%.erl=./%.beam)

%.beam: %.erl
	$(ERLC) $(ERLC_OPTS) $<

all:	$(OBJ)

clean:
	rm -f *.beam

export-default:
	cp *.beam ~/defaults


