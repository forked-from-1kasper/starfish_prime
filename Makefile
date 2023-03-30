BINARY = prime
SRCDIR = src

mosml: $(BINARY)

mlton:
	$(MLTON)/bin/mlton prime.mlb

mlkit:
	$(MLKIT)/bin/mlkit -o $(BINARY) prime.mlb

.depend: mosml.sml
	$(MOSML)/bin/mosml -quietdec mosml.sml > .depend

clean:
	find . -type f -name '*.ui' -exec rm -f "{}" +
	find . -type f -name '*.uo' -exec rm -f "{}" +
	find . -type d -name MLB -exec rm -rf "{}" +
	rm -f $(BINARY) .depend

include .depend