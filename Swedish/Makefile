include ../common.mk

%.csv: saldom-%.xsl saldom.xml
	xsltproc -o $@ $^

all: adjectives-nocomp.summary

adjectives-nocomp.summary: adjectives_nocomp.hs adjectives.csv
	time runghc -i../lib/src/ adjectives_nocomp.hs\
	     --lexicon=adjectives.csv\
	     --summary=adjectives-nocomp.summary\
	     --results=adjectives-nocomp.results
