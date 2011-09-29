PROG = French
DATA = nouns.csv adjectives.csv verbs.csv

include ../common.mk

%.csv: %.xsl Morphalou-2.0.xml
	xsltproc -o $@ $^
