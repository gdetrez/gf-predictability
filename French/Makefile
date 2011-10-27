EXPERIMENTS = nouns verbs adjectives
include ../common.mk

%.csv: %.xsl Morphalou-2.0.xml
	xsltproc -o $@ $^
