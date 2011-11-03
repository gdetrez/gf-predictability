EXPERIMENTS = nouns adjectives adjectives_nocomp verbs verbs_inf
include ../common.mk

%.csv: saldom-%.xsl saldom.xml
	xsltproc -o $@ $^

adjectives_nocomp.csv: | adjectives.csv
	ln -s $| $@

verbs_inf.csv: | verbs.csv
	ln -s $| $@
