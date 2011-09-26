all: nouns.csv
	runghc -i../lib/src Swedish.hs

test: nouns.csv
	runghc -i../lib/src Swedish.hs --test

%.csv: saldom-%.xsl saldom.xml
	xsltproc -o $@ $^
