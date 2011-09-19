all: run

run: commonNouns.csv
	runghc -i../lib/src Run.hs

test: commonNouns.csv
	runghc -i../lib/src Run.hs --test

commonNouns.csv: Morphalou-2.0-commnNouns.xsl Morphalou-2.0.xml
	xsltproc -o $@ $^