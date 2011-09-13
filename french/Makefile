all: run

run: commonNouns.csv
	runghc -i../lib/src Run.hs

commonNouns.csv: Morphalou-2.0-commnNouns.xsl Morphalou-2.0.xml
	xsltproc -o $@ $^