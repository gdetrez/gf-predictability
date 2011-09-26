CSVFILES = nouns.csv adjectives.csv verbs.csv
SCRIPT = French.hs
GHC_OPTS = -i../lib/src 

all: run

run: ${CSVFILES}
	runghc ${GHC_OPTS} ${SCRIPT} --dump-results dump.txt

test: ${CSVFILES}
	runghc ${GHC_OPTS} ${SCRIPT} --test

%.csv: %.xsl Morphalou-2.0.xml
	xsltproc -o $@ $^

clean:
	-rm ${CSVFILES}
	-rm *.txt
	-rm *.log