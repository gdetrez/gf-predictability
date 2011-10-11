DATE=`date +'%Y%m%d%H%M'`
DATA = verbs.csv nouns.csv adjectives.csv

.SECONDARY: ${DATA}

all: verbs.summary nouns.summary adjectives.summary

test: verbs-test.summary nouns-test.summary adjectives-test.summary

%.summary: %.hs %.csv
	time runghc -i../lib/src/ $*.hs\
	     --lexicon=$*.csv\
	     --summary=$@\
	     --results=$*.results

%-test.csv: %.csv
	head -n100 $< > $@

%-test.summary: %.hs %-test.csv
	time runghc -i../lib/src/ $*.hs\
	     --lexicon=$*-test.csv\
	     --summary=$@\
	     --results=$*-test.results

run-%: %.csv
	time runghc ${FLAGS} $*.hs

clean:
	-rm *.csv
	-rm *.summary
	-rm *.results
