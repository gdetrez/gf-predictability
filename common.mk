DATE := $(shell date +'%Y-%m-%d_%H:%M')
DATA = $(addsuffix .csv,${EXPERIMENTS})
SUMMARIES = $(addsuffix .summary,${EXPERIMENTS})

SUMMARY = "${DATE}.summary"

all: ${EXPERIMENTS}
	echo ${SUMMARY}

.SECONDARY: ${DATA}

test: $(addsuffix .test,${EXPERIMENTS})

%: %.hs %.csv
	time runghc -i../lib/src/ $*.hs\
	     --lexicon=$*.csv\
	     --summary=${SUMMARY}\
	     --results=$*.results

%.summary: %.hs %.csv
	time runghc -i../lib/src/ $*.hs\
	     --lexicon=$*.csv\
	     --summary=$@\
	     --results=$*.results

%.test.csv: %.csv
	head -n100 $< > $@

%.test.summary: %.hs %.test.csv
	time runghc -i../lib/src/ $*.hs\
	     --lexicon=$*.test.csv\
	     --summary=$@\
	     --results=$*-test.results

clean:
	-rm *.csv
	-rm *.summary
	-rm *.results
