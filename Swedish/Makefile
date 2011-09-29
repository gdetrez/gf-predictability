PROG = Swedish
DATA = nouns.csv
BUILDDIR = build

ifdef PROFILE
PROF_FLAGS = +RTS -p -hc -i0.001 -s${PROG}.summary -RTS
else
PROF_FLAGS = 
endif

all: run

run: LOGFILE = "summary-`date +'%Y-%m-%d:%H:%M'`"
run: ${DATA} ${PROG}
	./${PROG} --summary ${LOGFILE} ${PROF_FLAGS}

test: LOGFILE = "test-summary-`date +'%Y-%m-%d:%H:%m'`"
test: ${DATA} ${PROG}
	./${PROG} --test --summary ${LOGFILE} ${PROF_FLAGS}

.SECONDARY: ${DATA}

%.csv: saldom-%.xsl saldom.xml
	xsltproc -o $@ $^

profiling: ${PROG}
	time ./${PROG} +RTS -p -hc -i 0.001 -s${PROG}.summary

${PROG}: ${PROG}.hs | ${BUILDDIR}
	ghc --make -prof -caf-all -auto-all \
	    -hidir ${BUILDDIR}\
	    -odir ${BUILDDIR}\
	    -fforce-recomp\
	    -i../lib/src $<

${BUILDDIR}:
	mkdir ${BUILDDIR}

clean:
	-rm -rf ${BUILDDIR}
	-rm ${PROG}
	-rm *.csv
	-rm ${PROG} $(addprefix ${PROG}, .hi .hp .o .log .prof .ps .aux .summary)
