include ../common.mk

%.csv: oald_penn_morph_lists.txt
	python import_oald.py --$* $< > $@
