EXPERIMENTS = verbs nouns adjectives

include ../common.mk

verbs.csv: oald_penn_morph_lists.txt
	python import_oald.py --verbs $< > $@

nouns.csv: oald_penn_morph_lists.txt
	python import_oald.py --nouns $< > $@

adjectives.csv: oald_penn_morph_lists.txt
	python import_oald.py --adjectives $< > $@



