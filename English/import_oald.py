import re
import sys
import csv
from optparse import OptionParser

parser = OptionParser()
parser.add_option("-f", "--file", dest="filename",
                  help="write output to FILE", metavar="FILE")
parser.add_option( "--nouns", const="N",
                  action="store_const", dest="PoS", default=None,
                  help="Extract nouns")
parser.add_option("--verbs", const="V",
                  action="store_const", dest="PoS", default=None,
                  help="Extract verbs")
parser.add_option("--adjectives", const="A",
                  action="store_const", dest="PoS", default=None,
                  help="Extract verbs")
(options, args) = parser.parse_args()

def list2dict(l):
    d = {}
    for k,v in l:
        d[k] = v
    return d

if __name__ == "__main__":
    if options.PoS is None:
        print "Please specify what Part of Speach to extract"
        sys.exit(1)
        
    if len(args) != 1:
        print "Please specify one and only one source file"
        sys.exit(1)
    regex = re.compile(r"\s*([A-Z]+):\"([\w']+|-)\"")

    writer = csv.writer(sys.stdout, lineterminator = '\n')

    with open(args[0]) as source:
        for line in source:
            line = line.replace("\\'", "'")
            
            l = regex.findall(line)
            d = list2dict(l)
            if options.PoS == 'N' and d.has_key("NN"):
                try:
                    writer.writerow([d["NN"] + "_N", d["NN"], d["NNS"]])
                except KeyError:
                    print >> sys.stderr, "Error in line: %s" % line.strip()
            if options.PoS == 'V' and d.has_key("VB"):
                try:
                    writer.writerow([d["VB"] + "_V", d["VB"], d["VBZ"],
                                     d["VBN"], d["VBG"], d["VBD"]])
                except KeyError:
                    print >> sys.stderr, "Error in line: %s" % line.strip()
            if options.PoS == 'A' and d.has_key("JJ"):
                try:
                    writer.writerow([d["JJ"] + "_A", d["JJ"], d["JJR"],
                                     d["JJS"]])
                except KeyError:
                    print >> sys.stderr, "Error in line: %s" % line.strip()
