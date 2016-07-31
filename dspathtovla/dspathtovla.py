
import os
import sys
import argparse
import csv

line_type = "P"

def write_other_line (x, y, z):
    global line_type
    print "{} {} {} {} {}".format(line_type, x, y, z, 1.0)

def write_first_line (x, y, z):
    global write_line, line_type
    write_other_line(x, y, z)
    line_type = "L"
    write_line = write_other_line

write_line = write_first_line

def csvfile_to_vla (filename):
    print "set comment {}".format(filename)
    print """set filetype NEW
set coordsys LEFT
set defaultdraw STELLAR
set filecontent LINES
set parametric NON_PARAMETRIC
set depthcue 0"""
    with open(filename, 'rb') as csvfile:
        reader = csv.reader(csvfile)
        ## first row, first field is filetype.
        ##
        ##  'DII' - lightyears (X, Y, Z) => (Y, -X, -Z) for DII systems, otherwise (X, Y, Z)
        ##
        ## other fields ignored
        ##

        ## Face,TRUE or Face,FALSE (not needed)

        ## Date,TRUE = Julian day dates
        ## Date,FALSE = Relative day dates

        row1 = next(reader)
        if row1[0] == "DII":
            ## coords are in lightyears, and may need conversion to
            ## left-handed (Y, -X, -Z)
            pass

        for row in reader:
            if row[0] != "Node Data":
                continue
            [_, time, x, y, z, h, p, r] = row
            write_line(x, z, y)

parser = argparse.ArgumentParser()
parser.add_argument("dspath")
args = parser.parse_args()
csvfile_to_vla(args.dspath)
