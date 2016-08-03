# Copyright 2016 John J Foerch. All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
#    1. Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#
#    2. Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in
#       the documentation and/or other materials provided with the
#       distribution.
#
# THIS SOFTWARE IS PROVIDED BY JOHN J FOERCH ''AS IS'' AND ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL JOHN J FOERCH OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
# BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
# OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
# ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
