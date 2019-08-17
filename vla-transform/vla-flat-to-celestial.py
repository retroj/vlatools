
import os, sys
import math
import re

cos = math.cos
sin = math.sin

## constants
##
LY = 9.460730472580800e15
header_re = r'^set'
point_re = r'([PL])\s+(-?\S+)\s+(-?\S+)\s+(-?\S+)\s+(-?\S+)'


def rotate_around_x (theta, x, y, z): ## roll
    return (x,
            y*cos(theta) - z*sin(theta),
            y*sin(theta) + z*cos(theta))

def rotate_around_y (theta, x, y, z):
    return (x*cos(theta) + z*sin(theta),
            y,
            z*cos(theta) - x*sin(theta))

def rotate_around_z (theta, x, y, z): ## decination
    return (x*cos(theta) - y*sin(theta),
            x*sin(theta) + y*cos(theta),
            z)



def main (infile_path, rot_roll, rot_ra, rot_dec, scalex, scaley):
    rot_roll = float(rot_roll)
    rot_ra = float(rot_ra)
    rot_dec = float(rot_dec)
    scalex = float(scalex)
    scaley = float(scaley)
    infile = open(infile_path, "r")
    headers = []
    for line in infile:
        line = line.rstrip()
        m = re.match(header_re, line, re.I)
        if m:
            print(line)
            headers.append(line)
        else:
            m = re.match(point_re, line)
            cmd = m.group(1)
            x = float(m.group(2)) * scalex
            y = float(m.group(3)) * scaley
            z = float(m.group(4))
            i = float(m.group(5))
            z = 10.0

            ## the drawing is in X and Y
            (x, y, z) = z, y, -x

            (x, y, z) = rotate_around_x(rot_roll, x, y, z)
            (x, y, z) = rotate_around_y(rot_ra, x, y, z)
            (x, y, z) = rotate_around_z(rot_dec, x, y, z)

            (x, y, z) = (x * LY, y * LY, z * LY)

            print("{}\t{}\t{}\t{}\t{}".format(cmd,x,y,z,i))

if __name__ == "__main__":
    main(*sys.argv[1:])
