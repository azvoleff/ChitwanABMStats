#!/usr/bin/env python
# First run list_hh_areas.py to write out the hh_areas.txt text file.

import numpy as np
import csv

hh_file_list = open("hh_areas.txt", "r")
hh_areas = []
for line in hh_file_list:
    line = line.strip('\r\n ')
    line = line.replace('Acres', '')
    line = line.replace('Area', '')
    line = line.replace('=', '')
    line = line.strip()
    area = float(line)
    # Convert area in acres to area in square meters
    area = area * 4046.85642
    hh_areas.append(area)
hh_file_list.close()

outfile = open("hh_areas_T1_sq_meters.csv", "w")
outfile.write("area\n")
for area in hh_areas:
    outfile.write(str(area) + "\n")
outfile.close()

print "Mean area:", np.mean(hh_areas)
print "Std dev:", np.std(hh_areas)
print "n:", np.size(hh_areas)
