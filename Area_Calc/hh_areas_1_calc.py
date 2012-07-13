#!/usr/bin/env python
# First run list_hh_areas.py to write out the hh_areas.txt text file.

import os
import numpy as np

hh_areas_raw = []
for root, dirs, files in os.walk("V:/Nepal/CVFS_LULC_Mapping/T1_BND_Files"):
    for file in files:
        filepath = os.path.join(root, file)
        fid = open(filepath, 'r')
        filetext = fid.readlines()
        fid.close()
        household_flag = False
        for line in filetext:
            if 'household' in line.lower():
                household_flag = True
            if 'acre' in line.lower():
                hh_areas_line = line
                break
        if household_flag == True: hh_areas_raw.append(hh_areas_line)

hh_areas = []
for line in hh_areas_raw:
    line = line.strip('\r\n ')
    line = line.replace('Acres', '')
    line = line.replace('Area', '')
    line = line.replace('=', '')
    line = line.strip()
    area = float(line)
    # Convert area in acres to area in square meters
    area = area * 4046.85642
    hh_areas.append(area)

outfile = open("hh_areas_T1_sq_meters.csv", "w")
outfile.write("area\n")
for area in hh_areas:
    outfile.write(str(area) + "\n")
outfile.close()

print "Mean area:", np.mean(hh_areas)
print "Std dev:", np.std(hh_areas)
print "n:", np.size(hh_areas)
