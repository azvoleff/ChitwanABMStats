#!/usr/bin/env python
# First run list_nfo_areas.py to write out the nfo_areas.txt text file.

import os
import re
import csv

import numpy as np

nfo_areas_raw = []
nfo_types_raw = []
for root, dirs, files in os.walk("V:/Nepal/CVFS_LULC_Mapping/BND_Files/T3"):
    for file in files:
        filepath = os.path.join(root, file)
        fid = open(filepath, 'r')
        filetext = fid.readlines()
        fid.close()
        nfo_flag = False
        for line in filetext:
            if re.search('school|temple|church|health|medic', line.lower()) != None:
                nfo_flag = True
            if 'acre' in line.lower():
                nfo_areas_line = line
            if 'land parcel' in line.lower():
                nfo_area_types_line = line
        if nfo_flag == True:
            nfo_areas_raw.append(nfo_areas_line)
            nfo_types_raw.append(nfo_area_types_line)

nfo_areas = []
nfo_types = []
for area, type in zip(nfo_areas_raw, nfo_types_raw):
    area = area.strip('\r\n ')
    area = area.replace('Acres', '')
    area = area.replace('Area', '')
    area = area.replace('=', '')
    area = area.strip()
    area = float(area)
    # Convert area in acres to area in square meters
    area = area * 4046.85642
    nfo_areas.append(area)

    type = type.strip('\r\n ')
    type = type.replace('Results for LAND PARCEL ...', '')
    type = type.replace('=', '')
    type = type.strip()
    # Recategorize types:
    if "school" in type.lower():
        type = "school"
    if "temple" in type.lower() or "church" in type.lower():
        type = "temple"
    if "health" in type.lower() or "medic" in type.lower():
        type = "health"
    nfo_types.append(type)

outfile = open("nfo_areas_T3_sq_meters.csv", "w")
csv_writer = csv.writer(outfile)
csv_writer.writerow(["area", "nfo_type"])
for area, type in zip(nfo_areas, nfo_types):
    csv_writer.writerow([str(area), type])
outfile.close()

print "Mean area:", np.mean(nfo_areas)
print "Std dev:", np.std(nfo_areas)
print "n:", np.size(nfo_areas)
