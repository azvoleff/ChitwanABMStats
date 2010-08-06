#!/usr/bin/env python
# First run list_nfo_areas.py to write out the nfo_areas.txt text file.

import numpy as np
import csv

nfo_area_file = open("nfo_areas.txt", "r")
nfo_type_file = open("nfo_areas_types.txt", "r")
nfo_areas = []
nfo_types = []
for area, type in zip(nfo_area_file, nfo_type_file):
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
nfo_area_file.close()
nfo_type_file.close()

outfile = open("nfo_areas_T1_sq_meters.csv", "w")
csv_writer = csv.writer(outfile)
csv_writer.writerow(["area", "nfo_type"])
for area, type in zip(nfo_areas, nfo_types):
    csv_writer.writerow([str(area), type])
outfile.close()

print "Mean area:", np.mean(nfo_areas)
print "Std dev:", np.std(nfo_areas)
print "n:", np.size(nfo_areas)
