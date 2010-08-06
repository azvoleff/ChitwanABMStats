#!/bin/bash
rm hh_areas.txt
touch hh_areas.txt
for file in `grep -cRlE "HOUSEHOLD|household" /media/Local_Secure/CVFS_Land_Use_Mapping/T1`; do grep Acre $file >> hh_areas.txt; done
