#!/bin/bash
# NOTE: T2 Might be better - it looks like it has more data.
rm nfo_areas.txt
touch nfo_areas.txt
for file in `grep -ciRlE "school|temple|church|health|medic" /media/Local_Secure/CVFS_Land_Use_Mapping/T3`; do grep Acre $file >> nfo_areas.txt; done
# Below is for T2
#for file in `grep -ciRlE "school|temple|church|health|medic" /media/Local_Secure/CVFS_Land_Use_Mapping/T2`; do grep Acre $file >> nfo_areas.txt; done

rm nfo_areas_types.txt
touch nfo_areas_types.txt
# Below is for T3
for file in `grep -ciRlE "school|temple|church|health|medic" /media/Local_Secure/CVFS_Land_Use_Mapping/T3`; do grep "LAND PARCEL" $file >> nfo_areas_types.txt; done
# Below is for T2
#for file in `grep -ciRlE "school|temple|church|health|medic" /media/Local_Secure/CVFS_Land_Use_Mapping/T2`; do grep "LAND PARCEL" $file >> nfo_areas_types.txt; done
# Below is for T1
#for file in `grep -ciRlE "school|temple|church|health|medic" /media/Local_Secure/CVFS_Land_Use_Mapping/T3`; do grep "Area type" $file >> nfo_areas_types.txt; done
