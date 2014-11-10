#!/bin/bash

days=( 20130601 20130602 20130603 )
hms=( 000000 060000 120000 180000 240000 )

hmsN=${#hms[@]}
hmsN1=`expr ${#hms[@]} - 2`

mkdir bufrOut

for day in $days;do
    for i in `seq 0 $hmsN1`;do
        j=`expr $i + 1`
        mkdir $days${hms[$i]}
        cd $days${hms[$i]}

cat <<EOF >nameList.txt
&input
intxt="fileList.txt" outfile="out" coefile="eta.txt" compress="NO" process_Tb="YES" process_Ta="YES" beginT=$day${hms[$i]} endT=$day${hms[$j]} /
EOF

        cp ../OBSI_bufr_tranamsua.exe ./
        cp ../bufrtab.021 ./
        cp ../eta.txt ./
        cp ../lowtopog.dat ./
        cp ../fileList.txt ./
        ./OBSI_bufr_tranamsua.exe < nameList.txt
        cp fort.52 ../bufrOut/fort.$days${hms[$i]}
        cd ../
    done
done






