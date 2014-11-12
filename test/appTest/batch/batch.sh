#!/bin/bash

###################   function   ###############################################

days1=( 0 31 28 31 30 31 30 31 31 30 31 30 31 )
days2=( 0 31 29 31 30 31 30 31 31 30 31 30 31 )

ifLY(){
    if [ `expr $1 % 4` -eq 0 ] && [ `expr $1 % 100` -ne 0 ];then
        echo 1
    elif [ `expr $1 % 400` -eq 0 ];then
        echo 1
    else
        echo 0
    fi
}


nextDay(){
    y1=$((10#${1:0:4})); m1=$((10#${1:4:2})); d1=$((10#${1:6:2}))
    y2=$y1; m2=$m1; d2=`expr $d1 + 1`

    if [ `ifLY $y1` -eq 1 ];then
        if [ $d2 -gt ${days2[$m1]} ];then   
            d2=`expr $d2 - ${days2[$m1]}`
            m2=`expr $m1 + 1`
            if [ $m2 -gt 12 ];then
                m2=`expr $m2 - 12`
                y2=`expr $y1 + 1`
            fi
          
        fi
    fi
    if [ `ifLY $y1` -eq 0 ];then
        if [ $d2 -gt ${days1[$m1]} ];then   
            d2=`expr $d2 - ${days1[$m1]}`
            m2=`expr $m1 + 1`
            if [ $m2 -gt 12 ];then
                m2=`expr $m2 - 12`
                y2=`expr $y1 + 1`
            fi
          
        fi
    fi
    m2=`printf "%02d" $m2`; d2=`printf "%02d" $d2`
    echo $y2$m2$d2
}

lastDay(){
    y1=$((10#${1:0:4})); m1=$((10#${1:4:2})); d1=$((10#${1:6:2}))
    y2=$y1; m2=$m1; d2=`expr $d1 - 1`

    if [ `ifLY $y1` -eq 1 ];then
        if [ $d2 -lt 1 ];then   
            m2=`expr $m1 - 1`
            if [ $m2 -lt 1 ];then
                y2=`expr $y1 - 1`
                m2=12
                d2=31
            else
                d2=${days2[$m2]}
            fi
        fi
    fi

    if [ `ifLY $y1` -eq 0 ];then
        if [ $d2 -lt 1 ];then   
            m2=`expr $m1 - 1`
            if [ $m2 -lt 1 ];then
                y2=`expr $y1 - 1`
                m2=12
                d2=31
            else
                d2=${days1[$m2]}
            fi
        fi
    fi

    m2=`printf "%02d" $m2`; d2=`printf "%02d" $d2`
    echo $y2$m2$d2
}

timeAdd(){
#timeAdd 20140101120131 030200
    Y1=$((10#${1:0:4})); M1=$((10#${1:4:2})); D1=$((10#${1:6:2}))
    h1=$((10#${1:8:2})); m1=$((10#${1:10:2})); s1=$((10#${1:12:2}))
    dh=$((10#${2:0:2})); dm=$((10#${2:2:2})); ds=$((10#${2:4:2}))

    sec1=`expr $h1 \* 3600 + $m1 \* 60 + $s1`
    dsec=`expr $dh \* 3600 + $dm \* 60 + $ds`
    sec2=`expr $sec1 + $dsec`

    day=`expr $sec2 / 3600 / 24`
    hour=`expr $sec2 / 3600 % 24`
    minu=`expr $sec2 % 3600 / 60`
    sec=`expr $sec2 % 3600 % 60`

    Y1=`printf "%02d" $Y1`
    M1=`printf "%02d" $M1`
    D1=`printf "%02d" $D1`
    date=$Y1$M1$D1

    for((i=0;i<$day;i++));do
        date=`nextDay $date`
    done

    hour=`printf "%02d" $hour`
    minu=`printf "%02d" $minu`
    sec=`printf "%02d" $sec`
 
    echo $date$hour$minu$sec
}

timeMinus(){
#timeMinus 20140101120131 030200
    Y1=$((10#${1:0:4})); M1=$((10#${1:4:2})); D1=$((10#${1:6:2}))
    h1=$((10#${1:8:2})); m1=$((10#${1:10:2})); s1=$((10#${1:12:2}))
    dh=$((10#${2:0:2})); dm=$((10#${2:2:2})); ds=$((10#${2:4:2}))

    sec1=`expr $h1 \* 3600 + $m1 \* 60 + $s1`
    dsec=`expr $dh \* 3600 + $dm \* 60 + $ds`

    dday=`expr $dsec / 3600 / 24 + 1`

    Y1=`printf "%02d" $Y1`
    M1=`printf "%02d" $M1`
    D1=`printf "%02d" $D1`
    date=$Y1$M1$D1
    for((i=0;i<$dday;i++));do
        date=`lastDay $date`
    done

    sec2=`expr $sec1 + $dday \* 24 \* 3600 - $dsec`
    dh2=`expr $sec2 / 3600`; dm2=`expr $sec2 % 3600 / 60`; ds2=`expr $sec2 % 3600 % 60`

    dh2=`printf "%02d" $dh2`
    dm2=`printf "%02d" $dm2`
    ds2=`printf "%02d" $ds2`
 
    echo `timeAdd ${date}000000 $dh2$dm2$ds2`
}


dateToDays(){
    y1=$((10#${1:0:4})); m1=$((10#${1:4:2})); d1=$((10#${1:6:2}))
    y2=$y1; m2=$m1; d2=`expr $d1 - 1`

    days=0
    if [ `ifLY $y1` -eq 1 ];then
        for((i=1;i<$m1;i++));do
            days=`expr $days + ${days2[$i]}`
        done
        days=`expr $days + $d1`
    fi
    
    if [ `ifLY $y1` -eq 0 ];then
       for((i=1;i<$m1;i++));do
           days=`expr $days + ${days1[$i]}`
       done
       days=`expr $days + $d1`
    fi

    y1=$((10#${y1:2:2}))

    echo $y1$days
}

#######################     parameter     #######################################

startT=20130501
endT=20131231
beforeT=030000;afterT=030000
nowT=$startT
dataDir="/home/zxt/NOAA/test/appTest/"

timeP=( 000000 060000 120000 180000 ) 

#########################       run        ######################################

mkdir bufrOut

while [ $nowT -ne $endT ];do
    days1=`dateToDays $nowT`
    date0=`lastDay $nowT`; days0=`dateToDays $date0`
    date2=`nextDay $nowT`; days2=`dateToDays $date2`

    find $dataDir -name *D${days0}* -or -name *D${days1}* -or -name *D${days2}* 2>/dev/null 1>fileList.txt

    lineNum=`wc -l fileList.txt | awk '{printf("%s",$1)}'`
    if [ $lineNum -eq 0 ];then
        nowT=`nextDay $nowT`
        continue
    fi

    for tP in ${timeP[*]};do
        time1=`timeMinus $nowT$tP $beforeT` 
        time2=`timeAdd $nowT$tP $afterT` 
        echo $time1-$time2

        mkdir tran_$nowT$tP
        cd tran_$nowT$tP

cat <<EOF >nameList.txt
&input
intxt="fileList.txt" outfile="out" coefile="eta.txt" compress="NO" process_Tb="YES" process_Ta="YES" beginT=$time1 endT=$time2 /
EOF

        cp ../OBSI_bufr_tranamsua.exe ./
        cp ../bufrtab.021 ./
        cp ../eta.txt ./
        cp ../lowtopog.dat ./
        cp ../fileList.txt ./
        ./OBSI_bufr_tranamsua.exe < nameList.txt
        cp fort.52 ../bufrOut/fort.$nowT$tP

        cd ../

    done
    nowT=`nextDay $nowT`
done
