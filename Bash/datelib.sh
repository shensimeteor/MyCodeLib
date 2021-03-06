#!/bin/bash
# a common lib for date manipulation
# . support: date8: YYYYMMDD; date10: YYYYMMDDHH; date12: YYYYMMDDHHmm; date14: YYYYMMDDHHmmss
# -  date8/10/12/14 will be converted to datelib_globe_ymdhms first, to ease the difficulty for different date format
# -  fully make use of linux date command, to ease the coding
# . date_to_str (str is recognized by date command)
# . date_add
# . date_to_glob_ymdhms
# . date_to_seconds
# . date_compare

#global variables
# datelib_globe_ymdhms: 6
declare -a datelib_globe_ymdhms 

#functions

# $1=date, will define datelib_globe_ymdhms
function date_to_glob_ymdhms(){ #{{{
   datelib_globe_ymdhms[0]=$(echo $1 | cut -c 1-4)
   datelib_globe_ymdhms[1]=$(echo $1 | cut -c 5-6)
   datelib_globe_ymdhms[2]=$(echo $1 | cut -c 7-8)
   datelib_globe_ymdhms[3]=$(echo $1 | cut -c 9-10)
   datelib_globe_ymdhms[4]=$(echo $1 | cut -c 11-12)
   datelib_globe_ymdhms[5]=$(echo $1 | cut -c 13-14)
   if [ -z "${datelib_globe_ymdhms[3]}" ]; then
       datelib_globe_ymdhms[3]=00
   fi
   if [ -z "${datelib_globe_ymdhms[4]}" ]; then
       datelib_globe_ymdhms[4]=00
   fi
   if [ -z "${datelib_globe_ymdhms[5]}" ]; then
       datelib_globe_ymdhms[5]=00
   fi
} #}}}

# $1=date, echo = str
function date_to_str() { #{{{
    local str1 str2
    date_to_glob_ymdhms $1
    str1="${datelib_globe_ymdhms[0]}-${datelib_globe_ymdhms[1]}-${datelib_globe_ymdhms[2]}"
    str2="${datelib_globe_ymdhms[3]}:${datelib_globe_ymdhms[4]}:${datelib_globe_ymdhms[5]}"
    echo "$str1 $str2"
} #}}}

# $1=date, echo=seconds (date +%s)
function date_to_seconds() { #{{{
    local sec str
    str=$(date_to_str $1)
    sec=$(date -d "$str" +%s)
    echo $sec
} #}}}
   
# $1=str_of_date
function str_to_glob_ymdhms(){ #{{{
     datelib_globe_ymdhms[0]=$(date -d "$1" +%Y)
     datelib_globe_ymdhms[1]=$(date -d "$1" +%m)
     datelib_globe_ymdhms[2]=$(date -d "$1" +%d)
     datelib_globe_ymdhms[3]=$(date -d "$1" +%H)
     datelib_globe_ymdhms[4]=$(date -d "$1" +%M)
     datelib_globe_ymdhms[5]=$(date -d "$1" +%S)
} #}}}


# $1=str_of_date (YYYY-MM-DD HH:mm:ss); $2=8/10/12/14, echo=date
function str_to_date() { #{{{
    local xdate
    str_to_glob_ymdhms "$1"
    if [ $2 -eq 8 ]; then
        xdate=${datelib_globe_ymdhms[0]}${datelib_globe_ymdhms[1]}${datelib_globe_ymdhms[2]}
    elif [ $2 -eq 10 ]; then
        xdate=${datelib_globe_ymdhms[0]}${datelib_globe_ymdhms[1]}${datelib_globe_ymdhms[2]}
        xdate=$xdate${datelib_globe_ymdhms[3]}
    elif [ $2 -eq 12 ]; then
        xdate=${datelib_globe_ymdhms[0]}${datelib_globe_ymdhms[1]}${datelib_globe_ymdhms[2]}
        xdate=$xdate${datelib_globe_ymdhms[3]}${datelib_globe_ymdhms[4]}
    elif [ $2 -eq 14 ]; then
        xdate=${datelib_globe_ymdhms[0]}${datelib_globe_ymdhms[1]}${datelib_globe_ymdhms[2]}
        xdate=$xdate${datelib_globe_ymdhms[3]}${datelib_globe_ymdhms[4]}${datelib_globe_ymdhms[5]}
    else #use 14 as default
        xdate=${datelib_globe_ymdhms[0]}${datelib_globe_ymdhms[1]}${datelib_globe_ymdhms[2]}
        xdate=$xdate${datelib_globe_ymdhms[3]}${datelib_globe_ymdhms[4]}${datelib_globe_ymdhms[5]}
    fi
    echo $xdate
} #}}}


# $1=date, $2=add_value, $3=add_units (year,month,day,hour,minute,second), echo=format of $1
function date_add() { #{{{
    local str str2 xdate xdate2
    xdate=$1
    str=$(date_to_str $1)
    val=$2
    unt=$3
    if [ $val -lt 0 ]; then # negative value
        val=$(( -1 * $val))
        unt="$unt ago"
    elif [ $val -gt 0 ]; then # in case: --1, ----1
        val=$(( 1* $val))
    else # -eq 0, -0, or --0
        val=0
    fi
    str2=$(date -d "$str $val $unt")
    xdate2=$(str_to_date "$str2" ${#xdate})
    echo $xdate2
} #}}}

# $1=start_date, $2=end_date, $3=interval value(must>0), $4=interval units(same as $3 in date_add
# echo="date1 date2 .. daten" (separated by space); if no date in between, return ""
# e.g. dates="$(date_range 2015092718 2015101018 24 hours)"
function date_range(){ #{{{
    local sdate edate intv unit xdate res diff
    sdate=$1
    edate=$2
    intv=$3
    unit=$4
    xdate=$sdate
    res=""
    while [ 1==1 ]; do
        diff=$(date_compare $xdate $edate)
        if [ $diff -gt 0 ]; then # xdate > edate
            break;
        fi
        res="$res $xdate"
        xdate=$(date_add $xdate $intv $unit)
    done
    echo "$res"
} #}}}
 

# $1=date1, $2=date2, echo=($date1-$date2) in hour
function date_compare() { #{{{
    local sec1 sec2 dhr
    sec1=$(date_to_seconds $1)
    sec2=$(date_to_seconds $2)
    dhr=$(( $sec1 - $sec2))
    dhr=$(( $dhr / 3600))
    echo $dhr
} #}}}

# $1=outfile(aux or wrfout); $2=prefix, $3=postfix; return date14(yyyymmddHHMMSS)
function date_from_outfile() {  #{{{
    local outfile=$1
    local prefix=$2
    local postfix=$3
    local temp=${outfile#*$prefix}
    local temp2=${temp%$postfix*}
    local date14="$(echo $temp2 | cut -c 1-4,6-7,9-10,12-13,15-16,18-19)"
    echo $date14

}  #}}}

# no need to specify prefix /postfix, assume structure: < >_< >_2017-07-05_00:00:00*
function date_from_outfile2() { #{{{
    local outfile=$1
    local datetemp=$(echo $outfile | cut -d '_' -f 3,4)
    local date14=$(echo $datetemp | cut -c 1-4,6-7,9-10,12-13,15-16,18-19)
    echo $date14
} #}}}

