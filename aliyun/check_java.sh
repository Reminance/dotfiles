#!/bin/bash

#for i in `netstat -tpln|grep java|awk -F "[ /]+" '{print $7}'`
#for i in `netstat -tpln|grep java|awk -F "[ /]+" '{print $7}'|sort -u`
netstat -tpln|grep java|awk -F "[ /:]+" '{print $4,$7}'|sort -u|grep -v "0.0.0.0" | while read file
#netstat -tpln|grep java|awk -F "[ /:]+" '{print $4,$7}'|sort -u| while read file
do
    form=`pwdx ${file##* }`
    length=`echo ${form} | wc -L`
    if [ $length -gt 31 ];
    then
        echo -e "${form}\t\t${file%% *}"
    else
        echo -e "${form}\t\t\t${file%% *}"
    fi
#done < `netstat -tpln|grep java|awk -F "[ /]+" '{print $7}'|sort -u`
done

