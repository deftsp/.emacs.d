#!/bin/bash

for f in *.org
do
    file_base_name=`basename $f .org`
    slug_file_name=`echo "$file_base_name" | tr '[:upper:]' '[:lower:]' | tr '-' '_'`

    birth_time=`stat -c %W $f`
    file_name_date_prefix=`date +"%Y%m%d%H%M%S" -d @$birth_time`

    new_file_name="${file_name_date_prefix}-${slug_file_name}.org"

    birth_date=`date +"%Y-%m-%d %a" -d @$birth_time`

    modify_time=`stat -c %Y $f`
    modify_date=`date +"%Y-%m-%d %a %H:%M" -d @$birth_time`


    perl -pi -e "print \"#+TITLE: $file_base_name\n#+CREATED: [$birth_date]\n#+LAST_MODIFIED: [$modify_date]\n#+ROAM_TAGS: other\n\n\" if $. == 1" $f

    ######
    mv $f $new_file_name


done
