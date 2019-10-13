#Q2
#!/bin/env bash

extract_col () {
#find data, if cannot find it,break.
if [ ! -f "$data" ]; then

      # Print a message in the directory
    echo Hi $data! > $data/$data

else    #data set is already there
    head -n 1 $data | tr "," "\n" > recs_names.txt
    var1=$(echo $str | cut -f1 -d-)
    var2=$(echo $str | cut -f2 -d-)     #split string into varible
    cols=$(grep -n -e "var1" -e "var2" recs_names.txt | cut -d":" -f1 | paste -s -d ",")
    awk '{print $columns}' columns=$cols $data

}