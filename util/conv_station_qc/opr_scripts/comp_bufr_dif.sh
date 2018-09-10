#  the script to compare two bufr files: 

file1=$1
file2=$2
binv $file1 $file2 
bfrdif $file1 $file2
