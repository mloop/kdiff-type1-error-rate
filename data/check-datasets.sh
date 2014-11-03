#!/bin/bash

# Pseudocode:
#		0. Prompt user for number of iterations expected
#		1. Go into each directory of datasets (e.g., datasets/c-1/)
#		2. In each directory, count the number of files in that directory
#		3. If the number of files is not equal to the number of iterations, then exit with error that names directory that doesn't have all the files
#		4. If the number of files is equal to the number of iterations, move on to the next directory
#		5. If no mismatches between number of iterations and number of files has been found, exit and say `Number of datasets equal to number of iterations`

# 0 #
read -p "How many datasets should there be in each folder? " iterations

# 1 #
cd datasets
folders=c-*/
for f in $folders; do
	cd $f
# 2 #
	file_count=$(ls | wc -l)
# 3 #
	{
	if (( $file_count != $iterations )); then
		echo "$f does not have the correct number of files"
		exit 0
	fi
	}
cd ..
done
# 5 #
echo "Number of datasets equal to number of iterations"
