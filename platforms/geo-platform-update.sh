#~/bin/bash
# This script will update data from GEO

DIR=~
{
	now=`date +"%m_%d_%Y"`
	mv $DIR/platform.csv $DIR/backup-platform.csv &&
	echo "Backup created.."

} || {

	echo "No local platform data found.. one will be created"


}	
echo "Update GEO data..."
data=$(curl --silent "https://www.ncbi.nlm.nih.gov/geo/browse/?view=platforms" | grep "total_count")
count=$(echo "$data" | grep -Eo '[0-9]*')
count=`python -c "from math import ceil; print int(ceil($count/5000.0))"`

echo "Getting GEO Data..."
for ((i=1; i <= count; i++)); do

if [ "$i != 1" ]
then
	wget --content-disposition "https://www.ncbi.nlm.nih.gov/geo/browse/?view=platforms&zsort=date&mode=csv&page=$i&display=5000" -q -O ->> $DIR/platform.csv
else
	wget --content-disposition "https://www.ncbi.nlm.nih.gov/geo/browse/?view=platforms&zsort=date&mode=csv&page=$i&display=5000" -q -a  ->> $DIR/platform.csv	
   	
fi

done 

echo "Finished update...data saved to -> platform.csv" 
