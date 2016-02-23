#~/bin/bash
# This script will update data from GEO

{
	
	mv ~/series.csv ~/backup-series.csv &&
	echo "Backup created.."

} || {

	echo "No local series data found.. one will be created"


}	
echo "Update GEO data..."
data=$(curl --silent "http://www.ncbi.nlm.nih.gov/geo/browse/" | grep "total_count")
count=$(echo "$data" | grep -Eo '[0-9]*')
count=`python -c "from math import ceil; print int(ceil($count/5000.0))"`

echo "Getting GEO Data..."
for ((i=1; i <= count; i++)); do

if [ "$i != 1" ]
then
	wget --content-disposition "http://www.ncbi.nlm.nih.gov/geo/browse/?view=series&zsort=date&mode=csv&page=$i&display=5000" -q -O ->> ~/series.csv
else
	wget --content-disposition "http://www.ncbi.nlm.nih.gov/geo/browse/?view=series&zsort=date&mode=csv&page=$i&display=5000" -q -a  ->> ~/series.csv	
   	
fi

done 

echo "Finished update...data saved to -> series.csv" 
