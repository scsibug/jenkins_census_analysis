#!/bin/bash
echo "Fetching Jenkins Census data..."
wget -A .gz -l 1 -r  --http-user=jenkins --http-password=butler https://jenkins-ci.org/census/
if [ $? -ne 0 ]; then
  echo "Error retrieving census data."
  return 1
fi
mv jenkins-ci.org/census data
rm -rf jenkins-ci.org index.html
cd data
echo "Expanding compressed census data..."
for gz in *gz; do gzip -d $gz; done
if [ $? -ne 0 ]; then
  echo "Error expanding census gzip files."
else;
  echo "Complete"
fi