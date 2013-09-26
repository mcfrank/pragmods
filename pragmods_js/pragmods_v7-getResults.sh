#!/usr/bin/env sh
pushd /Applications/aws-mturk-clt-1.3.1/bin
./getResults.sh $1 $2 $3 $4 $5 $6 $7 $8 $9 -successfile /Users/mcfrank/Projects/Pragmatics/pragmods/pragmods_js//pragmods_v7.success -outputfile /Users/mcfrank/Projects/Pragmatics/pragmods/pragmods_js//pragmods_v7.results.tsv
popd