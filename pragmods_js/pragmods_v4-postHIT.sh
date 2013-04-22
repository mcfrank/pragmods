#!/usr/bin/env sh
pushd /Applications/aws-mturk-clt-1.3.1/bin
./loadHITs.sh $1 $2 $3 $4 $5 $6 $7 $8 $9 -label /Users/mcfrank/Projects/Pragmatics/pragmods/pragmods_js//pragmods_v4 -input /Users/mcfrank/Projects/Pragmatics/pragmods/pragmods_js//pragmods_v4.input -question /Users/mcfrank/Projects/Pragmatics/pragmods/pragmods_js//pragmods_v4.question -properties /Users/mcfrank/Projects/Pragmatics/pragmods/pragmods_js//pragmods_v4.properties -maxhits 1
popd