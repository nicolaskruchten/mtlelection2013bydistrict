#!/bin/bash

Rscript extractdata.r
topojson --bbox --properties --id-property=NUM_DISTRICT --external-properties=data.csv -o districts.topojson -- districts=districtelect.json alldistricts=districts.json
