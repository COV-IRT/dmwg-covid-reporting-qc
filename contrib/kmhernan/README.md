# Individual analysis tools

Kyle Hernandez

## ETL

These are extract-transform-load scripts located in `etls/`.

### USA Facts county-level data


#### `etls/usafacts.county.etl.R`

Extract the county-level case and death data from the USA FACTS website (needs web connectivity), transforms
into weekly data with county population column added, and writes to CSVs.

Packages needed:

* `getopt`
* `dplyr`
* `tidyr`
* `lubridate`

Usage:

```
Rscript usafacts.county.etl.R \
--out_weekly_deaths /path/to/outputs.weekly.deaths.csv \
--out_weekly_cases /path/to/outputs.weekly.cases.csv
```
