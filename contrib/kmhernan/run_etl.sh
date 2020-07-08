#!/bin/bash
set -e
DATADIR="./data"
UF_WEEKLY_DEATHS="covid_deaths_usafacts.weekly.csv"
UF_WEEKLY_CASES="covid_cases_usafacts.weekly.csv"

function print_usage() {
    echo "Usage: run_etl.sh <command>"
    echo "Commands:"
    echo "    usafacts - extract weekly death/cases from USAFacts"
}

function run_usafacts() {
    echo "Running Rscript..."
    Rscript ./etls/usafacts.county.etl.R \
    --out_weekly_deaths ${DATADIR}/${UF_WEEKLY_DEATHS} \
    --out_weekly_cases ${DATADIR}/${UF_WEEKLY_CASES}

    # compressing
    echo "Compressing outputs..." 
    gzip -f ${DATADIR}/${UF_WEEKLY_DEATHS}
    gzip -f ${DATADIR}/${UF_WEEKLY_CASES}
}

if [[ -z $1 ]]; then
    print_usage
    exit 1
fi

if [[ $1 == "usafacts" ]]; then
    echo "Running USA Facts ETL..."
    run_usafacts
else
    echo "Unknown command $1"
    print_usage
    exit 1
fi
