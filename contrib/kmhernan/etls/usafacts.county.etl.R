# Kyle Hernandez
# COV-IRT Modeling WG
# ETL for USA FACTS county-level data used in C Conley's analysis
suppressPackageStartupMessages(library(getopt))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(lubridate))

# URL-based so can be easily updated
USAFACTS.DEATHS.URL <- "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv"
USAFACTS.CASES.URL <- "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv"
USAFACTS.COUNTY_POPS.URL <- "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_county_population_usafacts.csv"

# Simple extract
uf.extract <- function() {
  list(uf.confirmed.df=read.csv(USAFACTS.CASES.URL, check.names = FALSE, stringsAsFactors = FALSE),
       uf.deaths.df=read.csv(USAFACTS.DEATHS.URL, check.names = FALSE, stringsAsFactors = FALSE),
       uf.county.pops=read.csv(USAFACTS.COUNTY_POPS.URL, check.names = FALSE, stringsAsFactors = FALSE))
}

# Simple transform
uf.transform <- function(df, pop.df) {
  df %>%
    pivot_longer(c(-countyFIPS, -`County Name`, -State, -stateFIPS), names_to="date", values_to="counts") %>%
    mutate(date=mdy(date)) %>%
    mutate(week_num=week(date), month=month(date)) %>%
    group_by(countyFIPS, `County Name`, State, stateFIPS, month, week_num) %>%
    summarise(counts=sum(counts)) %>%
    left_join(pop.df)
}

# Simple load
uf.load.csv <- function(df, out.file) {
  write.csv(df,
            file=out.file,
            row.names = FALSE)
}

### Options
spec <- matrix(c(
  'help', 'h', 0, "logical",
  'out_weekly_deaths', 'd', 1, "character",
  'out_weekly_cases', 'c', 1, "character"
), byrow=TRUE, ncol=4)
opt <- getopt(spec)

if(!is.null(opt$help)) {
  print(getopt(spec, usage=TRUE))
  q(status=1)
}

if(is.null(opt$out_weekly_deaths)) {
  print("Missing required option --out_weekly_deaths")
  q(status=1)
}
if(is.null(opt$out_weekly_cases)) {
  print("Missing required option --out_weekly_cases")
  q(status=1)
}

### USA facts
message("Extracting datasets from web...")
uf.datasets <- uf.extract()

message("Transforming confirmed cases...")
uf.confirmed.fmt <- uf.transform(uf.datasets$uf.confirmed.df, uf.datasets$uf.county.pops)
uf.confirmed.csv <- opt$out_weekly_cases
message("Loading transformed cases to: ", uf.confirmed.csv)
uf.load.csv(uf.confirmed.fmt, uf.confirmed.csv)

message("Transforming deaths...")
uf.deaths.fmt <- uf.transform(uf.datasets$uf.deaths.df, uf.datasets$uf.county.pops)
uf.deaths.csv <- opt$out_weekly_deaths
message("Loading transformed deaths to: ", uf.deaths.csv)
uf.load.csv(uf.deaths.fmt, uf.deaths.csv)

#sessionInfo()