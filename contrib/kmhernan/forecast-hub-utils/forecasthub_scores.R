# Logic used to generate scores based off of the forecast hub.
# User needs to clone the repo https://github.com/reichlab/covid19-forecast-hub
library(covidHubUtils)
library(dplyr)
library(rjson)

##################################################
# Users need to fill in these settings in the config.json file
config <- fromJSON(file="./config.json")

# Path to forecast hub repo
repo_path <- config$forecast_hub_repo

# select models
models.sel <- config$selected_models

# Path to save death incident forecasts
inc_death.forecasts.path <- config$inc_death_forecasts

# Path to save death incident forecast scores
inc_death.scores.path <- config$inc_death_scores

# Path to save death cumulative forecasts
cumulative_death.forecasts.path <- config$cumulative_death_forecasts

# Path to save death cumulative forecast scores
cumulative_death.scores.path <- config$cumulative_death_scores

##################################################
#### Death incidence
inc_death_targets <- paste(1:4, "wk ahead inc death")
hub.fc.inc_death.dat <- load_forecasts(models=models.sel, source="local_hub_repo",
                                    hub_repo_path = repo_path, hub="US",
                                    targets=inc_death_targets,
                                    verbose=TRUE)
write.csv(hub.fc.inc_death.dat,
          file=gzfile(inc_death.forecasts.path),
          row.names=FALSE, na = "")

# Pull out metadata
hub.meta <- hub.fc.inc_death.dat %>%
  distinct(location, location_name, population, abbreviation)

# Get truth data
jhu_truth_data_inc <- load_truth(truth_source = "JHU",
                         target_variable = "inc death",
                         data_location="local_hub_repo",
                         local_repo_path = repo_path)

# Filter
hub.sel <- hub.fc.inc_death.dat %>%
  mutate(target_end_date=as.Date(target_end_date), forecast_date=as.Date(forecast_date)) %>%
  filter(target_end_date>=as.Date("2020-06-01") & target_end_date <as.Date("2020-12-01"))

# Scores
hub.fc.inc_death.scores <- score_forecasts(hub.sel, jhu_truth_data_inc)
hub.fc.inc_death.scores <- hub.fc.inc_death.scores %>%
  left_join(hub.meta, by=c("location"="location"))

write.csv(hub.fc.inc_death.scores,
          file=gzfile(inc_death.scores.path),
          row.names=FALSE, na = "")

#########################################################3
### Cumulative
cum_death_targets <- paste(1:4, "wk ahead cum death")
hub.fc.cum_death.dat <- load_forecasts(models=models.sel, source="local_hub_repo",
                                    hub_repo_path = repo_path, hub="US",
                                    targets=cum_death_targets,
                                    verbose=TRUE)

# Save raw forecasts
write.csv(hub.fc.cum_death.dat,
          file=gzfile(cumulative_death.forecasts.path),
          row.names=FALSE, na = "")

# Get metadata
hub.meta <- hub.fc.cum_death.dat %>%
  distinct(location, location_name, population, abbreviation)

# Get truth data
jhu_truth_data <- load_truth(truth_source = "JHU",
                         target_variable = "cum death",
                         data_location="local_hub_repo",
                         local_repo_path = repo_path)

# Filter
hub.sel <- hub.fc.cum_death.dat %>%
  mutate(target_end_date=as.Date(target_end_date), forecast_date=as.Date(forecast_date)) %>%
  filter(target_end_date>=as.Date("2020-06-01") & target_end_date < as.Date("2020-12-01"))

# Scores
hub.fc.cum_death.scores <- score_forecasts(hub.sel, jhu_truth_data)
hub.fc.cum_death.scores <- hub.fc.cum_death.scores %>%
  left_join(hub.meta, by=c("location"="location"))

# Write cumulative scores
write.csv(hub.fc.cum_death.scores,
          file=gzfile(cumulative_death.scores.path),
          row.names=FALSE, na = "")