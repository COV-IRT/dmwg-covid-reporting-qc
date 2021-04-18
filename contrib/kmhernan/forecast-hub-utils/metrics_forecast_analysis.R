# Perform cross correlation an other explorations of the metrics and forecast scores.
library(ggplot2)
source("./forecast-hub-utils/utils.R")

# config
config <- get.config()

# load scores
incident.scores <- load.incident.score.metrics()

# select what I care about 
sel <- incident.scores %>%
  arrange(abbreviation, location_name, model, horizon, target_end_date) %>%
  select(abbreviation, location_name, model, target_end_date, horizon,
         overprediction, underprediction, wis, AprUnd.Surv,
         AprUnd.Surv.AveExp, Average.Expected.Count)

# cross-corr
inc.cc.dat <- do.cross.corr(sel)

# Write out results
write.csv(inc.cc.dat,
          file = file.path("/Users/kylehernandez/Projects/Other/COV-IRT/research/sylvain-eloise-covid",
                           "data/cassie-sets/derived/kmh.crosscorr.scores.excursions.all_models.7Apr21WeeklyTimecourse.csv"),
          na = "", row.names = FALSE)