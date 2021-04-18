# Utilities for forecast model analysis
require(dplyr)

# Load config
get.config <- function(path="./forecast-hub-utils/config.json"){
  return(rjson::fromJSON(file=path))
}

# Load cumulative scores
load.fc.cumulative.scores <- function(){
  path <- get.config()$inc_death_scores
  dat <- read.csv(path, stringsAsFactors = FALSE, na.strings=c(""))
  dat %>% mutate(target_end_date=lubridate::ymd(target_end_date))
}

# Load incidence scores
load.fc.incident.scores <- function(){
  path <- get.config()$cumulative_death_scores
  dat <- read.csv(path, stringsAsFactors = FALSE, na.strings=c(""))
  dat %>% mutate(target_end_date=lubridate::ymd(target_end_date))
}

# load metrics
load.metrics <- function(){
  path <- get.config()$metrics_data
  dat <- read.csv(path, stringsAsFactors = FALSE, na.strings=c(""))
  dat %>%
    mutate(Week.Ending.Date=lubridate::ymd(gsub("(.*)( .*)", "\\1", Week.Ending.Date))) 
}

# Load cumulative scores with metrics merged
load.cumulative.score.metrics <- function(){
  metrics.df <- load.metrics()
  score.df <- load.fc.cumulative.scores()
  score.df %>%
    filter(!(abbreviation %in% c("AS", "US", "GU", "MP", "PR", "VI"))) %>%
    inner_join(metrics.df, by=c("location_name"="Jurisdiction.of.Occurrence",
                                    "target_end_date"="Week.Ending.Date")) %>%
    arrange(model, location_name, target_end_date, horizon)
}

# Load incident scores with metrics merged
load.incident.score.metrics <- function(){
  metrics.df <- load.metrics()
  score.df <- load.fc.incident.scores()
  score.df %>%
    filter(!(abbreviation %in% c("AS", "US", "GU", "MP", "PR", "VI"))) %>%
    inner_join(metrics.df, by=c("location_name"="Jurisdiction.of.Occurrence",
                                    "target_end_date"="Week.Ending.Date")) %>%
    arrange(model, location_name, target_end_date, horizon)
}

# Do cross-correlation
do.cross.corr <- function(dat){
  # Get abbrev I care about
  abbvs <- unique(incident.scores$abbreviation)[
    !(unique(incident.scores$abbreviation) %in%
        c("AS", "US", "GU", "MP", "PR", "VI"))]
  .do.cross.corr(dat, abbvs)
}

.do.cross.corr <- function(sel, abbvs){
  # Nested apply functions to generate the cross-correlations within each
  # state and each horizon.
  res <- lapply(abbvs, function(x){
    print(paste("Processing state:", x))
    # subset state
    abbv.sel <- subset(sel, abbreviation==x)
    # nested apply for each horizon
    res.sub <- lapply(c(1, 2, 3, 4), function(y){
      # subset horizon
      horizon.sel <- subset(abbv.sel, horizon==y)
      models.names <- unique(horizon.sel$model)
      # Nested apply over models
      res.model <- lapply(models.names, function(z){
        curr <- subset(horizon.sel, model==z)
        curr <- curr %>%
          mutate(excursion=(overprediction-underprediction)/Average.Expected.Count) %>%
          arrange(target_end_date)
        
        # ccf excursion 
        res.excur <- ccf(curr$excursion, curr$AprUnd.Surv.AveExp,
                         na.action=na.pass, type="correlation",
                         plot=FALSE)
        # Convert to tibble and add a couple columns
        res.excur <- broom::tidy(res.excur) %>%
          mutate(abbreviation=x, var.a="excursion",
                 var.b="AprUnd.Surv.AveExp",
                 horizon=y,
                 model=z)
        
        # ccf underprediction
        res.under <- ccf(curr$underprediction, curr$AprUnd.Surv,
                         na.action=na.pass, type="correlation",
                         plot=FALSE)
        # Convert to tibble and add a couple columns
        res.under <- broom::tidy(res.under) %>%
          mutate(abbreviation=x, var.a="underprediction",
                 var.b="AprUnd.Surv",
                 horizon=y, model=z)
        
        # ccf overprediction
        res.over <- ccf(curr$overprediction, curr$AprUnd.Surv,
                        na.action=na.pass, type="correlation",
                        plot=FALSE)
        res.over <- broom::tidy(res.over) %>%
          mutate(abbreviation=x, var.a="overprediction",
                 var.b="AprUnd.Surv", horizon=y, model=z)
        
        # ccf overprediction with underprediction
        res.both <- ccf(curr$overprediction, curr$underprediction,
                        na.action=na.pass, type="correlation",
                        plot=FALSE)
        res.both <- broom::tidy(res.both) %>%
          mutate(abbreviation=x, var.a="overprediction",
                 var.b="underprediction", horizon=y, model=z)
        
        # ccf wis 
        res.wis <- ccf(curr$wis, curr$AprUnd.Surv,
                       na.action=na.pass, type="correlation",
                       plot=FALSE)
        res.wis <- broom::tidy(res.wis) %>%
          mutate(abbreviation=x, var.a="wis",
                 var.b="AprUnd.Surv", horizon=y, model=z)
        
        # Combine all three dfs
        return(rbind(res.excur, res.under, res.over,
                     res.both, res.wis)) 
      })
      
      # Combine list into dataframe
      return(do.call(rbind, res.model))
    })
    
    # Combine list returned by nested lapply into dataframe
    return(do.call(rbind, res.sub))
  })
  # make final dataframe
  do.call(rbind, res)
}