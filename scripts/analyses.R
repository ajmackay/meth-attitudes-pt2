if(!"packages" %in% ls()){
  source("scripts/load-packages.R")
}

source("scripts/functions.R")

load("objects/all-objects.RData")

# Mediation analysis
x <- lm(dd.total ~ ma.ingest, data = dat)
summary(x)
