# Analyses to test the reliability of the survey
if(!"packages" %in% ls()){
  source("scripts/load-packages.R")
}

source("scripts/functions.R")

load("objects/all-objects.RData")

duid.att.items <- duid.att.df %>%
  filter(id %in% dat$id) %>%
  select(-c(duid.att.risk, duid.att.sanction, duid.att.peer, duid.att.mean, duid.att.full))

# Risk
alpha.risk <- psych::alpha(select(duid.att.items, duid.att.overrated, duid.att.police, duid.att.caught, duid.att.high))

# Sanctions
alpha.sanctions <- psych::alpha(select(duid.att.items, duid.att.strict, duid.att.jail, duid.att.lose))

# Peer Attitudes
alpha.peer <- psych::alpha(select(duid.att.items, duid.att.friends, duid.att.dumb))
