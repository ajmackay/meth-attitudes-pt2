# Reliability and Factor Loadings
if(!"packages" %in% ls()){
  source("scripts/load-packages.R")
}

source("scripts/functions.R")

load("objects/all-objects.RData")

#### Factor Analysis and Reliability ####

duid.att.items <- duid.att.df %>%
  filter(id %in% dat$id) %>%
  select(-c(duid.att.risk, duid.att.sanction, duid.att.peer, duid.att.mean, duid.att.full, duid.att.once.while))

# Risk
alpha.risk <- psych::alpha(select(duid.att.items, duid.att.overrated, duid.att.police, duid.att.caught, duid.att.high))
fa.risk <- psych::fa(select(duid.att.items, duid.att.overrated, duid.att.police, duid.att.caught, duid.att.high))

# Sanctions
alpha.sanctions <- psych::alpha(select(duid.att.items, duid.att.strict, duid.att.jail, duid.att.lose))
fa.sanctions <- psych::fa(select(duid.att.items, duid.att.strict, duid.att.jail, duid.att.lose))

# Peer Attitudes
alpha.peer <- psych::alpha(select(duid.att.items, duid.att.friends, duid.att.dumb))
fa.peer <- psych::fa(select(duid.att.items, duid.att.friends, duid.att.dumb))


#### Confirmatory Factor Analysis ####
names(duid.att.items) <- names(duid.att.items) %>% str_remove('duid.att.')

# Measurement Model
cfa.measure <- '
  risk = ~overrated + police + caught + high

  sanctions = ~strict + jail + lose

  peers = ~friends + dumb
'

# Saving Model
cfa.att <- cfa(cfa.measure, data = select(duid.att.items, -c(id, ma.ingest)))

# Intercorrelations between latent factors
lavInspect(cfa.att, 'cov.lv')

summary(cfa.att)
