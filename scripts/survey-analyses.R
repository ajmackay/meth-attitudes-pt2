# Reliability and Factor Loadings
if(!"packages" %in% ls()){
  source("scripts/load-packages.R")
}

source("scripts/functions.R")

load("objects/all-objects.RData")

#### Reliability ####

duid.att.items <- duid.att.df %>%
  filter(id %in% dat$id) %>%
  select(-c(duid.att.risk, duid.att.sanction, duid.att.peer, duid.att.mean, duid.att.full, duid.att.once.while))

# Risk
alpha.risk <- psych::alpha(select(duid.att.items, duid.att.overrated, duid.att.police, duid.att.caught, duid.att.high))
# fa.risk <- psych::fa(select(duid.att.items, duid.att.overrated, duid.att.police, duid.att.caught, duid.att.high))

# Sanctions
alpha.sanctions <- psych::alpha(select(duid.att.items, duid.att.strict, duid.att.jail, duid.att.lose))
# fa.sanctions <- psych::fa(select(duid.att.items, duid.att.strict, duid.att.jail, duid.att.lose))

# Peer Attitudes
alpha.peer <- psych::alpha(select(duid.att.items, duid.att.friends, duid.att.dumb))
# fa.peer <- psych::fa(select(duid.att.items, duid.att.friends, duid.att.dumb))


#### Confirmatory Factor Analysis ####
names(duid.att.items) <- names(duid.att.items) %>% str_remove('duid.att.')

# Measurement Model
cfa.m1 <- '
  risk =~ overrated + police + caught + high

  sanctions =~ strict + jail + lose

  peers =~ friends + dumb
'

# Saving Model
cfa.att <- cfa(cfa.m1, duid.att.items)

inspect(cfa.att, 'cov.lv')

summary(cfa.att, standardized = TRUE, fit.measures = TRUE)

standardizedsolution(cfa.att)

tbl.cfa.loadings <- inspect(cfa.att, 'std')$lambda %>% data.frame() %>%  rownames_to_column("Variable") %>%
  mutate(across(where(is.numeric), ~round(.x, 3)))

tbl.fit

cfa.m2 <- '
  risk =~ overrated + caught + high
  sanctions =~ strict + jail + lose
  peers =~ friends + dumb
'

cfa.att2 <- cfa(cfa.m2, data = duid.att.items)

summary(cfa.att2, standardized = TRUE, fit.measures = TRUE)

# Intercorrelations/co-variance between latent factors


inspect(cfa.att, 'std')

summary(cfa.att)

cfa.att@Fit
