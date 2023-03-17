#### Load ####
if(!"packages" %in% ls()){
  source("scripts/load-packages.R")
}

source("scripts/functions.R")

load("objects/all-objects.RData")

#### Free & Constrained Models ####
# Free Model
mgp.model <- "dd.total ~ duid.att.risk + duid.att.sanction + duid.att.peer"

mgp.free <- sem(mgp.model, data = dat, group = "ma.ingest")

summary(mgp.free)

# Constrained Model
mgp.constrained <- sem(mgp.model, dat, group = "ma.ingest", group.equal = c("intercepts", "regressions"))

summary(mgp.constrained)

# Comparing two models
anova(mgp.free, mgp.constrained) %>% broom::tidy()


#### Comparing single constraints ####
# Risk
mgp.risk.model <- c("dd.total ~ c(b1, b1) * duid.att.risk + duid.att.sanction + duid.att.peer")
mgp.risk <- sem(mgp.risk.model, dat, group = "ma.ingest")
summary(mgp.risk)

anova(mgp.free, mgp.risk)
# DUID Attitudes toward risk varies between groups

#



#### Sandbox ####
# Testing to try and get chi-square statistic
mgp.test <- c("dd.total ~ duid.att.risk", "dd.total ~ duid.att.sanction")
sem(mgp.test, data = dat, group = "ma.ingest") %>% summary()

mgp.test <- c("duid.att.risk ~ duid.att.sanction", "duid.att.sanction ~ duid.att.peer")
sem(mgp.test, data = dat, group = "ma.ingest") %>% summary() # Here we get a Chi Sq statistic...
