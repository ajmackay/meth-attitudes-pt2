if(!"packages" %in% ls()) source("scripts/load-packages.R")
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
anova(mgp.free, mgp.constrained) #%>% broom::tidy()


#### Comparing single constraints ####
# Risk
mgp.risk.model <- c("dd.total ~ c(b1, b1) * duid.att.risk + duid.att.sanction + duid.att.peer")
mgp.risk <- sem(mgp.risk.model, dat, group = "ma.ingest")


anova(mgp.free, mgp.risk)
# DUID Attitudes toward risk varies between groups

# Sanction
mgp.sanction.model <-  c("dd.total ~ c(b1, b1) * duid.att.sanction + duid.att.risk + duid.att.peer")
mgp.sanction <- sem(mgp.sanction.model, dat, group = "ma.ingest")

anova(mgp.free, mgp.sanction)

# Peer
mgp.peer.model <- c("dd.total ~ c(b1, b1) * duid.att.peer + duid.att.risk + duid.att.sanction")
mgp.peer <- sem(mgp.peer.model, dat, group = "ma.ingest")

anova(mgp.free, mgp.peer)


#### Piecewise SEM  ####
# (basically does all the above stuff in one go lol)
model.psem <- piecewiseSEM::psem(lm(dd.total ~ duid.att.risk + duid.att.sanction + duid.att.peer, dat))

piecewiseSEM::multigroup(model.psem, group = "ma.ingest")

#### Models ####
ma.lm <- lm(dd.total ~ duid.att.risk + duid.att.sanction + duid.att.peer, filter(dat, ma.ingest))
nma.lm <- lm(dd.total ~ duid.att.risk + duid.att.sanction + duid.att.peer, filter(dat, !ma.ingest))


#### T test between groups for DD ####
dat %>% t_test(dd.total ~ ma.ingest) %>%
  adjust_pvalue() %>%
  add_significance("p.adj") %>%
  mutate(p.adj = scales::pvalue(p.adj))

t.test(dd.total ~ ma.ingest, data = dat)

#### Checking Normality ####
# Can use Kolmogorov-Smirnov as sample size > 50 (yay)
ols_test_normality(ma.lm)
ols_test_normality(nma.lm)


#### Sandbox ####
# Testing to try and get chi-square statistic
mgp.test <- c("dd.total ~ duid.att.risk", "dd.total ~ duid.att.sanction")
sem(mgp.test, data = dat, group = "ma.ingest") %>% summary()

mgp.test <- c("duid.att.risk ~ duid.att.sanction", "duid.att.sanction ~ duid.att.peer")
sem(mgp.test, data = dat, group = "ma.ingest") %>% summary() # Here we get a Chi Sq statistic...
