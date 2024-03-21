if(!"packages" %in% ls()) source("scripts/load-packages.R")
# devtools::install_github("https://github.com/ajmackay/AzTools.git")
source("scripts/functions.R")
load("objects/all-objects.RData")

####| Free & Constrained Models ####
# Free Model
mgp.model <- "dd.total ~ duid.att.risk + duid.att.sanction + duid.att.peer"

mgp.free <- sem(mgp.model, data = dat, group = "ma.ingest")

summary(mgp.free)

# Constrained Model
mgp.constrained <- sem(mgp.model, dat, group = "ma.ingest", group.equal = c("intercepts", "regressions"))

summary(mgp.constrained)

# Comparing two models
anova(mgp.free, mgp.constrained) #%>% broom::tidy()


####| Comparing single constraints ####
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

summary(model.psem, fit.measures = TRUE)

piecewiseSEM::multigroup(model.psem, group = "ma.ingest")

multigroup.model <- piecewiseSEM::multigroup(model.psem, group = "ma.ingest")


####| Model Fit ####
#####| Overall Model #####
overall.model <- sem(mgp.model, data = dat)
summary(overall.model, fit.measures = TRUE)

summary(mgp.free, fit.measures = TRUE)

####| Regression Models ####
ma.lm <- lm(dd.total ~ duid.att.risk + duid.att.sanction + duid.att.peer, filter(dat, ma.ingest))
nma.lm <- lm(dd.total ~ duid.att.risk + duid.att.sanction + duid.att.peer, filter(dat, !ma.ingest))

ma.effects <- dat %>%
  filter(ma.ingest) %>%
  lm.effect.size(c("duid.att.risk", "duid.att.sanction", "duid.att.peer"), "dd.total") %>%
  rename(term = Variable)


nma.effects <- dat %>%
  filter(!ma.ingest) %>%
  lm.effect.size(c("duid.att.risk", "duid.att.sanction", "duid.att.peer"), "dd.total") %>%
  rename(term = Variable)

ma.lm.tidy <- tidy(ma.lm, conf.int = TRUE)
nma.lm.tidy <- tidy(nma.lm, conf.int = TRUE)

ma.lm.ft <- ma.lm.tidy %>%
  mutate(Estimate = round(estimate, 2),
         `95% CI` = str_c(round(conf.low, 2), " - ", round(conf.high, 2)),
         SE = round(std.error, 2),
         p = scales::pvalue(p.value)) %>%

  left_join(ma.effects) %>%

  select(term, Estimate, `95% CI`, SE, Partial, F2, p) %>%

  prep.flex(digits = 2)

nma.lm.ft <- nma.lm.tidy %>%
  mutate(Estimate = round(estimate, 2),
         `95% CI` = str_c(round(conf.low, 2), " - ", round(conf.high, 2)),
         SE = round(std.error, 2),
         p = scales::pvalue(p.value)) %>%

  left_join(nma.effects) %>%

  select(term, Estimate, `95% CI`, SE, Partial, F2, p) %>%

  prep.flex(digits = 2)


if(FALSE){
save.flex(ma.lm.ft, name = "ma-lm.docx")
save.flex(nma.lm.ft, name = "nma-lm")
}


#### T test between groups for DD ####
dat %>% t_test(dd.total ~ ma.ingest) %>%
  adjust_pvalue() %>%
  add_significance("p.adj") %>%
  mutate(p.adj = scales::pvalue(p.adj))

t.test(dd.total ~ ma.ingest, data = dat)

####| Checking Normality ####
# Can use Kolmogorov-Smirnov as sample size > 50 (yay)
ols_test_normality(ma.lm)
ols_test_normality(nma.lm)


####| Sandbox ####
# Testing to try and get chi-square statistic
mgp.test <- c("dd.total ~ duid.att.risk", "dd.total ~ duid.att.sanction")
sem(mgp.test, data = dat, group = "ma.ingest") %>% summary()

mgp.test <- c("duid.att.risk ~ duid.att.sanction", "duid.att.sanction ~ duid.att.peer")
sem(mgp.test, data = dat, group = "ma.ingest") %>% summary() # Here we get a Chi Sq statistic...
