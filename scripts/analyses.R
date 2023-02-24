if(!"packages" %in% ls()){
  source("scripts/load-packages.R")
}

source("scripts/functions.R")

load("objects/all-objects.RData")

# Prep --------------------------------------------------------------------
duid.vars <- c("duid.att.risk", "duid.att.sanction", "duid.att.peer")


#### Correlation Plot ####
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

duid.cor <- cor(select(dat, all_of(duid.vars)))
duid.cor.ma <- cor(select(filter(dat, ma.ingest), all_of(duid.vars)))
duid.cor.ndu <- cor(select(filter(dat, !ma.ingest), all_of(duid.vars)))

corrplot::corrplot(duid.cor,
                   tl.col = "black", tl.srt = 45,
                   col = col(200), addCoef.col = "black", cl.pos = 'n',
                   type = 'lower')

#### Regression ####
##### No group #####
lm.full <- lm(dd.total ~ duid.att.risk + duid.att.sanction + duid.att.peer, data = dat)

##### MA #####
lm.ma <- lm(dd.total ~ duid.att.risk + duid.att.sanction + duid.att.peer, data = filter(dat, ma.ingest))

##### NDU #####
lm.ndu <- lm(dd.total ~ duid.att.risk + duid.att.sanction + duid.att.peer, data = filter(dat, !ma.ingest))

# Overall Multicolinearity Diagnostics
mctest::omcdiag(ma.lm)

# Individual Multicolinearity Diagnostics
mctest::imcdiag(ma.lm)
