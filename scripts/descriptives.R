if(!"packages" %in% ls()) source("scripts/load-packages.R")
source("scripts/functions.R")
load("objects/all-objects.RData")

#### Demographics ####
id.sample <- dat$id

dat.dems <- summ.df %>%
  filter(id %in% id.sample, dems.full) %>%
  select(id, ma.ingest, age, sex, ethnicity, marital.status, education, employment.status,
         area.live, license.status, ma.use.peak, sds.total, ma.type, ma.use.age, k6.total, k6.full)

dat.dems %>%
  summ.tbl(summ.by = 'ma.ingest')

