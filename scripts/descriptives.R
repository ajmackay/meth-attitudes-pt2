if(!"packages" %in% ls()) source("scripts/load-packages.R")
source("scripts/functions.R")
load("objects/all-objects.RData")

#### Demographics ####
id.sample <- dat$id

dat.dems %>%
  filter(id %in% id.sample) %>% glimpse()



