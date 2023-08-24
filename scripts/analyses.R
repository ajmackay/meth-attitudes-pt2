if(!"packages" %in% ls()) source("scripts/load-packages.R")
source("scripts/functions.R")
load("objects/all-objects.RData")


# Attitudes ----
####| Mean Difference ####
dat %>%
  group_by(ma.ingest) %>%
  summarise(mean_cl_normal(duid.att.mean),
            sd = sd(duid.att.mean))

rstatix::t_test(dat, duid.att.mean ~ ma.ingest) %>%
  format_p()


####| Logistic Regression ####
duid.att.df %>%
  filter(id %in% dat$id) %>%
  select(-c(duid.att.risk, duid.att.sanction, duid.att.peer, duid.att.mean, duid.att.full)) %>%
  pivot_longer(cols = -c(id, ma.ingest)) %>%
  mutate(dich.response = value > 4) %>%
  pivot_wider(id_cols = -value, names_from = name, values_from = dich.response)
