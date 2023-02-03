if(!"packages" %in% ls()){
  source("scripts/load-packages.R")
}

source("scripts/functions.R")

load("objects/all-objects.RData")



# Completed data ----------------------------------------------------------
#### Dems ####
dems.ids <- dems.df %>% filter(dems.full) %>% pull(id)

#### DDDI ####
dd.ids <- dd.df %>% filter(dd.full) %>% pull(id)

#### Anger ####
state.ids <- state.df %>% filter(state.full) %>% pull(id)

trait.ids <- trait.df %>% filter(trait.full) %>% pull(id)

#### AUDIT ####
audit.ids <- audit.df %>% filter(audit.full) %>% pull(id)



# N with full attidues ----------------------------------------------------
#### Meth users ####
##### Instances #####
duid.inst.ids <- duid.inst.df %>%
  filter(ma.ingest, duid.inst.full) %>% pull(id)

##### Attitudes #####
duid.att.df %>%
  filter(ma.ingest, id %in% dems.ids, id %in% dd.ids, id %in% state.ids
         # ,id %in% trait.ids
         ) %>%
  select(-c("duid.att.total", "duid.att.full")) %>%
  # select(-duid.att.once.while) %>%
  select(-duid.att.strict) %>%
  # select(-duid.att.jail) %>%
  select(-duid.att.friends) %>%
  # select(-duid.att.high) %>%

  # select(-duid.att.police) %>%
  add_any_miss() %>% #vis_miss()
  count(any_miss_all)


# Correlation between answers
select(duid.att.df, -c(id, duid.att.total, duid.att.full)) %>%
  ggpairs(showStrips = TRUE)
  # cor(use = "complete.obs", method = "pearson") %>% view

duid.att.df %>%
  ggplot(aes(x = duid.att.strict, y = duid.att.jail)) +
  geom_jitter(height = .1, width = .1)



duid.inst.df %>%
  filter(!ma.ingest) %>%
  select(-c("duid.inst.last12months", "duid.inst.before12months", "duid.inst.total")) %>%
  add_any_miss() %>% count(any_miss_all)

duid.att.df %>%
  filter(!ma.ingest) %>%
  select(-duid.att.total) %>%
  add_any_miss() %>% count(any_miss_all)
  vis_miss()

