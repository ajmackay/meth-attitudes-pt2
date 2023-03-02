if(!"packages" %in% ls()){
  source("scripts/load-packages.R")
}

source("scripts/functions.R")

load("objects/all-objects.RData")



# Completed data ----------------------------------------------------------
#### Demographics ####
dems.ids <- dems.df %>% filter(dems.full) %>% pull(id)

#### SDS ####
sds.ids <- ma.df %>%
  filter(sds.full) %>% pull(id)

#### DDDI ####
dd.ids <- dd.df %>% filter(dd.full) %>% pull(id)

#### Anger ####
state.ids <- state.df %>% filter(state.full) %>% pull(id)

trait.ids <- trait.df %>% filter(trait.full) %>% pull(id)

#### AUDIT ####
audit.ids <- audit.df %>% filter(audit.full) %>% pull(id)

#### K6 Psychological Distress ####
k6.ids <- k6.df %>%
  filter(k6.full) %>%
  pull(id)

#### DUID ####
##### Instances #####
duid.inst.ids <- duid.inst.df %>%
  filter(duid.inst.full) %>% pull(id)

##### Attitudes #####
duid.att.ids <- duid.att.df %>%
  filter(duid.att.full) %>% pull(id)

##### Strategies #####
duid.strat.ids <- duid.strat.df %>%
  filter(duid.strat.full) %>% pull(id)


#### DUI ####
##### Attitudes #####
dui.att.ids <- dui.att.df %>%
  filter(dui.att.full) %>% pull(id)

##### Strategies #####
dui.strat.ids <- dui.strat.df %>%
  filter(dui.strat.full) %>% pull(id)



# Sample ------------------------------------------------------------------
#### Sample of Meth users ####
ma.ids <- dems.df %>%
  filter(
    ma.ingest
    ,dems.full
    ,id %in% sds.ids

    ,id %in% dd.ids
    ,id %in% trait.ids
    ,id %in% state.ids
    ,id %in% audit.ids
    ,id %in% k6.ids


    ,id %in% dui.att.ids
    ,id %in% dui.strat.ids

    # ,id %in% duid.inst.ids
    ,id %in% duid.att.ids
    ,id %in% duid.strat.ids
  ) %>% pull(id)


#### Sample of non-drug users
n.ma.ids <- dems.df %>%
  filter(
    !ma.ingest
    ,dems.full
    ,id %in% dd.ids
    ,id %in% trait.ids
    ,id %in% audit.ids
    ,id %in% k6.ids

    ,id %in% dui.att.ids
    ,id %in% dui.strat.ids

    ,id %in% duid.att.ids
  ) %>% pull(id)

#### TODO Only Attitudes ####
ma.ids <- summ. %>%
  filter(
    ma.ingest
    ,dems.full
    ,id %in% sds.ids

    ,id %in% dd.ids
    ,id %in% trait.ids
    ,id %in% state.ids
    ,id %in% audit.ids
    ,id %in% k6.ids


    ,id %in% dui.att.ids
    ,id %in% dui.strat.ids

    # ,id %in% duid.inst.ids
    ,id %in% duid.att.ids
    ,id %in% duid.strat.ids
  ) %>% pull(id)

# Full dataframe ----------------------------------------------------------
dat <- dems.df %>%
  filter(id %in% c(ma.ids, n.ma.ids)) %>%
  left_join(select(ma.df, id, ma.use.peak, ma.12.months, ma.recent.use, ma.use.age, ma.use.ways, sds.total, ma.type,
                   ma.want.to.change)) %>%
  # DDDI
  left_join(select(dd.df, id, dd.ne.total, dd.ad.total, dd.rd.total, dd.total)) %>%
  # Anger
  left_join(select(trait.df, id, trait.total)) %>%
  left_join(select(state.df, id, state.total)) %>%
  # Alcohol Use (AUDIT)
  left_join(select(audit.df, id, audit.total, audit.risky)) %>%
  # Psychological Distress
  left_join(select(k6.df, id, k6.total, psychiatric.diagnosis)) %>%
  # DUI
  left_join(select(dui.att.df, id, dui.att.risk, dui.att.sanction, dui.att.peer, dui.att.mean)) %>%
  # left_join(select(dui.strat.df, id, dui.strat.total))
  # DUID
  left_join(select(duid.att.df, id, duid.att.risk, duid.att.sanction, duid.att.peer, duid.att.mean))










# Archive -----------------------------------------------------------------

if(FALSE){



duid.att.df %>%   select(-c("duid.att.total", "duid.att.full")) %>%
  filter(ma.ingest,
         id %in% ma.id,
         id %in% dems.ids,
         id %in% dd.ids,
         id %in% state.ids,
         id %in% trait.ids
         ,id %in% audit.ids
         ) %>%
  select(-duid.att.once.while) %>%
  # select(-duid.att.strict) %>%
  # select(-duid.att.jail) %>%
  # select(-duid.att.friends) %>%
  # select(-duid.att.high) %>%

  # select(-duid.att.police) %>%
  add_any_miss() %>% #vis_miss()
  count(any_miss_all)


# Correlation between answers
select(duid.att.df, -c(id, duid.att.total, duid.att.full)) %>%
  add_any_miss() %>% filter(any_miss_all == "complete") %>%
  # ggpairs(showStrips = TRUE)
  ggcorr(label = TRUE,
         label_size = 3)
  # cor(use = "complete.obs", method = "pearson") %>%

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

}
