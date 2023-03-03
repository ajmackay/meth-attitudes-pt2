if(!"packages" %in% ls()){
  source("scripts/load-packages.R")
}

source("scripts/functions.R")

load("objects/all-objects.RData")


# Summary of Data ---------------------------------------------------------
t.summ <- select(dat, -c(id, dd.ne.total, dd.ad.total, dd.rd.total, duid.att.mean)) %>%
  mutate(ma.ingest = if_else(ma.ingest, "MA User", "Non-Drug User")) %>%
  tbl_summary(by = ma.ingest,
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              label = list(dd.total ~ "DDDI Total",
                           duid.att.risk ~ "DUID Attitude (Risk)",
                           duid.att.sanction ~ "DUID Attitude (Sanction)",
                           duid.att.peer ~ "DUID Attitude (Peer)")) %>%
  as_flex_table()


