if(!"packages" %in% ls()){
  source("scripts/load-packages.R")
}

source("scripts/functions.R")

load("objects/all-objects.RData")


# Summary of Data ---------------------------------------------------------
t.summ <- select(dat, -c(id, dd.ne.total, dd.ad.total, dd.rd.total)) %>%
  mutate(ma.ingest = if_else(ma.ingest, "MA User", "Non-Drug User")) %>%
  tbl_summary(by = ma.ingest,
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              label = list(dd.total ~ "DDDI Total",
                           duid.att.mean ~ "DUID Attitude Mean",
                           duid.att.risk ~ "DUID Attitude (Risk)",
                           duid.att.sanction ~ "DUID Attitude (Sanction)",
                           duid.att.peer ~ "DUID Attitude (Peer)")) %>% add_p()

t.summ

#### Attitudes boxplot ####
dat.long <- dat %>%
  pivot_longer(cols = c(duid.att.risk, duid.att.sanction, duid.att.peer), names_to = "att.subscale") %>%
  mutate(att.subscale = factor(att.subscale, levels = c("duid.att.risk", "duid.att.sanction", "duid.att.peer")))

stat.test <- dat.long %>%
  group_by(att.subscale) %>%
  t_test(value ~ ma.ingest) %>%
  adjust_pvalue() %>%
  add_significance("p.adj") %>%
  mutate(p.adj = scales::pvalue(p.adj))


ggboxplot(dat.long, x = "att.subscale", y = "value", width = 0.5, fill = "ma.ingest") +
  stat_pvalue_manual(stat.test, x = "att.subscale", y.position = 8, label = "p.adj")
  stat_pvalue_manual(
    pwc, label = "p.adj",
    y.position = c(8, 10, 9)
  )

dat %>%
  pivot_longer(cols = c(duid.att.risk, duid.att.sanction, duid.att.peer), names_to = "att.subscale") %>%
  mutate(att.subscale = factor(att.subscale, levels = c("duid.att.risk", "duid.att.sanction", "duid.att.peer"))) %>%

  ggboxplot(x = "att.subscale", y = "value", width = 0.5, fill = "ma.ingest") +
  stat_pvalue_manual(
    stat.test, x = "att.subscale", label = "p.adj",
    y.position = c(8, 10, 9)
  )

dat %>%
  mutate(dd.subscale = factor(dd.subscale, levels = c("dd.ad.total", "dd.ne.total", "dd.rd.total"))) %>%

  ggboxplot(x = "dd.subscale", y = "value", width = 0.5, color = "ma.ingest")
  stat_pvalue_manual(
    pwc, label = "p.adj",
    y.position = c(45, 50, 57),
    size = 3
  )



