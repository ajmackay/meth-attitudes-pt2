if(!"packages" %in% ls()){
  source("scripts/load-packages.R")
}

source("scripts/functions.R")

load("objects/all-objects.RData")

#### Attitudes boxplot ####
dat.long <- dat %>%
  pivot_longer(cols = c(duid.att.risk, duid.att.sanction, duid.att.peer), names_to = "att.subscale") %>%
  mutate(
    # att.subscale = factor(att.subscale, levels = c("duid.att.risk", "duid.att.sanction", "duid.att.peer")),
    att.subscale = case_when(
      att.subscale == "duid.att.risk" ~ "Risks",
      att.subscale == "duid.att.sanction" ~ "Sanctions",
      att.subscale == "duid.att.peer" ~ "Peer Attitudes"
    ),
    group = if_else(ma.ingest, "Methamphetamine Users", "Non-Drug Users"))

stat.test <- dat.long %>%
  group_by(att.subscale) %>%
  t_test(value ~ group) %>%
  adjust_pvalue() %>%
  add_significance("p.adj") %>%
  mutate(p.adj = scales::pvalue(p.adj))


p.att.score <- ggboxplot(dat.long, x = "att.subscale", y = "value", width = 0.5, fill = "group") +
  stat_pvalue_manual(stat.test, x = "att.subscale", y.position = 7.5, label = "p.adj") +
  labs(fill = element_blank(),
       x = "Attitude Factor",
       y = "Score") +
  scale_fill_brewer(palette = "OrRd") +
  scale_y_continuous(breaks = seq(2, 7, 1)) +

  theme(axis.title = element_text(face = 'bold'))

ggsave(p.att.score, width = 900, height = 700, units = 'px', dpi = 95, filename = "output/attitude-boxplot.png")



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






dat %>%
  mutate(dd.subscale = factor(dd.subscale, levels = c("dd.ad.total", "dd.ne.total", "dd.rd.total"))) %>%

  ggboxplot(x = "dd.subscale", y = "value", width = 0.5, color = "ma.ingest")
stat_pvalue_manual(
  pwc, label = "p.adj",
  y.position = c(45, 50, 57),
  size = 3
)
