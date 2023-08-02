if (!"packages" %in% ls()) source("scripts/load-packages.R")

source("scripts/functions.R")

load("objects/all-objects.RData")




#### Questionnaire Psychemetrics ####




#### Sample Characteristics ####
##### Demographics #####
tbl.dems <- dat.dems %>%
  mutate(ma.ingest = if_else(ma.ingest, "MA User", "Non-Drug User"),
         any.psychiatric.diagnosis = if_else(any.psychiatric.diagnosis, "Yes", "No")) %>%
  select(ma.ingest, Age = age, Sex = sex, education,  ever.used.alcohol = alcohol.ever, audit.total,
         employment.status, license.status, psychiatric.diagnosis = any.psychiatric.diagnosis, residential.area = area.live) %>%
  prep.names() %>%
  flex.summary(summ.by = "Ma Ingest", include.p = !TRUE, dp = 2)

#### Methamphetamine Use #####
tbl.ma.use <- ma.df %>%
  filter(id %in% dat$id) %>%
  select(ma.use.peak, ma.use.age, ma.use.ways, sds.total, ma.type) %>%
  prep.names() %>%
  flex.summary(dp = 2)

# AUDIT Scores
audit.df %>%
  filter(id %in% dat$id) %>%
  group_by(ma.ingest) %>%
  count(audit.risky) %>%
  mutate(p = scales::percent(n / sum(n), .01))


if (FALSE) {
  save_as_docx(tbl.dems, path = "output/tables/demographics.docx")
  flex2xl(tbl.dems, "output/tables/demographics.xlsx")

  save_as_docx(tbl.ma.use, path = "output/tables/ma-use.docx")
}


#### Attitudes boxplot ####
dat.long <- dat %>%
  pivot_longer(cols = c(duid.att.risk, duid.att.sanction, duid.att.peer), names_to = "att.subscale") %>%
  mutate(
    # att.subscale = factor(att.subscale, levels = c("duid.att.risk", "duid.att.sanction", "duid.att.peer")),
    att.subscale = case_when(
      att.subscale == "duid.att.risk" ~ "Favourable Attitude to Risks",
      att.subscale == "duid.att.sanction" ~ "Unfavourable Attitude to \nSanctions",
      att.subscale == "duid.att.peer" ~ "Favourable Peer Attitudes"
    ),
    group = if_else(ma.ingest, "Methamphetamine Users", "Non-Drug Users")
  )

stat.test <- dat.long %>%
  group_by(att.subscale) %>%
  t_test(value ~ group) %>%
  adjust_pvalue() %>%
  add_significance("p.adj") %>%
  mutate(p.adj = scales::pvalue(p.adj))


plt.att.score <- ggboxplot(dat.long, x = "att.subscale", y = "value", width = 0.5, fill = "group") +
  stat_pvalue_manual(stat.test, x = "att.subscale", y.position = 7.5, label = "p.adj") +
  labs(
    fill = element_blank(),
    x = "Attitude Factor",
    y = "Score"
  ) +
  scale_fill_brewer(palette = "OrRd") +
  scale_y_continuous(breaks = seq(2, 7, 1)) +

  theme(axis.title = element_text(face = "bold"),
        axis.text = element_text(size = 10))

if (FALSE) {
  ggsave(p.att.score, width = 900, height = 700, units = "px", dpi = 95, filename = "output/attitude-boxplot.png")
}

#### Dangerous Driving Boxplot ####
test.dd.total <- dat %>%
  mutate(ma.ingest = if_else(ma.ingest, "Methamphetamine User", "Non-Drug User")) %>%
  t_test(dd.total ~ ma.ingest) %>%
  mutate(p = scales::pvalue(p))

plt.dd.total <- mutate(dat, ma.ingest = if_else(ma.ingest, "Methamphetamine User", "Non-Drug User") %>%
         factor(levels = c("Methamphetamine User", "Non-Drug User"))) %>%
ggboxplot(x = "ma.ingest", y = "dd.total", width = 0.5, fill = 'ma.ingest') +
  stat_pvalue_manual(test.dd.total, y = 100, label = "p") +

  scale_fill_brewer(palette = "OrRd") +

  labs(x = element_blank(),
       y = "DDDI Total") +
  theme(legend.position = "none")


# Summary of Data ---------------------------------------------------------
t.summ <- select(dat, -c(id, dd.ne.total, dd.ad.total, dd.rd.total)) %>%
  mutate(ma.ingest = if_else(ma.ingest, "MA User", "Non-Drug User")) %>%
  tbl_summary(
    by = ma.ingest,
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    label = list(
      dd.total ~ "DDDI Total",
      duid.att.mean ~ "DUID Attitude Mean",
      duid.att.risk ~ "DUID Attitude (Risk)",
      duid.att.sanction ~ "DUID Attitude (Sanction)",
      duid.att.peer ~ "DUID Attitude (Peer)"
    )
  ) %>%
  add_p()


#### Psychometric Properties ####
##### Reliability #####
tbl.alphas <- as_tibble(alpha.risk$total) %>%
  mutate(Factor = 'Risk') %>%
  select(alpha = std.alpha, Factor) %>%

  bind_rows(
    as_tibble(alpha.sanctions$total) %>%
      mutate(Factor = 'Sanctions') %>%
      select(alpha = std.alpha, Factor)
  ) %>%

  bind_rows(
    as_tibble(alpha.peer$total) %>%
      mutate(Factor = 'Peer') %>%
      select(alpha = std.alpha, Factor)
  ) %>%
  prep.flex() %>%
  colformat_double(digits = 2)

##### CFA #####
tbl.cfa.loadings <- cfa.loadings %>%
  mutate(Variable = str_to_title(Variable),
         across(where(is.numeric), ~as.character(.x) %>% str_replace("^0$", "-"))) %>%
  prep.flex() %>% colformat_double(digits = 2)


#### Model Coefficients ####
tbl.model.comparison <- anova(mgp.free, mgp.constrained) %>% broom::tidy() %>%
  mutate(p.value = scales::pvalue(p.value)) %>%
  ft.prep() %>% set_caption("Chi Square: Free vs Constrained Model")

# Model-Wide Interactions:
tbl.model.int <- multigroup.model$anovaInts %>%
  as_tibble(.name_repair = "unique") %>%
  mutate(P.Value = scales::pvalue(P.Value)) %>%
  select(Response, Predictor, Test.Stat, DF, P.Value) %>% ft.prep() %>% set_caption("Model-Wide Interactions", align_with_table = TRUE)

mga.coefs <- map(multigroup.model$group.coefs, function(dat){
  # browser()
  dat <- as_tibble(dat, .name_repair = "unique")
  mutate(dat,
         p = scales::pvalue(as.numeric(P.Value)),
         across(.cols = c(Estimate, Std.Error, DF, Crit.Value), ~as.numeric(.x))) %>%
    select(Response, Predictor, Estimate, Std.Error, DF, p)
})

tbl.mga.ndu <- mga.coefs$`FALSE` %>%
  mutate(across(where(is.numeric), ~round(.x, 2))) %>%
  ft.prep(digits = 2) %>% set_caption("Non-Drug Users")


tbl.mga.mu <- mga.coefs$`TRUE` %>% ft.prep(digits = 2) %>% set_caption("Methamphetamine Users")

if(FALSE){
  save_as_docx(tbl.mga.ndu, path = "output/tables/non-drug-user-regression.docx")
  save_as_docx(tbl.mga.mu, path = "output/tables/methamphetamine-user-regression.docx")
}

dat %>%
  mutate(dd.subscale = factor(dd.subscale, levels = c("dd.ad.total", "dd.ne.total", "dd.rd.total"))) %>%
  ggboxplot(x = "dd.subscale", y = "value", width = 0.5, color = "ma.ingest")
stat_pvalue_manual(
  pwc,
  label = "p.adj",
  y.position = c(45, 50, 57),
  size = 3
)


save(list = ls()[str_detect(ls(), "plt\\.|tbl\\.|fig\\.")], file = "objects/figs-tables.RData", envir = .GlobalEnv)
