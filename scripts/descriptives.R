if(!"packages" %in% ls()){
  source("scripts/load-packages.R")
}

source("scripts/functions.R")

load("objects/all-objects.RData")


# Relationship of attitudes to DD -----------------------------------------
duid.dd.dat <- dat %>%
  select(id, starts_with("duid"), starts_with("dd"), ma.ingest) %>%
  pivot_longer(cols = c("duid.att.risk", "duid.att.sanction", "duid.att.peer"),
               names_to = "duid.att.factors", values_to = "duid.att.score") %>%
  pivot_longer(cols = c("dd.ne.total", "dd.ad.total", "dd.rd.total"),
               names_to = "dd.subscales", values_to = "dd.subscale.score")

duid.dd.dat %>%
  ggplot(aes(x = duid.att.score, y = dd.total, col = duid.att.factors)) +
  geom_smooth(method = "lm", size = 1.2, alpha = 0.2) +
  geom_point(alpha = 6) +

  facet_wrap(ma.ingest~duid.att.factors) +
  labs(title = "Dula Total Score by DUID Attitude Score between Groups") +

  ggthemes::scale_color_few() +
  theme_light()

duid.dd.dat %>%
  ggplot(aes(x = duid.att.score, y = dd.subscale.score, col = dd.subscales)) +
  geom_smooth(method = "lm", size = 1.2, alpha = 0.2) +
  geom_point(alpha = 6) +

  facet_wrap(ma.ingest~duid.att.factors) +
  labs(title = "Dula Subscale Score by DUID Attitude Score between Groups") +

  ggthemes::scale_color_few() +
  theme_light()

#### Boxplot ####
duid.dd.dat %>%
  group_by(ma.ingest) %>%
  ggboxplot(x = "duid.att.factors", y = "duid.att.score", fill = "ma.ingest", width = 0.5) +
  labs(title = "DUID Attitude Score Compared Between Groups")
  # stat_pvalue_manual(
  #   pwc, label = "p.adj",
  #   y.position = c(45, 50, 57),
  #   size = 3
  # )

duid.dd.dat %>%
  ggboxplot(x = "dd.subscales", y = "dd.subscale.score", fill = "ma.ingest",
            width = 0.5) +
  labs(title = "Dula Subscale Scores Compared between Groups")




p.subscales <- subscales.df %>%
  mutate(dd.subscale = factor(dd.subscale, levels = c("dd.ad.total", "dd.ne.total", "dd.rd.total"))) %>%

  ggboxplot(x = "dd.subscale", y = "value", width = 0.5) +
  stat_pvalue_manual(
    pwc, label = "p.adj",
    y.position = c(45, 50, 57),
    size = 3
  )

dat %>%
  ggplot(aes(x = k6.total, y = dd.total, col = ma.ingest)) +
  geom_smooth(method = "lm") +
  theme_light()

# DUID Error plot




