if(!"packages" %in% ls()){
  source("scripts/load-packages.R")
}

source("scripts/functions.R")

load("objects/all-objects.RData")


# Checking Assumptions ----------------------------------------------------
#### Prep ####
##### Regression Model #####
# # Full Data (including meth use)
# lm.full <- lm(dd.total ~ duid.att.risk + duid.att.sanction + duid.att.peer + ma.ingest, data = dat)

# MA
lm.ma <- lm(dd.total ~ duid.att.risk + duid.att.sanction + duid.att.peer, data = filter(dat, ma.ingest))

# NDU
lm.ndu <- lm(dd.total ~ duid.att.risk + duid.att.sanction + duid.att.peer, data = filter(dat, !ma.ingest))

##### Corrplot #####
duid.vars <- c("duid.att.risk", "duid.att.sanction", "duid.att.peer")

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

duid.cor <- cor(select(dat, all_of(duid.vars), dd.total))
duid.cor.ma <- cor(select(filter(dat, ma.ingest), all_of(duid.vars), dd.total))
duid.cor.ndu <- cor(select(filter(dat, !ma.ingest), all_of(duid.vars), dd.total))

#### Assumption Checks ####
##### Linearity #####
p.full.1 <- autoplot(lm.full, 1) + labs(title = "Full Data") + theme_light()
p.ma.1 <- autoplot(lm.ma, 1) + labs(title = "MA Data") + theme_light()
p.ndu.1 <- autoplot(lm.ndu, 1) + labs(title = "NDU Data") + theme_light()

# ass.path <- "output/regression-assumptions/"
#
# if(FALSE){
#   ggsave(str_c(ass.path, "linearity-ma.svg"), p.ma.1)
# }

dat %>%
  filter(ma.ingest) %>%
  ggplot(aes(x = duid.att.peer, y = dd.total)) +
  geom_jitter(height = 0, width = 0.1) +
  theme_light()

##### Normal Distribution of Residuals #####
p.full.2 <- autoplot(lm.full, 2) + labs(title = "Full Data") +theme_light()
p.ma.2 <- autoplot(lm.ma, 2) + labs(title = "MA Data") + theme_light()
p.ndu.2 <- autoplot(lm.ndu, 2) + labs(title = "NDU Data") + theme_light()

##### Homogeneity of Variance #####
p.full.3 <- autoplot(lm.full, 3) + labs(title = "Full Data") + theme_light()
p.ma.3 <- autoplot(lm.ma, 3) + labs(title = "MA Data") + theme_light()
p.ndu.3 <- autoplot(lm.ndu, 3) + labs(title = "NDU Data") + theme_light()

#### Multicolinearity ####
p.cor.full <- corrplot::corrplot(duid.cor,
                   tl.col = "black", tl.srt = 45,
                   col = col(200), addCoef.col = "black", cl.pos = 'n',
                   type = 'lower')

p.cor.ma <- corrplot::corrplot(duid.cor.ma,
                                   tl.col = "black", tl.srt = 45,
                               col = col(200), addCoef.col = "black", cl.pos = 'n',
                               type = 'lower')

p.cor.ndu <- corrplot::corrplot(duid.cor.ndu,
                                tl.col = 'black', tl.srt = 45,
                                col = col(200), addCoef.col = 'black', cl.pos = 'n',
                                type = 'lower')

#### Regression ####
# Overall Multicolinearity Diagnostics
mctest::omcdiag(lm.ma)

# Individual Multicolinearity Diagnostics
row.names <- c("DUID Risk", "DUID Sanction", "DUID Peer")
## Full
mcl.full <- mctest::imcdiag(lm.full) %>%
  {.$idiags} %>% data.frame() %>%
  select(VIF, TOL) %>%
  add_column(var = row.names, .before = 1) %>%
  tibble()

## MA
mcl.ma <- mctest::imcdiag(lm.ma) %>%
  {.$idiags} %>% data.frame() %>%
  select(VIF, TOL) %>%
  add_column(var = row.names, .before = 1) %>%
  tibble()

## NDU
mcl.ndu <- mctest::imcdiag(lm.ndu) %>%
  {.$idiags} %>% data.frame() %>%
  select(VIF, TOL) %>%
  add_column(var = row.names, .before = 1) %>%
  tibble()

x$idiags

#### Quadratic Model ####




#### Individual Variables with DD ####
p.di.att <- dat %>%
  select(dd.total, ma.ingest, duid.att.risk, duid.att.sanction, duid.att.peer) %>%
  mutate(ma.ingest = if_else(ma.ingest, "MA User", "Non-Drug User")) %>%
  pivot_longer(cols = starts_with("duid.")) %>%

  ggplot(aes(x = value, y = dd.total, col = name)) +
  geom_point(size = 2) +

  facet_grid(ma.ingest~name, scales = "free", space = "free") +

  labs(x = "Attitude Score",
       y = "DDDI Total",
       title = "DDDI Total Score with Attitudes Score by Drug User Group") +


  scale_color_discrete(
    name = element_blank(),
    labels = c("Peer Attitudes", "Attitudes toward Risk", "Attitudes toward Sanctions")
  ) +

  theme_light() +

    theme(
      legend.position = "bottom",
      strip.text.x = element_blank(),
      strip.background.y = element_rect(fill = "grey20")
    )


if(FALSE){
  ggsave("output/plots/dddi-attitudes.svg", p.di.att, width = 13, height = 8)

}



#### Regression Output ####
tidy(lm.ndu, conf.int = TRUE) %>%
  format.p() %>%
  prep.table() %>%
  flextable() %>%
  save_as_docx(path = "output/regression output/ndu-tidy.docx")


dd.total ~ duid.att.risk + duid.att.sanction + duid.att.peer

#### Comparing dems between groups ####
dat.dems %>% select(ma.ingest, age, sex, education, area.live) %>% tbl_summary(by = ma.ingest) %>% add_p()
