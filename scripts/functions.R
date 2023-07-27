
save.figs.x <- function(dir = 'objects/', name = 'figs-tables'){
  browser()
  full.dir <- str_c(dir, name, ".RData")
  save(list = ls()[str_detect(ls(), "plt\\.|tbl\\.|fig\\.")], file = full.dir, envir = .GlobalEnv)
  cat(crayon::green(str_glue("Figures and Tables saved in {dir}{name}.RData")))
}

tbl.plts <- ls()[str_detect(ls(), "plt\\.|tbl\\.|fig\\.")]

save(list = tbl.plts, file = "objects/figs-tables.RData", envir = .GlobalEnv)

### THIS IS WEIRD THAT IS DOESN'T WORK
x <- function(){
save(list = ls()[str_detect(ls(), "plt\\.|tbl\\.|fig\\.")], file = "objects/figs-tables.RData", envir = .GlobalEnv)
}






#### save.table ####



format.p <- function(dat){
  if(!any(str_detect(class(dat), "tbl"))){

    as_tibble(dat) %>%
      mutate(p = scales::pvalue(p))
  }
}

format.p <- function(dat, p.value = p.value){

  dat %>%
    mutate(p.value = scales::pvalue(p.value))
}



# format.p(anova.tbl)

# any(str_detect(class(anova.tbl), "data.frame"))


#### Margin of error calculator ####
margin.error <- function(level = 0.975, sd, n){
  # browser()
  qnorm(level) * sd / sqrt(n)
}



#### Effect Size Calculator ####
lm.effect.size <- function(dat, iv, dv){
  # browser()
  part.cor <- ppcor::spcor(select(dat, !!enquo(iv), !!enquo(dv)))
  partial.cor <- ppcor::pcor(select(dat, !!enquo(iv), !!enquo(dv)))

  message(str_c("Dependent Variable: ", dv))

  tibble(
    Variable = c(iv, dv),
    Part = data.frame(part.cor$estimate) %>% pull(!!enquo(dv)),
    Partial = data.frame(partial.cor$estimate) %>% pull(!!enquo(dv)),
    F2 = (Part^2) / (1 - Part^2)
  ) %>%
    filter(Variable != dv)
}


# Model comparison

tmp <- function(models){
  map(models, function(models){
    browser()
    summ <- summary(models)
    tibble(
      dv = summ$terms[[2]]
    )
  # tibble(dv = summary(models)$terms[[2]])


  })
}


if(FALSE){
  summ.tbl <- function(dat, summ.by = NULL, dp = 1, caption = NULL){
    tbl_summary(dat,
                by = summ.by,
                statistic = list(all_continuous() ~ "{mean} ({sd})"),
                digits = list(
                  all_continuous() ~ dp,
                  all_categorical() ~ dp
                ),
                missing_text = "Missing",
                sort = list(everything() ~ "frequency")) %>%
      bold_labels() %>%
      as_flex_table() %>%
      fontsize(size = 10) %>%
      theme_zebra() %>%
      set_caption(caption) %>%
      set_table_properties(layout = "autofit")
  }

}






# Table -------------------------------------------------------------------
# https://cran.r-project.org/web/packages/table1/vignettes/table1-examples.html#example-a-column-of-p-values


