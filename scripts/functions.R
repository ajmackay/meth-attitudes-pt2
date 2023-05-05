#### Flextable defaults ####
set.default.ft <- function(font.family = 'Calibri',
                           font.size = 10,
                           layout = 'autofit',
                           padding = 6,
                           theme_fun = 'theme_vanilla',
                           digits = 3, ...) {

  flextable::set_flextable_defaults(
    font.family = font.family,
    font.size = font.size,
    layout = layout,
    padding = padding,
    theme_fun = theme_fun,
    digits = digits)

  if(digits != 3){
    cat(crayon::red("Use colformat_double() to adjust digits"))
  }

  cat(crayon::green("Flextable Default Set"))

}

#### decimal places ####
#### Prep Table ####
prep.table <- function(dat, dp = 2){
  # Turning names into title
  # dat.names <- names(dat)
  # already.up <- str_detect(x, "^[:upper:]+$")
  # str_to_title(dat.names[!already.up])
  dat <- dat %>%
    mutate(across(where(is.numeric), ~round(.x, dp)))

  dat
}

#### Format p ####
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

#### Stop Quietly ####
stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}

#### Margin of error calculator ####
margin.error <- function(level = 0.975, sd, n){
  # browser()
  qnorm(level) * sd / sqrt(n)
}



#### Save Objects ####
save.objects <- function(path = NULL) {
  if(is.null(path)){
    path <- "objects/all-objects.RData"
    save.image(file = path)
  } else{
    path <- path
    save.image(file = path)
  }

  message(crayon::green(str_glue("Environment saved at ", crayon::black("{path}"))))

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







# Table -------------------------------------------------------------------
# https://cran.r-project.org/web/packages/table1/vignettes/table1-examples.html#example-a-column-of-p-values


