if(!"packages" %in% ls()) source("scripts/load-packages.R")

survey.files <- list.files("data/", pattern = "*Meth-Survey.csv")
survey.file <- survey.files[length(survey.files)]

#### File moving ####
to.move <- survey.files[!survey.files == survey.file]
old.path <- str_c("data/", to.move)
new.path <- str_c("data/archive/", to.move)

if(!is_empty(to.move)){

  if(file.copy(old.path, new.path)){
    file.remove(old.path)
  }

  # Headers
  survey.headers <- read_csv(str_c("data/", survey.file), n_max = 1) %>%
    names() %>%
    tolower() %>%
    str_replace("\\.\\.\\.", "\\.") %>%
    str_replace("\\_", "\\.") %>%
    str_replace("duration \\(in seconds\\)", "duration")

  survey.raw <- read_csv(str_c("data/", survey.file), skip = 2)

  colnames(survey.raw) <- survey.headers

  survey.raw <- survey.raw %>%
    mutate(id = row_number(),
           .before = 1)

  saveRDS(survey.raw, file = "objects/survey-raw.RData")

}else survey.raw <- readRDS("objects/survey-raw.RData")

.load.data <- logical()
