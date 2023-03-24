packages <- c("janitor",
              # "table1",
              "gt",
              "naniar",
              "kableExtra",
              # "GGally",
              # "mctest",
              # "stargazer",
              "flextable",
              # "xaringan",
              "DT",
              "patchwork",
              "tidyverse",
              "olsrr", # For regression checking and results
              "broom", # For tidy regression results
              "gtsummary",
              "officer",
              "shadowtext",
              "rstatix", # For ANOVA test and quick summary stats
              "ggpubr", # Publishable graphs
              "gdtools" # For font name
              ,"lavaan" # path analysis (latent varable analysis)
              ,"ggfortify" # Regression assumptions plots as ggplot
              )

librarian::shelf(packages)

message(crayon::green(str_c("Packages Loaded Successfully: ", glue::glue_collapse(packages, sep = ", ", last = " and ") )))
