library(magrittr)

da_brutos %>%
  dplyr::filter(stringr::str_detect(basename, "parte"))


