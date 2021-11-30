library(magrittr)
library(patchwork)

da_spss <- readr::read_rds("data-raw/2_tidy_spss.rds")

boxplot_freq <- function(cond) {
  label_cond <- switch(
    cond,
    bas = "Basal 1", pos = "Basal 2",
    c = "Contingente", nc = "Não contingente"
  )
  da_spss %>%
    dplyr::filter(condicao == cond) %>%
    ggplot2::ggplot(ggplot2::aes(x = grupo, y = freq_apertos, fill = dia)) +
    ggplot2::geom_boxplot(colour = "grey", outlier.colour = "red") +
    ggplot2::scale_fill_viridis_d() +
    ggplot2::labs(x = "Grupo", y = glue::glue("Frequência - {label_cond}")) +
    ggplot2::theme_minimal() +
    ggplot2::facet_wrap(~dia)
}

basal1 <- boxplot_freq("bas")
contingente <- boxplot_freq("c")
naocont <- boxplot_freq("nc")
basal2 <- boxplot_freq("pos")

basal1 / contingente / naocont / basal2



# da_spss %>%
#     # dplyr::filter(condicao == cond) %>%
#     ggplot2::ggplot(ggplot2::aes(x = grupo, y = freq_apertos, fill = condicao)) +
#     ggplot2::geom_boxplot() +
#     ggplot2::scale_fill_viridis_d() +
#     ggplot2::theme_minimal() +
#     ggplot2::facet_wrap(~condicao + dia, nrow = 4)
