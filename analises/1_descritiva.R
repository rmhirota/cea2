library(magrittr)

da_spss <- readr::read_rds("data-raw/da_spss_tidy.rds")
dplyr::glimpse(da_spss)

# Funções -----------------------------------------------------------------

# Média por agrupamento
media_grupo <- function(da, variavel, ...) {
  da %>%
    dplyr::group_by(...) %>%
    dplyr::summarise(
      "media_{{variavel}}" := mean({{variavel}}),
      .groups = "drop"
    )
}
media_grupo(da_spss, n_apertos, dia, grupo)
media_grupo(da_spss, n_apertos, dia)
media_grupo(da_spss, n_apertos, grupo)

# boxplot
boxplot_uni <- function(da, variavel, grupo) {
  da %>%
    ggplot2::ggplot(ggplot2::aes(x = {{grupo}}, y = {{variavel}})) +
    ggplot2::geom_boxplot() +
    ggplot2::theme_light() +
    ggplot2::labs(title = glue::glue(
      "Boxplot de {rlang::enexpr(variavel)} por {rlang::enexpr(grupo)}"
    ))
}

col_dia_grupo_cond <- function(da, variavel) {
  da %>%
    ggplot2::ggplot(ggplot2::aes(x = dia, y = {{variavel}}, fill = grupo)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::facet_wrap(~condicao) +
    ggplot2::theme_light()
}

# Base SPSS ---------------------------------------------------------------

# n_apertos ---------------------------------------------------------------

# Boxplots univariados
boxplot_uni(da_spss, n_apertos, dia) # n_apertos por dia
boxplot_uni(da_spss, n_apertos, grupo) # n_apertos por grupo
boxplot_uni(da_spss, n_apertos, condicao) # n_apertos por condicao

# Média por grupo, condição e dia
da_spss %>%
  media_grupo(n_apertos, grupo, condicao, dia) %>%
  col_dia_grupo_cond(media_n_apertos)

# da_spss %>%
#   media_grupo(n_apertos, nome, grupo, condicao, dia) %>%
#   ggplot2::ggplot(ggplot2::aes(
#     x = grupo, y = media_n_apertos, colour = grupo
#   )) +
#   ggplot2::geom_point() +
#   ggplot2::facet_wrap(~condicao)
#
# da_spss %>%
#   dplyr::mutate(
#     nome = stringr::str_c(nome, grupo),
#     grupo = stringr::str_c("B", grupo),
#     dia = as.factor(dia)
#   ) %>%
#   dplyr::group_by(nome, grupo, condicao, dia) %>%
#   dplyr::summarise(media = mean(n_apertos)) %>%
#   ggplot2::ggplot(ggplot2::aes(
#     x = grupo, y = media, colour = dia
#   )) +
#   ggplot2::geom_point() +
#   ggplot2::facet_wrap(~condicao)


#  freq_apertos -----------------------------------------------------------

# Boxplots univariados
boxplot_uni(da_spss, freq_apertos, dia) # freq_apertos por dia
boxplot_uni(da_spss, freq_apertos, grupo) # freq_apertos por grupo
boxplot_uni(da_spss, freq_apertos, condicao) # freq_apertos por condicao

# Média por grupo, condição e dia
da_spss %>%
  media_grupo(freq_apertos, grupo, condicao, dia) %>%
  col_dia_grupo_cond(media_freq_apertos)


# media_pressao -----------------------------------------------------------

# Boxplots univariados
boxplot_uni(da_spss, media_pressao, dia) # media_pressao por dia
boxplot_uni(da_spss, media_pressao, grupo) # media_pressao por grupo
boxplot_uni(da_spss, media_pressao, condicao) # media_pressao por condicao

# Média por grupo, condição e dia
da_spss %>%
  media_grupo(media_pressao, grupo, condicao, dia) %>%
  col_dia_grupo_cond(media_media_pressao)

# da_spss %>%
#   dplyr::filter(grupo == 3) %>%
#   dplyr::mutate(
#     nome = stringr::str_c(nome, grupo),
#     grupo = stringr::str_c("B", grupo),
#     dia = as.factor(dia)
#   ) %>%
#   dplyr::group_by(nome, grupo, condicao, dia) %>%
#   dplyr::summarise(media = mean(media_pressao)) %>%
#   ggplot2::ggplot(ggplot2::aes(
#     x = dia, y = media, colour = condicao
#   )) +
#   ggplot2::geom_point() +
#   ggplot2::facet_wrap(~condicao)
#
# da_spss %>%
#   dplyr::mutate(
#     nome = stringr::str_c(nome, grupo),
#     grupo = stringr::str_c("B", grupo),
#     dia = as.factor(dia)
#   ) %>%
#   dplyr::group_by(nome, grupo, condicao, dia) %>%
#   dplyr::summarise(media = mean(media_pressao)) %>%
#   ggplot2::ggplot(ggplot2::aes(
#     x = dia, y = grupo, colour = condicao, size = media
#   )) +
#   ggbeeswarm::geom_beeswarm(grouponX = TRUE) +
#   ggplot2::facet_wrap(~condicao)


# pico --------------------------------------------------------------------
# confirmar definição da variável

# Boxplots univariados
boxplot_uni(da_spss, pico, dia) # pico por dia
boxplot_uni(da_spss, pico, grupo) # pico por grupo
boxplot_uni(da_spss, pico, condicao) # pico por condicao

# Média por grupo, condição e dia
da_spss %>%
  media_grupo(pico, grupo, condicao, dia) %>%
  col_dia_grupo_cond(media_pico)

# media_duracao_pico ------------------------------------------------------

# Boxplots univariados
boxplot_uni(da_spss, media_duracao_pico, dia) # media_duracao_pico por dia
boxplot_uni(da_spss, media_duracao_pico, grupo) # media_duracao_pico por grupo
boxplot_uni(da_spss, media_duracao_pico, condicao) # media_duracao_pico por condicao

# Média por grupo, condição e dia
da_spss %>%
  media_grupo(media_duracao_pico, grupo, condicao, dia) %>%
  col_dia_grupo_cond(media_media_duracao_pico)





