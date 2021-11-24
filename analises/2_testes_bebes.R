library(magrittr)

cea2::da_tidy %>% dplyr::distinct(grupo, nome) %>% print(n = 30)

bbs <- list(
  grupo = c("b1", "b2", "b3"),
  nome = c("eduarda", "arthur", "nicolas")
)

teste_ks <- function(g, n) {
  bb <- cea2::da_tidy %>%
  dplyr::filter(
    grupo == g, nome == n, condicao == "contingente", dia == "1"
  )  %>%
  dplyr::arrange(dia, tempo) %>%
  dplyr::filter(status_aperto %in% c("inicio", "fim")) %>%
  dplyr::mutate(diff_aperto = dplyr::case_when(
    status_aperto == "inicio" & dplyr::lag(status_aperto) == "fim"
    ~ (tempo - dplyr::lag(tempo))
  )) %>%
  dplyr::filter(!is.na(diff_aperto)) %>%
  dplyr::pull(diff_aperto)

  fit <- MASS::fitdistr(bb, "exponential")
  ks.test(bb, "pexp", fit$estimate) %>%
    broom::tidy()
}

purrr::map2_dfr(bbs$grupo, bbs$nome, teste_ks) %>%
  dplyr::mutate(bebe = c(1, 2, 3)) %>%
  dplyr::relocate(bebe) %>%
  knitr::kable()


grafico <- function(g, n) {
  bb <- cea2::da_tidy %>%
  dplyr::filter(
    grupo == g, nome == n, condicao == "contingente", dia == "1"
  )  %>%
  dplyr::arrange(dia, tempo) %>%
  dplyr::filter(status_aperto %in% c("inicio", "fim")) %>%
  dplyr::mutate(diff_aperto = dplyr::case_when(
    status_aperto == "inicio" & dplyr::lag(status_aperto) == "fim"
    ~ (tempo - dplyr::lag(tempo))
  )) %>%
  dplyr::filter(!is.na(diff_aperto))
  bb %>%
    ggplot2::ggplot(ggplot2::aes(x = diff_aperto)) +
    ggplot2::geom_histogram(ggplot2::aes(y=..density..),  binwidth = .2) +
    ggplot2::geom_density(col = "red") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Distribuição do tempo entre apertos",
      x = "Tempo entre apertos"
    )
}

purrr::map2(bbs$grupo, bbs$nome, grafico)


# Kruskal-Wallis ----------------------------------------------------------

da_spss <- readr::read_rds("data-raw/2_tidy_spss.rds")

teste_kw <- function(cond, d) {
  da <- da_spss %>%
    dplyr::filter(condicao == cond, dia == d) %>%
    dplyr::select(grupo, freq_apertos)
  g1 <- dplyr::filter(da, grupo == 1)
  g2 <- dplyr::filter(da, grupo == 2)
  g3 <- dplyr::filter(da, grupo == 3)
  kruskal.test(list(
    g1$freq_apertos, g2$freq_apertos, g3$freq_apertos
  ))
}

# Contingente
teste_kw("c", 1)  # dia 1
teste_kw("c", 2)  # dia 2
teste_kw("c", 3)  # dia 3

# Basal 1
teste_kw("bas", 1)  # dia 1
teste_kw("bas", 2)  # dia 2
teste_kw("bas", 3)  # dia 3

# Basal 2
teste_kw("pos", 1)  # dia 1
teste_kw("pos", 2)  # dia 2
teste_kw("pos", 3)  # dia 3

# Não contingente
teste_kw("nc", 1)  # dia 1
teste_kw("nc", 2)  # dia 2
teste_kw("nc", 3)  # dia 3


# Wilcoxon ----------------------------------------------------------------

# Wilcoxon: Comparação entre dias (basal 2 - basal 1)
da_spss <- readr::read_rds("data-raw/2_tidy_spss.rds")

da_wilcoxon <- da_spss %>%
  dplyr::filter(condicao %in% c("bas", "pos")) %>%
  dplyr::mutate(id = paste0(nome, grupo)) %>%
  dplyr::select(id, dia, condicao, freq_apertos) %>%
  tidyr::pivot_wider(names_from = condicao, values_from = freq_apertos) %>%
  dplyr::mutate(dif_basal = (pos - bas)/bas)

teste_w <- function(dia1, dia2, alt = "two.sided") {
  d1 <- da_wilcoxon %>%
    dplyr::filter(dia == dia1) %>%
    dplyr::pull(dif_basal)
  d2 <- da_wilcoxon %>%
    dplyr::filter(dia == dia2) %>%
    dplyr::pull(dif_basal)
  wilcox.test(d1, d2, paired = TRUE, alternative = alt)
}

# teste bicaudal
teste_w(1, 2)  # diferença entre dias 1 e 2
teste_w(1, 3)  # diferença entre dias 1 e 3
teste_w(2, 3)  # diferença entre dias 2 e 3

# teste H_1: Md1 > Md2
teste_w(1, 2, "greater")  # diferença entre dias 1 e 2
teste_w(1, 3, "greater")  # diferença entre dias 1 e 3
teste_w(2, 3, "greater")  # diferença entre dias 2 e 3

# teste H_1: Md1 < Md2
teste_w(1, 2, "less")  # diferença entre dias 1 e 2
teste_w(1, 3, "less")  # diferença entre dias 1 e 3
teste_w(2, 3, "less")  # diferença entre dias 2 e 3



