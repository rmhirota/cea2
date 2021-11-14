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

# Wilcoxon: Comparação entre dias
da_spss <- readr::read_rds("data-raw/2_tidy_spss.rds")

dia1 = as.matrix(da_spss %>% dplyr::filter(dia==1) %>% dplyr::select(freq_apertos))
dia2 = as.matrix(da_spss %>% dplyr::filter(dia==2) %>% dplyr::select(freq_apertos))
dia3 = as.matrix(da_spss %>% dplyr::filter(dia==3) %>% dplyr::select(freq_apertos))

teste_w <- function(d, n) {
  wilcox.test(d,n)
}

teste_w(dia1,dia2) #p-value = 0.2476, não rejeita a igualdade
teste_w(dia1,dia3) #p-value = 0.3803, não rejeita a igualdade
teste_w(dia2,dia3) #p-value = 0.6977, não rejeita a igualdade

# Wilcoxon: Comparação entre grupos

grupo1 = as.matrix(da_spss %>% dplyr::filter(grupo==1) %>% dplyr::select(freq_apertos))
grupo2 = as.matrix(da_spss %>% dplyr::filter(grupo==2) %>% dplyr::select(freq_apertos))
grupo3 = as.matrix(da_spss %>% dplyr::filter(grupo==3) %>% dplyr::select(freq_apertos))


teste_w(grupo1,grupo2) #p-value = p-value = 0.004112, nrejeita-se a igualdade
teste_w(grupo1,grupo3) #p-value = 0.008848, não rejeita a igualdade
teste_w(grupo2,grupo3) # p-value = 0.8918, não rejeita a igualdade

