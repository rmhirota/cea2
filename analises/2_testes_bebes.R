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


