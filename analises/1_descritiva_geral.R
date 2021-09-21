# Análise descritiva dos dados brutos
library(magrittr)
library(patchwork)

# dados arrumados
da <- readr::read_rds("data-raw/da_tidy.rds")


# Perfis ------------------------------------------------------------------
# Perfil de todos os bebês por grupo/dia para condição contingente
p_video_bebe <- function(da, bebe, d, gr) {
  da_bebe <- da %>%
    dplyr::filter(
      condicao == "contingente", dia == d,
      grupo == gr, nome == bebe
    )
  cor <- switch(d, `1` = "#440154FF", `2` = "#21908CFF", `3` = "#FDE725FF")
  video <- da_bebe %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    dplyr::select(id, video, tempo, pressao) %>%
    dplyr::filter(video)
  # pressao_min <- min(video$id)
  # gatilho <- da_bebe %>%
  #   dplyr::mutate(id = dplyr::row_number()) %>%
  #   dplyr::filter(id < pressao_min) %>%
  #   dplyr::slice_max(id) %>%
  #   dplyr::pull(pressao)
  da_bebe %>%
    ggplot2::ggplot(ggplot2::aes(x = tempo, y = pressao)) +
    ggplot2::geom_line() +
    ggplot2::geom_vline(
      data = video, ggplot2::aes(xintercept = tempo),
      alpha = .01, colour = cor
    ) +
    # ggplot2::geom_hline(yintercept = gatilho, colour = "red") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank() ,
      panel.grid.major.y = ggplot2::element_line(size = .1, color = "black"),
      panel.grid.minor.x = ggplot2::element_blank() ,
      panel.grid.minor.y = ggplot2::element_line(size = .01, color = "black")
    ) +
    ggplot2::labs(
    #   title = "Pressão ao longo do tempo",
    #   subtitle = glue::glue("Grupo {gr} / Dia {d}"),
      y = ggplot2::element_blank(),
      x = ggplot2::element_blank()
    )
}

filtra_nomes <- function(da, grupo, dia) {
  da %>%
    dplyr::filter(grupo == grupo, condicao == "contingente", dia = dia) %>%
    dplyr::pull(nome) %>%
    unique()
}
nomes_b1 <- da %>%
  dplyr::filter(grupo == "b1", condicao == "contingente") %>%
  dplyr::pull(nome) %>% unique()
nomes_b2 <- da %>%
  dplyr::filter(grupo == "b2", condicao == "contingente") %>%
  dplyr::pull(nome) %>% unique()
nomes_b3 <- da %>%
  dplyr::filter(grupo == "b3", condicao == "contingente") %>%
  dplyr::pull(nome) %>% unique()

b1_d1 <- purrr::map(filtra_nomes(da, "b1", 1), ~p_video_bebe(da, .x, 1, "b1"))
b1_d2 <- purrr::map(filtra_nomes(da, "b1", 2), ~p_video_bebe(da, .x, 2, "b1"))
b1_d3 <- purrr::map(filtra_nomes(da, "b1", 3), ~p_video_bebe(da, .x, 3, "b1"))
b2_d1 <- purrr::map(filtra_nomes(da, "b2", 1), ~p_video_bebe(da, .x, 1, "b2"))
b2_d2 <- purrr::map(filtra_nomes(da, "b2", 2), ~p_video_bebe(da, .x, 2, "b2"))
b2_d3 <- purrr::map(filtra_nomes(da, "b2", 3), ~p_video_bebe(da, .x, 3, "b2"))
b3_d1 <- purrr::map(filtra_nomes(da, "b3", 1), ~p_video_bebe(da, .x, 1, "b3"))
b3_d2 <- purrr::map(filtra_nomes(da, "b2", 2), ~p_video_bebe(da, .x, 2, "b3"))
b3_d3 <- purrr::map(filtra_nomes(da, "b3", 3), ~p_video_bebe(da, .x, 3, "b3"))

# grupo 1
dia1 <- purrr::reduce(b1_d1, `/`)
dia2 <- purrr::reduce(b1_d2, `/`)
dia3 <- purrr::reduce(b1_d3, `/`)
grupo1 <- dia1 | dia2 | dia3
ggplot2::ggsave("analises/perfis_grupo1.jpeg", grupo1)

# grupo 2
dia1 <- purrr::reduce(b2_d1, `/`)
dia2 <- purrr::reduce(b2_d2, `/`)
dia3 <- purrr::reduce(b2_d3, `/`)
grupo2 <- dia1 | dia2 | dia3
ggplot2::ggsave(grupo2, "analises/perfis_grupo2.jpeg")

# grupo 3
dia1 <- purrr::reduce(b3_d1, `/`)
dia2 <- purrr::reduce(b3_d2, `/`)
dia3 <- purrr::reduce(b3_d3, `/`)
grupo3 <- dia1 | dia2 | dia3
ggplot2::ggsave(grupo3, "analises/perfis_grupo3.jpeg")


# Pressão média por condição ----------------------------------------------

da %>%
  dplyr::group_by(grupo, condicao, dia) %>%
  dplyr::summarise(media = mean(pressao)) %>%
  ggplot2::ggplot(ggplot2::aes(
    x = dia, y = media, fill = grupo
  ),fill=condicao) +
  ggplot2::geom_bar(stat="identity",position="dodge") +
  ggplot2::facet_wrap(~condicao)+
  #ggplot2::ggtitle("Pressão Média") +
  ggplot2::labs(x = "Dia")+
  ggplot2::labs(y = "Pressão Média")+
  ggplot2::theme_minimal()+
  ggplot2::scale_fill_manual(values=c("#CCCCCC", "#999999", "#666666"))
