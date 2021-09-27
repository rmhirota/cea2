# Análise descritiva dos dados brutos
library(magrittr)
library(patchwork)

# dados arrumados
da <- readr::read_rds("data-raw/da_tidy.rds")


# Perfis ------------------------------------------------------------------
# Perfil de todos os bebês por grupo/dia para condição contingente

nomes_b1 <- cea2::da_tidy %>%
  dplyr::filter(grupo == "b1", condicao == "contingente") %>%
  dplyr::pull(nome) %>% unique()
nomes_b2 <- cea2::da_tidy %>%
  dplyr::filter(grupo == "b2", condicao == "contingente") %>%
  dplyr::pull(nome) %>% unique()
nomes_b3 <- cea2::da_tidy %>%
  dplyr::filter(grupo == "b3", condicao == "contingente") %>%
  dplyr::pull(nome) %>% unique()

b1_d1 <- purrr::map(cea2::filtra_nomes(da, "b1", 1), ~cea2::p_video_bebe(da, .x, 1, "b1"))
b1_d2 <- purrr::map(cea2::filtra_nomes(da, "b1", 2), ~cea2::p_video_bebe(da, .x, 2, "b1"))
b1_d3 <- purrr::map(cea2::filtra_nomes(da, "b1", 3), ~cea2::p_video_bebe(da, .x, 3, "b1"))
b2_d1 <- purrr::map(cea2::filtra_nomes(da, "b2", 1), ~cea2::p_video_bebe(da, .x, 1, "b2"))
b2_d2 <- purrr::map(cea2::filtra_nomes(da, "b2", 2), ~cea2::p_video_bebe(da, .x, 2, "b2"))
b2_d3 <- purrr::map(cea2::filtra_nomes(da, "b2", 3), ~cea2::p_video_bebe(da, .x, 3, "b2"))
b3_d1 <- purrr::map(cea2::filtra_nomes(da, "b3", 1), ~cea2::p_video_bebe(da, .x, 1, "b3"))
b3_d2 <- purrr::map(cea2::filtra_nomes(da, "b2", 2), ~cea2::p_video_bebe(da, .x, 2, "b3"))
b3_d3 <- purrr::map(cea2::filtra_nomes(da, "b3", 3), ~cea2::p_video_bebe(da, .x, 3, "b3"))

# grupo 1
dia1 <- purrr::reduce(b1_d1, `/`)
dia2 <- purrr::reduce(b1_d2, `/`)
dia3 <- purrr::reduce(b1_d3, `/`)
grupo1 <- dia1 | dia2 | dia3
ggplot2::ggsave("analises/perfis_grupo1.png", grupo1)
ggplot2::ggsave("pres/plots/perfis_grupo1.png", grupo1)

# grupo 2
dia1 <- purrr::reduce(b2_d1, `/`)
dia2 <- purrr::reduce(b2_d2, `/`)
dia3 <- purrr::reduce(b2_d3, `/`)
grupo2 <- dia1 | dia2 | dia3
ggplot2::ggsave("analises/perfis_grupo2.png", grupo2)
ggplot2::ggsave("pres/plots/perfis_grupo2.png", grupo2)

# grupo 3
dia1 <- purrr::reduce(b3_d1, `/`)
dia2 <- purrr::reduce(b3_d2, `/`)
dia3 <- purrr::reduce(b3_d3, `/`)
grupo3 <- dia1 | dia2 | dia3
ggplot2::ggsave("analises/perfis_grupo3.png", grupo3)
ggplot2::ggsave("pres/plots/perfis_grupo3.png", grupo3)


# Pressão média por condição ----------------------------------------------

p_media = da %>%
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
  ggplot2::scale_fill_manual(values=c("#440154FF", "#21908CFF", "#FDE725FF"))

ggplot2::ggsave("analises/pressao_media.jpeg", p_media)

# variavel tempo entre disparo do video

tempo_video <- function(da, d, gr, b) {
  cor <- switch(d, `1` = "#440154FF", `2` = "#21908CFF", `3` = "#FDE725FF")
  da %>%
    dplyr::filter(condicao == "contingente", dia == d, grupo == gr, nome == b) %>%
    dplyr::arrange(tempo) %>%
    dplyr::mutate(status_video = dplyr::case_when(video == FALSE & dplyr::lag(video) == TRUE  ~ "fim",
                                                  video == TRUE & dplyr::lag(video) == FALSE ~ "inicio",
                                                  video == TRUE & dplyr::lag(video) == TRUE ~ "meio")) %>%
    dplyr::filter(status_video %in% c("inicio","fim")) %>%
    dplyr::mutate(diff_video = dplyr::case_when(status_video == "inicio" &
                                                  dplyr::lag(status_video) == "fim" ~ (tempo-dplyr::lag(tempo)))) %>%
    ggplot2::ggplot(ggplot2::aes(
      x = nome, y = diff_video)) +
    ggplot2::geom_boxplot(color=cor) +
    #ggplot2::xlab("Bebê") +
    #ggplot2::ylab("Tempo")+
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      legend.position="none"
    ) +
    ggplot2::labs(
      #   title = "Pressão ao longo do tempo",
      #   subtitle = glue::glue("Grupo {gr} / Dia {d}"),
      y = ggplot2::element_blank(),
      x = ggplot2::element_blank(),
      axis.title.x= ggplot2::element_blank()
    )
}

## grupo 1
t_g1_d1 = purrr::map(nomes_b1, ~tempo_video(cea2::da_tidy,1, "b1",.x))
purrr::reduce(t_g1_d1,`+`)
t_g1_d2 = purrr::map(nomes_b1, ~tempo_video(cea2::da_tidy,2, "b1",.x))
purrr::reduce(t_g1_d2,`+`)
t_g1_d3 = purrr::map(nomes_b1, ~tempo_video(cea2::da_tidy,3, "b1",.x))
purrr::reduce(t_g1_d3,`+`)

ggplot2::ggsave("analises/tempo_video_g1_d1.jpeg", purrr::reduce(t_g1_d1,`+`))
ggplot2::ggsave("analises/tempo_video_g1_d2.jpeg", purrr::reduce(t_g1_d2,`+`))
ggplot2::ggsave("analises/tempo_video_g1_d3.jpeg", purrr::reduce(t_g1_d3,`+`))


## grupo 2
t_g2_d1 = purrr::map(nomes_b2, ~tempo_video(cea2::da_tidy,1, "b2",.x))
purrr::reduce(t_g2_d1,`+`)
t_g2_d2 = purrr::map(nomes_b2, ~tempo_video(cea2::da_tidy,2,"b2",.x))
purrr::reduce(t_g2_d2,`+`)
t_g2_d3 = purrr::map(nomes_b2, ~tempo_video(cea2::da_tidy,3,"b2",.x))
purrr::reduce(t_g2_d3,`+`)

ggplot2::ggsave("analises/tempo_video_g2_d1.jpeg", purrr::reduce(t_g2_d1,`+`))
ggplot2::ggsave("analises/tempo_video_g2_d2.jpeg", purrr::reduce(t_g2_d2,`+`))
ggplot2::ggsave("analises/tempo_video_g2_d3.jpeg", purrr::reduce(t_g2_d3,`+`))

## grupo 3
t_g3_d1 = purrr::map(nomes_b3, ~tempo_video(cea2::da_tidy,1, "b3",.x))
purrr::reduce(t_g3_d1,`/`)
t_g3_d2 = purrr::map(nomes_b3, ~tempo_video(cea2::da_tidy,2,"b3",.x))
purrr::reduce(t_g3_d2,`+`)
t_g3_d3 = purrr::map(nomes_b3, ~tempo_video(cea2::da_tidy,3,"b3",.x))
purrr::reduce(t_g3_d3,`+`)

ggplot2::ggsave("analises/tempo_video_g3_d1.jpeg", purrr::reduce(t_g3_d1,`+`))
ggplot2::ggsave("analises/tempo_video_g3_d2.jpeg", purrr::reduce(t_g3_d2,`+`))
ggplot2::ggsave("analises/tempo_video_g3_d3.jpeg", purrr::reduce(t_g3_d3,`+`))

# variavel tempo entre disparo do video - por dia

  da %>% dplyr::filter(condicao == "contingente") %>%
    dplyr::mutate(rank = dplyr::row_number(tempo)) %>%
    dplyr::group_by(grupo, nome, dia, condicao) %>%
    dplyr::mutate(status_video = dplyr::case_when(video == FALSE & dplyr::lag(video) == TRUE  ~ "fim",
                                                  video == TRUE & dplyr::lag(video) == FALSE ~ "inicio",
                                                  video == TRUE & dplyr::lag(video) == TRUE ~ "meio")) %>%
    dplyr::filter(status_video %in% c("inicio","fim")) %>%
    dplyr::mutate(diff_video = dplyr::case_when(status_video == "inicio" &
                                                  dplyr::lag(status_video) == "fim" ~ (tempo-dplyr::lag(tempo)))) %>%

    ggplot2::ggplot(ggplot2::aes(
      x = dia, y = diff_video, fill = dia)) +
    ggplot2::geom_boxplot() +
    #ggplot2::xlab("Bebê") +
    #ggplot2::ylab("Tempo")+
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position="none"
    ) +
    ggplot2::labs(
      #   title = "Pressão ao longo do tempo",
      #   subtitle = glue::glue("Grupo {gr} / Dia {d}"),
      y = ggplot2::element_blank(),
      x = ggplot2::element_blank(),
      axis.title.x= ggplot2::element_blank()
    ) + ggplot2::theme_minimal() +
    ggplot2::scale_fill_manual(values=c("#440154FF", "#21908CFF", "#FDE725FF"))

