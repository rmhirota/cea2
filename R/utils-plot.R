#' Gráfico de pressao ao longo do tempo
#'
#' @param da dados [da_tidy]
#' @param bebe nome do bebe
#' @param d dia
#' @param gr grupo (b1, b2, b3)
#'
#' @export
#'
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


#' Filtra nomes de bebês que fizeram condição contingente para dado grupo e dia
#'
#' @param da dados [da_tidy]
#' @param grupo grupo (b1, b2, b3)
#' @param dia 1, 2 ou 3
#'
#' @export
#'
filtra_nomes <- function(da, grupo, dia) {
  da %>%
    dplyr::filter(grupo == grupo, condicao == "contingente", dia = dia) %>%
    dplyr::pull(nome) %>%
    unique()
}
