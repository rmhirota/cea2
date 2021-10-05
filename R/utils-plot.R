#' Gráfico de pressao ao longo do tempo
#'
#' @param da dados [da_tidy]
#' @param bebe nome do bebe
#' @param d dia
#' @param gr grupo (b1, b2, b3)
#'
#' @export
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
#' @param gr grupo (b1, b2, b3)
#' @param d 1, 2 ou 3
#'
#' @export
filtra_nomes <- function(da, gr, d) {
  da %>%
    dplyr::filter(grupo == gr, condicao == "contingente", dia == d) %>%
    dplyr::pull(nome) %>%
    unique()
}


#' Gráfico de pressão média por condição basal e contingente
#'
#' @param g grupo (b1, b2, b3)
#'
#' @export
hc_grupo <- function(g) {
  cea2::da_tidy %>%
    dplyr::filter(
      condicao %in% c("basal1", "contingente"),
      grupo == g, pressao < 2
    ) %>%
    dplyr::mutate(tempo = floor(tempo*10)/10) %>%
    dplyr::group_by(condicao, tempo) %>%
    dplyr::summarise(
      pressao_media = mean(pressao, na.rm= TRUE),
      .groups = "drop"
    ) %>%
    highcharter::hchart(
      "line",
      highcharter::hcaes(tempo, pressao_media, group = condicao)
    ) %>%
    highcharter:: hc_xAxis(title = list(text = "Tempo"), crosshair=TRUE) %>%
    highcharter::hc_yAxis(title = list(text = "Press\u00e3o m\u00e9dia"), crosshair = TRUE) %>%
    highcharter::hc_tooltip(
      pointFormat = paste0(
        "<b>Press\u00e3o</b>: {point.M\u00e9dia Press\u00e3o}<br>",
        "<b>Condi\u00e7\u00e3o</b>: {point.condicao}<br>"
      )
    )
}
