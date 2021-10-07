#' relocate_refs
#'
#' @param html html string
#'
#' @return html string
#' @export
#'
relocate_refs <- function(html) {
  refs <- html %>%
    stringr::str_extract(paste0(
      "<div id=\"ficha-técnica\"[\\s\\S]+",
      "<!--chapter:end:[0-9]{2}-referencias.Rmd-->[\\s\\S]+?</div>\n</div>"
    ))
  pref <- html %>%
    stringr::str_extract("<div class=\"front-matter-container\">")
  text <- paste(pref, pref, refs, "</div>", sep = "\n")
  novo_html <- html %>%
    stringr::str_remove(stringr::fixed(refs)) %>%
    stringr::str_replace(pref, stringr::fixed(text)) %>%
    stringr::str_remove(
      "<li><a href=\"#ficha-técnica\">[\\s\\S]+?</ul></li>\n"
    )
  return(novo_html)
}

#' add_footer
#'
#' @param html html string
#' @param footer footer text
#'
#' @return html string
#' @export
#'
add_footer <- function(html, footer) {
  footer <- paste("<body>\n<div class='footer'><hr>", footer, "</div>")
  html <- html %>%
    stringr::str_replace(
      "<body>",
      stringr::fixed(footer)
    )
  return(html)
}
