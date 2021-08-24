import::from("magrittr", "%>%")

# gera html
rmarkdown::render_site("inst/book")

# faz mudanças necessárias
readr::read_file("inst/book/pseudopalavras.html") %>%
  pseudopalavras::relocate_refs() %>%
  pseudopalavras::add_footer("CENTRO DE ESTATÍSTICA APLICADA  - IME/ USP") %>%
  readr::write_file("inst/book/relatorio.html")

# fazer print em pdf
# se não rodar, abrir o relatorio.html e imprimir em pdf
pagedown::chrome_print(
  "inst/book/relatorio.html",
  output = "inst/book/relatorio/relatorio.pdf",
  extra_args = c("--disable-gpu", "--no-sandbox")
)

# adiciona capa
qpdf::pdf_combine(
  c("inst/book/assets/capa.pdf", "inst/book/relatorio/relatorio.pdf"),
  "inst/book/relatorio/pseudopalavras.pdf"
)


