# Demonstration of a module to be loaded into shiny
list(
  module_name = "WELCOME",
  module_label = "Introduction",
  imports=NULL,
  ui_element = fluidPage(
  tags$h2(HTML("<b>C</b>omparison <b>O</b>f <b>M</b>ultifaceted <b>P</b>references <b>A</b>s <b>R</b>esearch <b>E</b>ndpoints using <b>WIN</b> <b>S</b>tatistics")),
  hr(),
  tags$h3("Introduction"),
  hr(),
  tags$h3("Credits")
  ),
  server_element = substitute({})
)
