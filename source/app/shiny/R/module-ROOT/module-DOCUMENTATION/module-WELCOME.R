# Demonstration of a module to be loaded into shiny
list(
  module_name = "WELCOME",
  module_label = "Introduction",
  imports=NULL,
  ui_element = fluidPage(
  tags$h2(HTML("<b>C</b>omparison <b>O</b>f <b>M</b>ultifaceted <b>P</b>references <b>A</b>s <b>R</b>esearch <b>E</b>ndpoints using <b>WIN</b> <b>S</b>tatistics")),
  hr(),
  tags$h3("Introduction"),
  tags$b("COMPARE WINS "),"provides a point-and-click interface to a variety of ",
  tags$h4("What are Generalised Pairwise Comparisons?"),
  "Generalised Pairwise Comparisons are a type of statistical method that...",
  "Because the idea of 'better outcome' is extremely general, ...",
  "The approach has many other names incluiding:",
  tags$div(
    tags$ul(
      tags$li("Win Ratio"),
      tags$li("Desirability of Outcome Ranking (DOOR)")
    )
  ),
  hr(),
  tags$h3("How to use this software"),
  hr(),
  tags$h3("Credits")
  ),
  server_element = substitute({})
)
