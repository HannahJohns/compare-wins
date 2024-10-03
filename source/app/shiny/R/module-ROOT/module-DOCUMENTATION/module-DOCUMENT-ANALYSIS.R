# Demonstration of a module to be loaded into shiny
list(
  module_name = "DOCUMENT-ANALYSIS",
  module_label = "Conducting Analysis",
  imports=NULL,
  ui_element = fluidPage(
    fluidRow(
      column(width=6,
             "Generalised Pairwise Comparisons refers to a family of statistical
              methods, each with their own limitations.
              COMPARE WINS supports the following methods:"
             ,
             tags$ul(
               tags$li(tags$b("Win Statistics")),
               tags$li(tags$b("Probabilistic Index Models"))
             )
             )
    ),
    fluidRow(bsCollapse(bsCollapsePanel(title="Win Statistics",
                                        
                                        tags$h3("Overview"),
                                        
                                        "Supported by the",
                                        tags$a(href="https://www.doi.org/10.32614/CRAN.package.pim",target="_blank","WINS"),
                                        "package",
                                        
                                        tags$h3("Limitations"),
                                        
                                        tags$h3("Managing Time-To-Event Data")
                                        
                                        
    ))
    ),
    fluidRow(bsCollapse(bsCollapsePanel(title="Probabilistic Index Models",
                                        
                                        tags$h3("Overview"),
                                        
                                        "Supported by the",
                                        tags$a(href="https://www.doi.org/10.32614/CRAN.package.pim",target="_blank","pim"),
                                        "package",
                                        
                                        tags$h3("Limitations")
                                        
                                        
    ))
    ),
    fluidRow(
      column(width=6,
      tags$h3("Interpreting Results")
      )
    )
  ),
  server_element = substitute({})
)
