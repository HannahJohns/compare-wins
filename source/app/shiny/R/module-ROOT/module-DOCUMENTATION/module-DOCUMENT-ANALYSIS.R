# Demonstration of a module to be loaded into shiny
list(
  module_name = "DOCUMENT-ANALYSIS",
  module_label = "Conducting Analysis",
  imports=NULL,
  ui_element = fluidPage(
    
    fluidRow(
      column(width=6,
        "Generalised Pairwise Comparisons are a family of statistical methods,
        not a single individual approach. COMPARE WINS provides a point-and-click
        interface to several statistical methods within the Generalised Pairwise Comparisons
        family. 
        "        
      )
    ),
    fluidRow(
      column(width=6,
             tags$h3("Effect Size Measures")
      )
    ),
    fluidRow(
      tags$h3("Methods supported by COMPARE WINS"),
    ),
    fluidRow(
    style = "height:500px;",  
    column(width=12,
           navlistPanel(
             tabPanel(title="Win Statistics",
                      absolutePanel(style="overflow: auto;", height="500px",width="100%",
                                    tags$h3("Overview"),
                                    
                                    "Supported by the",
                                    tags$a(href="https://www.doi.org/10.32614/CRAN.package.pim",target="_blank","WINS"),
                                    "package",
                                    
                                    tags$h3("Limitations"),
                                    
                                    tags$h3("Managing Time-To-Event Data")
                                    
                      )
             ),
             tabPanel(title="Probabilistic Index Models",
                      absolutePanel(style="overflow: auto;",
                                
                      tags$h3("Overview"),
                      
                      "Supported by the",
                      tags$a(href="https://www.doi.org/10.32614/CRAN.package.pim",target="_blank","pim"),
                      "package",
                      
                      tags$h3("Limitations")    
                                    
                      )
             )
           )
    )         
                         
    )
  ),
  server_element = substitute({})
)
