# Demonstration of a module to be loaded into shiny
list(
  module_name = "DOCUMENT-ANALYSIS",
  module_label = "Analysis Methods",
  imports=NULL,
  ui_element = fluidPage(
  fluidRow(
    column(width=6,
           "The Generalised Pairwise Comparisons approach to data analysis
           requires a formalised method for determining which combination
           of outcomes is considered to be preferable. COMPARE WINS supports
           several approaches that have been proposed in medical literature."
    )
  ),
  fluidRow(
    tags$h3("Methods for defining preference supported by COMPARE WINS"),
  ),
  fluidRow(
    style = "height:500px;",  
    column(width=12,
           navlistPanel(
             tabPanel(
               title="Heirarchy",
               absolutePanel(style="overflow: auto;", height="500px",width="100%",
                             tags$h3("Overview"),
                             
                             "The Heirarchical approach to defining ",
                             
                             
                             "This preference definition requires...",
                             
                             tags$h3("How Preference is Determined for Individual Outcomes"),
                             
                             tags$h4("Numeric Data"),
                             tags$h4("Survival Data"),
                             
                             tags$h3("Minimal Clinical Difference")
               )
             ),
             tabPanel(
               title="List-Based",
               absolutePanel(style="overflow: auto;", height="500px",width="100%",
                             tags$h3("Overview"),
                             "This preference definition requires..."
               )
              )
           )
      )
  )
  ),       
  server_element = substitute({})
)
