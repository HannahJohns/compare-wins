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
           of outcomes is considered to be preferable.
           COMPARE WINS supports the following approaches:
           The ",
           tags$b("Heirarchy"),
           "-based approach (also called 'Prioritised Outcomes'), where
           each outcome is strictly ordered in terms of preference",
           ", and the",
           tags$b("List"),
           "-based approach, which allows for explicit trade-offs
           between outcomes.
           "
    )
  ),
  fluidRow(bsCollapse(bsCollapsePanel(title="Heirarchy",
                                      
                                      tags$h3("Overview"),
                                      "This preference definition requires...",
                                      
                                      tags$h3("How Preference is Determined for Individual Outcomes"),
                                      
                                      tags$h4("Numeric Data"),
                                      tags$h4("Survival Data"),
                                      
                                      tags$h3("Minimal Clinical Difference")
                                      
                                      ))
           ),
  fluidRow(bsCollapse(bsCollapsePanel(title="List-Based",
                                      "This preference definition requires..."
                                      ))
           )
  ),
  server_element = substitute({})
)
