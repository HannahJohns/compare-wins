# Demonstration of a module to be loaded into shiny
list(
  module_name = "DOCUMENT-ANALYSIS",
  module_label = "Conducting Analysis",
  imports=NULL,
  ui_element = fluidPage(
    
    fluidRow(
      column(width=6,
       "COMPARE WINS estimates sample size based on the power formula provided by",
       tags$a("Yu and Ganju (2022)",
              href="https://doi.org/10.1002/sim.9297",target="_blank"),
       ". This sample size estimation is provided in two approaches, detailed below"
      )
    ),
    fluidRow(
      tags$h3("Sample Size Estimation Methods Supported by COMPARE WINS"),
    ),
    fluidRow(
    style = "height:500px;",  
    column(width=12,
           navlistPanel(
             tabPanel(title="Pairs Approach",
                      absolutePanel(style="overflow: auto;", height="500px",width="100%",
                                    tags$h3("Overview"),
                                    "This approach takes the number of expected wins, losses and ties,
                                     and estimates the required sample size to achieve a given power.
                                    Up to 10 scenarios corresponding to different counts of wins,
                                    losses and ties may be supplied to examine how sample size
                                    changes according to different assumptions.
                                    ",
                                    tags$br(),tags$br(),
                                    tags$b("NOTE: "),
                                    "The total number of wins, losses and ties does not impact",
                                    "the estimated sample size, all numbers are relative. 
                                    If 50 wins, 25 losses and 25 ties are specified, this will
                                    be identical to providing 2 wins, 1 loss and 1 tie,
                                    or 1 win, 0.5 losses and 0.5 ties.",
                                  
                                    tags$h3("Limitations"),
                                    
                                    "Changing the number of ties without impacting the effect size,
                                    or changing the effect size without impacting the numer of ties,
                                    is difficult to do directly, especially for the Win Odds. This
                                    can make sensitivity analyses where one of these is assumed fixed
                                    difficult. This capability is provided by the
                                    ", tags$em("Effect Size Approach"),"."
                                    
                                    
                      )
             ),
             tabPanel(title="Effect Size Approach",
                      absolutePanel(style="overflow: auto;",
                                
                      tags$h3("Overview"),
                      
                      "This approach takes the number of expected wins, losses and ties,
                       and estimates the required sample size to achieve a given power.
                       Up to 10 scenarios corresponding to different counts of wins,
                       losses and ties may be supplied to examine how sample size
                       changes according to different assumptions.
                       ",
                    
                      tags$h3("Limitations"),
                      
                      "This method effectively assumes a specific relationship
                      between the proportion of wins and losses, which is governed
                      by the effect size specified and the proportion of ties specified.
                      This may be unrealistic in practice.
                      The proportion of wins, losses and ties may be provided directly
                      by using the", tags$em("Pairs Approach"),"."
                                    
                      )
             )
           )
    )         
                         
    )
  ),
  server_element = substitute({})
)
