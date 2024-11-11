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
             tags$h3("Effect Size Measures"),
             "There are multiple effect size measures that can be used in
             Generalized Pairwise Comparisons analysis.
             COMPARE WINS supports the following effect size measures:",
             tags$ul(
               tags$li(tags$b("Win Ratio"),
                       "$$Win Ratio = \\frac{Prob(win)}{Prob(loss)}$$",
                       ""),
               tags$li(tags$b("Win Odds"),
                       "$$Win Odds = \\frac{Prob(win)+0.5\\times Prob(tie)}{Prob(loss)+0.5\\times Prob(tie)}$$",
                       ""),
               tags$li(tags$b("Net Benefit"),
                       "$$Net Benefit = Prob(win)-Prob(loss)$$",
                       "")
             )
      )
    ),
    fluidRow(
      tags$h3("Estimation methods supported by COMPARE WINS"),
    ),
    fluidRow(
    style = "height:500px;",  
    column(width=12,
           navlistPanel(
             tabPanel(title="Win Statistics",
                      absolutePanel(style="overflow: auto;", height="500px",width="100%",
                                    tags$h3("Overview"),
                                    
                                    "Calculates the Win Ratio, Win Odds and Net Benefit
                                    ",
                                    
                                    "Supported by the",
                                    tags$a(href="https://www.doi.org/10.32614/CRAN.package.pim",target="_blank","WINS"),
                                    "package",
                                
                                    tags$h3("Managing Covariates"),
                                    
                                    # TODO: Note here that we can run stratification but not covariate adjustment
                                    
                                    tags$h3("Managing Time-To-Event Data"),
                                    
                                    # TODO: Note here that survival data induces limitations:
                                    # Win Odds is misleading if not adjusted for censoring
                                    # Direction must be greater is better
                                    
                                    tags$h3("Statistical Controls")
                                    
                                    # TODO: Note here that survival data induces limitations:
                                    # Win Odds is misleading if not adjusted for censoring
                                    # Direction must be greater is better
                                    
                                    
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
