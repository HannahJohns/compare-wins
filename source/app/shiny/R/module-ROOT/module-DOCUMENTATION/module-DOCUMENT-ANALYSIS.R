# Demonstration of a module to be loaded into shiny
list(
  module_name = "DOCUMENT-ANALYSIS",
  module_label = "Conducting Analysis",
  imports=NULL,
  ui_element = fluidPage(
    fluidRow(
      tags$h3("Methods supported by COMPARE WINS"),
    ),
    fluidRow(
    style = "height:500px;",  
    column(width=12,
           navlistPanel(
             tabPanel(title="Effect Size Measures",
                      absolutePanel(
                        style="overflow: auto;", height="500px",width="100%",
                        
                        "Generalised Pairwise Comparisons methods work by comparing all participants
              in the control group to all participants in the intervention group.
              These comparisons produce one of three types of outcomes:",
                        tags$ul(
                          tags$li(tags$b("Wins")," - the participant in the intervention group had a better outcome than the person in the control group"),
                          tags$li(tags$b("Losses")," - the participant in the intervention group had a worse outcome than the person in the control group"),
                          tags$li(tags$b("Ties")," - there was no observed meaningful difference between these two participants")
                        ),
                        "All effect sizes for Generalised Pairwise Comparisons
        combine the probability of a random pair being a win, loss or tie
        in some way.",
                        
                        tags$h3("Effect Size Measures supported by COMPARE WINS"),
                        tags$ul(
                          tags$li("The ",tags$b("Win Ratio"),
                                  "is the ratio between wins and losses, and is given by",
                                  "$$\\textrm{Win Ratio} = \\frac{Prob(win)}{Prob(loss)}$$",
                                  "It can be interpreted as the odds that a random participant
                       that recieves the intervention will have a better outcome than a random
                       participant that recieves the control,",
                                  tags$em("assuming that there is a meaningful difference between them."),
                                  "The Win Ratio can remain large, even if the number of tied pairs increases."
                          ),
                          tags$li("The ",tags$b("Win Odds"),
                                  "is a modification of the Win Ratio that considers ties as half a win/half a loss. It is given by:",
                                  "$$\\textrm{Win Odds} = \\frac{Prob(win)+0.5\\times Prob(tie)}{Prob(loss)+0.5\\times Prob(tie)}$$",
                                  "Splitting ties in this manner is consistent with conventional",
                                  tags$em("nonparametric"),
                                  "statistics, such as the Wilcoxon-Mann-Whitney test.
                       The traditional justification for splitting ties evenly between being considered
                       a 'win' and a 'loss' is that a more granular endpoint would have demonstrated
                       a difference and, in the absence of other information, we assume there is no
                       difference between groups (i.e. equipoise is maintained). 
                       It can be interpreted as the odds that a random participant
                       that recieves the intervention will have a better outcome than a random
                       participant that recieves the control",
                                  tags$em("assuming that tied pairs are equally likely to be wins as losses"),
                                  "Unlike the Win Ratio, the Win Odds will tend towards 1 (no effect) as
                       the number of tied pairs increases.
                       "
                          ),
                          tags$li("The ",tags$b("Net Benefit"),
                                  "is the difference between wins and losses. It is given by:",
                                  "$$\\textrm{Net Benefit} = Prob(win)-Prob(loss)$$",
                                  "
                       The Net Benefit statistic has many of the same properties as the Win Odds
                       (i.e. it tends towards 0 (no effect) as the number of tied pairs increases).
                       
                       "))
                       
                      )
                      ),
             tabPanel(title="Win Statistics",
                      absolutePanel(style="overflow: auto;", height="500px",width="100%",
                                    tags$h3("Overview"),
                                    
                                    "This method calculates the Win Ratio, Win Odds and Net Benefit,
                                    using the
                                    ", 
                                    tags$a(href="https://www.doi.org/10.32614/CRAN.package.WINS",target="_blank","WINS"),
                                    "R package in the background.
                                    This method is intended for use with",
                                    tags$em("Heirarchical"),
                                    "preference definitions, but can also be,
                                    used with ",
                                    tags$em("List-based"),
                                    "preference definitions.
                                    ",
                                
                                    tags$h3("Managing Covariates"),
                                    
                                    "Win Statistics can handle covariates through stratification.
                                    COMPARE WINS provides this support, and additionally calculates
                                    effects within each strata.
                                    
                                    The following statistical methods for running stratified analysis are supported:",
                                    tags$ul(
                                     tags$li(tags$b("Maentel-Haenszel")," - weight the wins with the reciprocal of the stratum size following the Mantel-Haenszel type stratified analysis as described in Dong et al. (2018)."), 
                                     tags$li(tags$b("Proportional")," - weight the win statistics with weight equal to the number of subjects in each stratum divided by the total number of subjects."), 
                                     tags$li(tags$b("Proportional (Observed Events)")," - weight the win statistics with weight equal to the number of subjects with events (of any TTE endpoint) in each stratum divided by the total number of subjects with events (of any TTE endpoint)."),
                                     tags$li(tags$b("Equal Weights")," - set equal weights for all stratum."),
                                     tags$li(tags$b("Unstratified")," - don't stratify the analysis (but still estimate treatment effect within subgroups/strata)")
                                    ),
                                    
                                    "If more than one stratum variable is specified,
                                    the combination of all strata is used.",
                                    
                                    tags$h3("Managing Time-To-Event Endpoints"),
                                    
                                    "This method provides full support for survival endpoints,
                                    including adjustment for the impact of censoring on
                                    estimated treatment effect. COMPARE WINS supports the following
                                    methods:
                                    ",
                                    
                                    tags$ul(
                                      tags$li(tags$b("Unadjusted")," - do not apply adjustment for censoring."), 
                                      tags$li(tags$b("IPCW")," - apply adjustment for censoring"), 
                                      tags$li(tags$b("IPCW (Covariate adjusted)"),
                                              " - apply adjustment for censoring, where covariates can influence
                                              the censoring distribution
                                              "
                                              )
                                    ),
                                    
                                    tags$b("WARNING:"),
                                    "The Win Odds is highly sensitive to the presence of censored",
                                    "endpoint data. If you are estimating the win odds and your
                                    preference definition contains at least one survival endpoint,
                                    consider applying IPCW weighting. More information on this topic
                                    can be found",
                                    tags$a("HERE.",href=" https://doi.org/10.1002/pst.2251"),
                                    
                                    "Adjustment for censoring is not supported if any endpoint considers
                                    lower values to be better.
                                    "
                                    
                                    # tags$h3("Results"),
                                    # tags$h4("Visualisations")
                                    
                      )
             ),
             tabPanel(title="Probabilistic Index Models",
                      absolutePanel(style="overflow: auto;",
                                
                      tags$h3("Overview"),
                      
                      "This method calculates the Win Odds
                                    using the
                                    ", 
                      tags$a(href="https://www.doi.org/10.32614/CRAN.package.pim",target="_blank","pim"),
                      "R package in the background.
                      This method is intended for use with",
                      tags$em("List-based"),
                      "preference definitions, but can also be,
                                    used with ",
                      tags$em("Heirarchical"),
                      "preference definitions provided that:",
                      tags$ul(
                        tags$li("No included endpoint is survival, AND"),
                        tags$li("All included endpoints have a minimal clinical difference of 0"),
                      ),
                      tags$h3("Managing Covariates"),
                      "This method provides covariate adjustment through both stratification and 
                      continuous covariate adjustment. 
                      "
                      
                      # tags$h3("Results"),
                      # tags$h4("Visualisations")
                    
                                    
                      )
             )
           )
    )         
                         
    )
  ),
  server_element = substitute({})
)
