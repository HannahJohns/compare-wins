# Demonstration of a module to be loaded into shiny
list(
  module_name = "SAMPLESIZE",
  module_label = "Sample Size Estimation",
  imports=NULL,
  ui_element = fluidPage(
    fluidRow(
     column(width=2,
            radioButtons("SAMPLESIZE__effectSizeType",
                         label = "Effect Size",
                         choices=c("winRatio","winOdds","netBenefit"))
     ),
     column(width=2,
            numericInput("SAMPLESIZE__effectSize",
                         label = "Effect Size",
                         value=0)
            ),
     column(width=2,
            numericInput("SAMPLESIZE__prop_ties",
                         label = "Proportion of tied observations",
                         value=0)
     ),
     column(width=2,
            numericInput("SAMPLESIZE__prop_ties_error",
                         label = "Sensitivity error margin",
                         value=0)
     )
   ),
   fluidRow("Results of sample size estimation go here")
  ),
  server_element = substitute({
    
    # These sample sizes are in total. Divide by 2 to get per-group sample size
    yu_power_generic <- function(p_win,p_loss,p_tie, k=1, alpha, power){
      
      # k here is proportion assigned to one group, not ratio
      # For consistency with winp, convert from ratio to proportion
      k <- k/(1+k)
      
      logwr = log(p_win/p_loss)
      
      s2 = 4*(1+p_tie)/(3*k*(1-k)*(1-p_tie))
      
      N = s2 * (qnorm(1-alpha/2) + qnorm(power))^2 / (logwr^2)
      
      return(N)
    }
    
    #' Converts effect size and assumed proportion of ties
    #' to win/loss/tie proportions
    get_wlt <- function(effect,ES,ties){
      
      # Just uniroot() this.
      # It's cheap to run, handles error checking for us,
      # and extendable to other effect sizes as they're proposed/developed
      
      prop_wl <- 1-ties
      
      wins <- tryCatch({uniroot(function(wins){
        
        losses = prop_wl-wins
        
        this_ES <- switch (effect,
                           "winRatio" = wins/losses,
                           "winOdds" = (wins+0.5*ties)/(losses+0.5*ties),
                           "netBenefit" = wins-losses
        )
        
        this_ES-ES
      },
      interval = c(0,prop_wl),
      tol = .Machine$double.eps
      )$root
      },
      error = function(e){NA})
      
      c(wins=wins,losses=prop_wl-wins,ties=ties)
    }
   
  })
)
