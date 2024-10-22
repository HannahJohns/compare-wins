# Demonstration of a module to be loaded into shiny
list(
  module_name = "SAMPLESIZE",
  module_label = "Sample Size Estimation",
  imports=NULL,
  ui_element = fluidPage(
    fluidRow(
     column(width=3,
            radioButtons("SAMPLESIZE__effectSizeType",
                         label = "Effect Size",
                         choices=c("Win Ratio"="winRatio",
                                   "Win Odds"="winOdds",
                                   "Net Benefit"="netBenefit"))
     ),
     column(width=3,
            numericInput("SAMPLESIZE__effectSize",
                         label = "Effect Size",
                         value=0),
            numericInput("SAMPLESIZE__prop_ties",
                         label = "Proportion of tied observations",
                         value=0)
            ),
     column(width=3,
            numericInput("SAMPLESIZE__alpha",
                         label = "Type-I error rate",
                         value=0.05),
            numericInput("SAMPLESIZE__power",
                         label = "Target Power",
                         value=0.8),
     )
   ),
   tags$hr(),
   fluidRow(
     textOutput("SAMPLESIZE__result_sampleSize")
   ),
   tags$hr(),
   tags$h4("Sensitivity Analysis"),
   fluidRow(
     column(width=3,
            radioButtons("SAMPLESIZE__sensitivity_y",
                         label = "Show change in",
                         choices = c("Power"="power",
                                     "Sample Size"="sampleSize")
                         )
     ),
     column(width=3,
            radioButtons("SAMPLESIZE__sensitivity_x",
                         label = "As a function of",
                         choices=c("Sample Size"="sampleSize",
                                   "Effect Size",
                                   "Proportion of ties"
                                   ),
                         )
            ),
     column(width=2,
            numericInput("SAMPLESIZE__sensitivity_start",
                         label = "From",
                         value=100),
            numericInput("SAMPLESIZE__sensitivity_start",
                         label = "To",
                         value=1000)
     ),
     column(width=2,
       conditionalPanel("input.SAMPLESIZE__sensitivity_y=='power' && input.SAMPLESIZE__sensitivity_x!='sampleSize'",
                        numericInput("SAMPLESIZE__sensitivity_N",
                                     label = "Fix Sample size at:",
                                     value=100)
       )
     )
   )
  ),
  server_element = substitute({
    
    # These sample sizes are in total. Divide by 2 to get per-group sample size
    yu_power_generic <- function(p_win,p_loss,p_tie, k=0.5, alpha, power){
      
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
    
    output$SAMPLESIZE__result_sampleSize <- renderText({
      
     

      wlt <- get_wlt(effect = input$SAMPLESIZE__effectSizeType,
                     ES = input$SAMPLESIZE__effectSize,
                     ties = input$SAMPLESIZE__prop_ties
      )

      N <- yu_power_generic(p_win = wlt["wins"],
                            p_loss =  wlt["losses"],
                            p_tie =  wlt["ties"],
                            alpha = input$SAMPLESIZE__alpha,
                            power = input$SAMPLESIZE__power,
                            k=0.5)
      
      # Round up to nearest whole number
      N <- ceiling(N)
      
      # Formatting to report back
      effectName <- c("winRatio"="Win Ratio",
                      "winOdds"="Win Odds",
                      "netBenefit"="Net Benefit")[input$SAMPLESIZE__effectSizeType]
      
      # Strip trailing zeros
      
      strip_zeroes <- function(x){
        x <- sprintf("%f",x)
        x <- gsub("(?<=.)[0]+$","",x,perl = T)
        x <- gsub("\\.$",".0",x)
        x
      }
      
      sprintf("Assuming that %4.2f%% of pairs are wins for the intervention arm, 
               %4.2f%% losses are losses for the intervention arm,
               and %4.2f%% pairs are tied
               (%s=%s), a total sample size of 
               %0.0f provides %s power with a Type-I error of %s",
              100*wlt["wins"],
              100*wlt["losses"],
              100*wlt["ties"],
              effectName,
              strip_zeroes(input$SAMPLESIZE__effectSize),
              N,
              strip_zeroes(input$SAMPLESIZE__power),
              strip_zeroes(input$SAMPLESIZE__alpha)
              )
      
    }) 
    
    
    # TODO: Build plot based on the selected inputs above
    
    # output$SAMPLESIZE__result_sampleSizePlot <- renderPlot({
    #   
    #   input <- list(SAMPLESIZE__effectSizeType="winRatio",
    #                 SAMPLESIZE__effectSize = 1.5,
    #                 SAMPLESIZE__prop_ties=0.1,
    #                 SAMPLESIZE__prop_ties_error=0.01,
    #                 SAMPLESIZE__alpha=0.05,
    #                 SAMPLESIZE__power=0.8)
    #   
    #   effectName <- c("winRatio"="Win Ratio",
    #                   "winOdds"="Win Odds",
    #                   "netBenefit"="Net Benefit")[input$SAMPLESIZE__effectSizeType]
    #   
    #   
    #   ties_bounds <- input$SAMPLESIZE__prop_ties + ((-1):1) * input$SAMPLESIZE__prop_ties_error
    #   ties_bounds[ties_bounds<0] <- 0
    #   ties_bounds[ties_bounds>1] <- 1
    #   
    #   
    #   # Get range of effect sizes to plot
    #   multiplier <- 1.5 # Should be an input
    #   
    #   ES <- input$SAMPLESIZE__effectSize
    #   
    #   # If we need to log transform
    #   logTransform <- (effectName %in% c("Win Ratio", "Win Odds"))
    #   
    #   if(logTransform) ES <- log(ES)
    #   
    #   ES_Small <- ES/multiplier
    #   ES_Large <- ES*multiplier
    #   
    #   ES_seq <- unique(c(
    #     seq(min(ES_Small,ES_Large),ES,length.out=10),
    #     ES,
    #     seq(ES,max(ES_Small,ES_Large),length.out=10) 
    #   ))
    #   
    #   if(logTransform){
    #     ES <- exp(ES)
    #     ES_seq <- exp(ES_seq)
    #   }
    #   
    #   tmpdf <- expand.grid(
    #     ES = ES_seq,
    #     ties = input$SAMPLESIZE__prop_ties+ties_bounds
    #   )
    #   
    #   tmpdf$N <- sapply(1:nrow(tmpdf),function(i){
    #     
    #    
    #     
    #     wlt <- get_wlt(effect = input$SAMPLESIZE__effectSizeType,
    #                    ES = tmpdf[i,"ES"],
    #                    ties = tmpdf[i,"ties"]
    #     )
    #     
    #     N <- yu_power_generic(p_win = wlt["wins"],
    #                           p_loss =  wlt["losses"],
    #                           p_tie =  wlt["ties"],
    #                           alpha = input$SAMPLESIZE__alpha,
    #                           power = input$SAMPLESIZE__power,
    #                           k=0.5)
    #   })
    #   
    #   tmpdf$provided_value <- tmpdf$ES==input$SAMPLESIZE__effectSize & tmpdf$ties==input$SAMPLESIZE__prop_ties
    #   
    #   tmpdf
    #   
    #   tmpdf[which(tmpdf$provided_value),]
    #   
    #   ggplot(tmpdf, aes(x=ES,y=N,linetype=factor(ties)))+
    #     geom_line()+
    #     geom_hline()
    #  
    #   
    #   
    # }) 
   
  }) # End server
)
