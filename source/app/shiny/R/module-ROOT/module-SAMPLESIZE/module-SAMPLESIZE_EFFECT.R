# Demonstration of a module to be loaded into shiny
list(
  module_name = "SAMPLESIZE_EFFECT",
  module_label = "Sample Size Estimation",
  imports=NULL,
  ui_element = fluidPage(
    fluidRow(
     column(width=3,
            radioButtons("SAMPLESIZE_EFFECT__effectSizeType",
                         label = "Effect Size",
                         choices=c("Win Ratio"="winRatio",
                                   "Win Odds"="winOdds",
                                   "Net Benefit"="netBenefit"))
     ),
     column(width=3,
            numericInput("SAMPLESIZE_EFFECT__effectSize",
                         label = "Effect Size",
                         value=0),
            numericInput("SAMPLESIZE_EFFECT__prop_ties",
                         label = "Proportion of tied observations",
                         value=0)
            ),
     column(width=3,
            numericInput("SAMPLESIZE_EFFECT__alpha",
                         label = "Type-I error rate",
                         value=0.05),
            numericInput("SAMPLESIZE_EFFECT__power",
                         label = "Target Power",
                         value=0.8),
     )
   ),
   tags$hr(),
   fluidRow(
     textOutput("SAMPLESIZE_EFFECT__result_sampleSize")
   ),
   tags$hr(),
   tags$h4("Sensitivity Analysis"),
   fluidRow(
     column(width=3,
            radioButtons("SAMPLESIZE_EFFECT__sensitivity_y",
                         label = "Show change in",
                         choices = c("Power"="power",
                                     "Sample Size"="sampleSize")
                         )
     ),
     column(width=3,
            radioButtons("SAMPLESIZE_EFFECT__sensitivity_x",
                         label = "As a function of",
                         choices=c("Sample Size"="sampleSize",
                                   "Effect Size"="effectSize",
                                   "Proportion of ties"="prop_ties"
                                   ),
                         )
            ),
     column(width=2,
            numericInput("SAMPLESIZE_EFFECT__sensitivity_start",
                         label = "From",
                         value=100),
            numericInput("SAMPLESIZE_EFFECT__sensitivity_end",
                         label = "To",
                         value=1000)
     ),
     column(width=2,
       conditionalPanel("input.SAMPLESIZE_EFFECT__sensitivity_y=='power' && input.SAMPLESIZE_EFFECT__sensitivity_x!='sampleSize'",
                        numericInput("SAMPLESIZE_EFFECT__sensitivity_N",
                                     label = "Fix Sample size at:",
                                     value=100)
       )
     )
   ),
   fluidRow(
     plotOutput("SAMPLESIZE_EFFECT__result_sampleSizePlot")
   )
  ),
  server_element = substitute({
    
    SAMPLESIZE_EFFECT__reactive_N <- reactive({
      
      wlt <- get_wlt(effect = input$SAMPLESIZE_EFFECT__effectSizeType,
                     ES = input$SAMPLESIZE_EFFECT__effectSize,
                     ties = input$SAMPLESIZE_EFFECT__prop_ties
      )
      
      N <- yu_power_generic(p_win = wlt["wins"],
                            p_loss =  wlt["losses"],
                            p_tie =  wlt["ties"],
                            alpha = input$SAMPLESIZE_EFFECT__alpha,
                            power = input$SAMPLESIZE_EFFECT__power,
                            k=0.5)
    })
    
    output$SAMPLESIZE_EFFECT__result_sampleSize <- renderText({
      
      
      wlt <- get_wlt(effect = input$SAMPLESIZE_EFFECT__effectSizeType,
                     ES = input$SAMPLESIZE_EFFECT__effectSize,
                     ties = input$SAMPLESIZE_EFFECT__prop_ties
      )
      
      N <- SAMPLESIZE_EFFECT__reactive_N()
      
      # Round up to nearest whole number
      N <- ceiling(N)
      
      # Formatting to report back
      effectName <- c("winRatio"="Win Ratio",
                      "winOdds"="Win Odds",
                      "netBenefit"="Net Benefit")[input$SAMPLESIZE_EFFECT__effectSizeType]
      
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
              strip_zeroes(input$SAMPLESIZE_EFFECT__effectSize),
              N,
              strip_zeroes(input$SAMPLESIZE_EFFECT__power),
              strip_zeroes(input$SAMPLESIZE_EFFECT__alpha)
              )
      
    }) 
    
    
    
    # Generic solver for power analysis.
    # Flag what to solve for based on what value is provided
    # as NA and return it.
    
    fill_power_blank <- function(effect,effectSize,prop_ties,alpha,power,sampleSize){
      
      get_N <- function(effect,
                        ES,
                        ties,
                        alpha,
                        power){
        
        
        wlt <- get_wlt(effect = effect,
                       ES = ES,
                       ties = ties
        )
        
        N <- yu_power_generic(p_win = wlt["wins"],
                              p_loss =  wlt["losses"],
                              p_tie =  wlt["ties"],
                              alpha = alpha,
                              power = power,
                              k=0.5)
        
        unname(N)
        
      }
      
      
      all_args <- c(ES=effectSize,ties=prop_ties,alpha=alpha,power=power,sampleSize=sampleSize)
      
      which_na <- is.na(all_args)
      which_na <- names(which_na[which_na])
      
      args <- as.list(all_args[names(all_args)!=which_na])
      args[["effect"]] <- input$SAMPLESIZE_EFFECT__effectSizeType
      
      
      if(is.na(sampleSize)){
        
        args <- args[which(names(args)!="sampleSize")]
        out <- do.call("get_N",args)
        
      } else {
        
        # Root finding function - fit whatever we're varying
        # to reach target sample size
        
        rootFun <- function(toVary,which_na,args){
          
          toVary <<- toVary
          
          args[[which_na]] <- toVary
          
          target_N <- args[["sampleSize"]]        
          args <- args[which(names(args)!="sampleSize")]
          
          target_N-do.call("get_N",args)

        }
        
        # Get starting intervals.
        
        # NOTE: The power formula we're using appears to be unstable
        # for extreme values of power (<0.01). This shouldn't
        # cause problems in practice, but it makes root finding annoying
        # To resolve this, provide a battery of values to each interval
        # and then take the largest and smallest values.
        
        if(which_na=="ES"){
          
          if(args[["effect"]] == "netBenefit") {
            
            interval <- seq(-0.999,0.999,length.out=1000)
            
          } else {
            interval <- exp(seq(-0.999,0.999,length.out=1000))
          }
          
        } else if(which_na=="ties"){
          
          interval <- seq(0.001,0.99,length.out=1000)
          
        } else if(which_na=="alpha"){
          
          interval <- seq(0.001,0.99,length.out=1000)
          
        } else if(which_na=="power"){
          
          interval <- seq(0.001,0.99,length.out=1000)
        } 
        
        
        interval_f <- sapply(interval,rootFun,which_na=which_na,args=args)
        
        interval <- c(
          interval[interval_f==max(interval_f)],
          interval[interval_f==min(interval_f)]
        )
        interval <- sort(interval)
        
        out <- uniroot(rootFun,interval = interval,which_na=which_na,args=args,
        tol = .Machine$double.eps)$root
        
      }
      
      names(out) <- which_na
      out
      
    }
    
    
    output$SAMPLESIZE_EFFECT__result_sampleSizePlot <- renderPlot({

      # input <- list(
      # 
      #   SAMPLESIZE_EFFECT__effectSizeType = c("winRatio",
      #                                  "winOdds",
      #                                  "netBenefit")[1],
      # 
      #   SAMPLESIZE_EFFECT__effectSize = 1.2,
      # 
      #   SAMPLESIZE_EFFECT__prop_ties = 0,
      # 
      #   SAMPLESIZE_EFFECT__alpha=0.05,
      #   SAMPLESIZE_EFFECT__power = 0.8,
      # 
      #   SAMPLESIZE_EFFECT__sensitivity_y = c("power","sampleSize")[1],
      #   SAMPLESIZE_EFFECT__sensitivity_x = c("sampleSize","effectSize","prop_ties")[2],
      #   SAMPLESIZE_EFFECT__sensitivity_start=1.1,
      #   SAMPLESIZE_EFFECT__sensitivity_end=1.5,
      #   SAMPLESIZE_EFFECT__sensitivity_N = 100
      # )

      # browser()
      parGrid <- list(
                   effect= input$SAMPLESIZE_EFFECT__effectSizeType,
                   effectSize = input$SAMPLESIZE_EFFECT__effectSize,
                   prop_ties = input$SAMPLESIZE_EFFECT__prop_ties,
                   alpha=input$SAMPLESIZE_EFFECT__alpha,
                   power=input$SAMPLESIZE_EFFECT__power,
                   sampleSize=unname(SAMPLESIZE_EFFECT__reactive_N())
                  )
      parGrid_main_result <- parGrid
      
      parGrid[[input$SAMPLESIZE_EFFECT__sensitivity_y]] <- NA
      parGrid[[input$SAMPLESIZE_EFFECT__sensitivity_x]] <- seq(input$SAMPLESIZE_EFFECT__sensitivity_start,
                                                        input$SAMPLESIZE_EFFECT__sensitivity_end,
                                                        length.out=20) 
      
      parGrid <- do.call("expand.grid",parGrid)
      
      parGrid[,input$SAMPLESIZE_EFFECT__sensitivity_y] <- sapply(1:nrow(parGrid),function(i){
        
        out <- tryCatch({
          do.call("fill_power_blank",as.list(parGrid[i,]))
        }, error=function(e){NA})  

        unname(out)
      })
      
      
      parGrid <- rbind(parGrid,do.call("data.frame",parGrid_main_result))
      
      parGrid$y <-  parGrid[,input$SAMPLESIZE_EFFECT__sensitivity_y]
      parGrid$x <-  parGrid[,input$SAMPLESIZE_EFFECT__sensitivity_x]
      
      parGrid <- parGrid[order(parGrid$x),]
      
      hline <- switch(input$SAMPLESIZE_EFFECT__sensitivity_y,
                      power=input$SAMPLESIZE_EFFECT__power,
                      sampleSize=SAMPLESIZE_EFFECT__reactive_N())
      
      vline <- switch(input$SAMPLESIZE_EFFECT__sensitivity_x,
                      sampleSize=SAMPLESIZE_EFFECT__reactive_N(),
                      effectSize=input$SAMPLESIZE_EFFECT__effectSize,
                      prop_ties=input$SAMPLESIZE_EFFECT__prop_ties,
                      power=input$SAMPLESIZE_EFFECT__power
                      )
      
      ylabel <- c(power="Power",sampleSize="Sample Size")[input$SAMPLESIZE_EFFECT__sensitivity_y]
      xlabel <- c(sampleSize="Sample Size",
                  effectSize="Effect Size",
                  prop_ties="Proportion of ties")[input$SAMPLESIZE_EFFECT__sensitivity_x]
      
      
      if(xlabel=="Effect Size") {
        xlabel <- c("winRatio"="Win Ratio",
                    "winOdds"="Win Odds",
                    "netBenefit"="Net Benefit")[input$SAMPLESIZE_EFFECT__effectSizeType]
      }
      
      ggplot(parGrid,aes(x=x,y=y))+
        geom_line()+
        geom_point(size=2)+
        geom_hline(yintercept = hline,color="dark red", linetype="dashed")+
        geom_vline(xintercept = vline,color="dark red", linetype="dashed")+
        labs(x=xlabel,
             y=ylabel)+
        theme_bw()
      
      
      
    })
   
  }) # End server
)
