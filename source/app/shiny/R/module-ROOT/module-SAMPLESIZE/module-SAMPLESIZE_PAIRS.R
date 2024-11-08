# Demonstration of a module to be loaded into shiny
list(
  module_name = "SAMPLESIZE_PAIRS",
  module_label = "Sample Size Estimation (Pairs approach)",
  imports=NULL,
  ui_element = fluidPage(
    fluidRow(
      column(width=2,numericInput("SAMPLESIZE_PAIRS__power","Power",value=0.8,min = 0.05, max=1,step = 0.05)),
      column(width=2,numericInput("SAMPLESIZE_PAIRS__alpha","Type-I error",value=0.05,min = 0,max=0.2,step = 0.01)),
      column(width=4,sliderInput("SAMPLESIZE_PAIRS__num_scenarios","Number of scenarios",min=1,max=10,step = 1,value=1))
    ),
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(width=12,uiOutput("SAMPLESIZE_PAIRS__power_pairs_control"))
          )
        ),
        mainPanel(
          tableOutput("power_results")
        )
      )
    )
  ),
  server_element = substitute({
    
    power_xtab_generic <- reactiveVal({})
    
    
    output$SAMPLESIZE_PAIRS__power_pairs_control <- renderUI({
      
      strata_list <- lapply(1:input$SAMPLESIZE_PAIRS__num_scenarios, function(i) {
        
        ({
          fluidRow(
            fluidRow(
              column(width=12,
                     textInput(paste0("power_pairs_strataName_",i),
                               paste0("Scenario ",i,":"),
                               paste0("scenario",i))
              )
            ),
            fluidRow(
              column(width=4, numericInput(paste0("power_pairs_win_",i),
                                                     HTML("Number of wins"),
                                                     value = "1",min = "0")),
              column(width=4, numericInput(paste0("power_pairs_tie_",i),
                                           HTML("Number of ties"),value = "1",min = "0")),
              column(width=4,numericInput(paste0("power_pairs_loss_",i),
                                          HTML("Number of losses"),
                                          value = "1",min = "0"))
            )
          )
        })
        
      })

      strata_list <- do.call("tagList",strata_list)
      
      
    })
    
    observe({
      
      x <- NULL
      tryCatch({
        x <- lapply(1:(input$SAMPLESIZE_PAIRS__num_scenarios), function(i) {
          
          if(is.null( input[[paste0("power_pairs_strataName_", 
                                    i)]])){
            x <- NULL
            
          } else {
            data.frame(strata_i =i,
                       strata=input[[paste0("power_pairs_strataName_",i)]],
                       pWin = input[[paste0("power_pairs_win_",i)]],
                       pTie = input[[paste0("power_pairs_tie_",i)]],
                       pLoss = input[[paste0("power_pairs_loss_",i)]]
            ) -> x
            sum <- x$pWin+x$pTie+x$pLoss
            
            x$pWin <- x$pWin/sum
            x$pTie <- x$pTie/sum
            x$pLoss <- x$pLoss/sum
          }

        
          return(x)
        })
        
        
        x <- do.call("rbind",x)
      })
    
      
      # if we didn't return an error/class is a data frame/etc
      # then update the proportions used for power calculations
      
      if("data.frame" %in% class(x))
      {
        power_xtab_generic(x)
      }
      
    })
    
    
    output$power_results<- renderTable({
      
      power <- input$SAMPLESIZE_PAIRS__power
      alpha <- input$SAMPLESIZE_PAIRS__alpha
      k <- 0.5
      
      x <- power_xtab_generic()
      
      out <- by(x,x$strata_i,function(x){
        
        data.frame(
                   Scenario = x$strata,
                   `Win_Probability` = x$pWin,
                   `Tie_Probability` = x$pTie,
                   `Loss_Probability` = x$pLoss,
                   `Win_Ratio` = x$pWin/x$pLoss,
                   `Win_Odds` = (x$pWin+x$pTie)/(x$pLoss+x$pTie),
                   `Net_Benefit` = x$pWin-x$pLoss,
                   `N` = yu_power_generic(p_win = x$pWin,
                                         p_tie = x$pTie,
                                         p_loss = x$pLoss,
                                         k = k,
                                         alpha = alpha,
                                         power=power))
        
      })
      
      if(length(out)>0){
        out <- do.call("rbind",out)
        
        colnames(out) <- gsub("_"," ",colnames(out))
      } else {
        out <- NULL
      }
      
      return(out)
      
    })
    
    
    
  }) # End server
)
