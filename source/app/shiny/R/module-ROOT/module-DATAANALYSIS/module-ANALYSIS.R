# Demonstration of a module to be loaded into shiny

# List of packages to implement:

# genodds
# PIM
# Some custom code for detecting non-transitivity

# Win Ratio
# WINS
# WR
# WRestimates
# EventWinRatios


list(
  module_name = "DATAANALYSIS",
  module_label = "Data Analysis",
  imports=c("DATAIMPORT__data_sheet"="SYMBOLIC_LINK__data_sheet",
            "PREFDEF__preference_export"="SYMBOLIC_LINK__preference_export",
            "PREFDEF__preferenceType"="SYMBOLIC_LINK__preferenceType"
            ),
  ui_element = sidebarLayout(
    sidebarPanel(
      uiOutput("DATAANALYSIS__ui_options")
      # fluidRow(
      #   actionButton("DATAANALYSIS__save_button","DEV_ONLY: Save state for interactive testing")
      # )
    ),
    mainPanel(
      fluidRow(tableOutput("DATAANALYSIS__wins_output"))
    )
  ),
  server_element = substitute({
    
    # Wrapper function for interfacing with WINS::win.stat
    
    # We can probably move this to an external R file rather than have it sit within the server definition
    wins_wrapper <- function(data, outcomes, arm,
                             stratum=NULL,
                             covariates=NULL,
                             method = "unadjusted",
                             stratum.weight = "unstratified",
                             pvalue = "two-sided"
    ){
      
      # First, we need to convert the data sheet into the correct format
      
      formatted_data <- data.frame(id=1:nrow(data))
      formatted_data$arm <- factor(data[,arm])
      
      if(!is.null(stratum)){
        formatted_data$stratum <- data[,stratum]
      } else {
        stratum.weight <- "unstratified"
      }
      
      ep_type <- {}
      np_direction <- {}
      for(i in 1:length(outcomes)){
        ep_type[i] <- outcomes[[i]]$type
        tau <- 0 # TODO: Need to add this
        np_direction[i] <- c(`>`="larger",`<`="smaller")[outcomes[[i]]$direction]
        
        formatted_data[,sprintf("Y_%d",i)] <- data[,outcomes[[i]]$var]
        if(ep_type[i] %in% c("surv","tte")){
          ep_type[i] <- "tte"
          formatted_data[,sprintf("Delta_%d",i)]  <- data[,outcomes[[i]]$indicator]
        } 
      }
      
      # Only support covariates at baseline for the time being. 
      
      # NOTE: This needs added
      if(is.null(covariates)){
        Z_t_trt <- Z_t_con <- NULL
      } else {
        # At present only support covariates at baseline
        Z_t_con <- data.frame(id=1:nrow(data), time=0) 
        for(i in 1:length(covariates)){
          Z_t_con[,covariates[i]] <- data[,covariates[i]]
        }
        Z_t_trt <-Z_t_con
      }
      
      
      out <- WINS::win.stat(data = formatted_data,
                     ep_type = ep_type,
                     priority = 1:length(outcomes),
                     arm.name = levels(formatted_data$arm),
                     tau = tau,
                     np_direction = np_direction,
                     Z_t_trt = Z_t_trt,
                     Z_t_con = Z_t_con, 
                     alpha = 0.05,
                     method = method,
                     stratum.weight = stratum.weight,
                     pvalue = pvalue,
                     summary.print = FALSE 
      )
      
      
      # Format results into a table to report back
      
      estimates <- do.call("rbind",lapply(names(out$Win_statistic),function(i){
        cbind(outcome=i,
              as.data.frame(matrix(out$Win_statistic[[i]],nrow=1))
              )
      }))
      
      colnames(estimates)[-1] <- c("Estimate","Lower CI","Upper CI")
      
      estimates$p.value <- c(out$p_value)

      # # Get endpoint wins/losses/ties/etc and format neatly for reporting as well
      # 
      # winTab <- out$summary_ep[
      # names(out$summary_ep)[grepl("^Trt_",names(out$summary_ep))]
      # ]
      # winTab <- do.call("rbind",winTab)
      # winTab$endpoint <- gsub("Trt_Endpoint","",rownames(winTab))
      # winTab <- winTab[,c("Stratum","endpoint","Proportion")]
      # colnames(winTab)[3] <- "Proportion_Win"
      # 
      # lossTab <- out$summary_ep[    
      # names(out$summary_ep)[grepl("^Con_",names(out$summary_ep))]
      # ]
      # lossTab <- do.call("rbind",lossTab)
      # lossTab$endpoint <- gsub("Con_Endpoint","",rownames(lossTab))
      # lossTab <- lossTab[,c("Stratum","endpoint","Proportion")]
      # colnames(lossTab)[3] <- "Proportion_Loss"
      # 
      # dplyr::left_join(winTab,lossTab)
      
      estimates
    }
    
    
    output$DATAANALYSIS__ui_options <- renderUI({
      req(SYMBOLIC_LINK__data_sheet())
      data_sheet <- isolate(SYMBOLIC_LINK__data_sheet())
      
      
      tagList(
        # Need a better way of describing these
        radioButtons("DATAANALYSIS__method",
                     label="Select analysis method:",
                     choices=c("Win Ratio Analysis")), #"Probabilistic Index Model Analysis"
        # All of this should be nested inside conditional panels
        fluidRow(selectInput(inputId = "DATAANALYSIS__arm",
                             label = "Treatment Arm is:",
                             choices = colnames(data_sheet)
        )),
        # fluidRow("Option for selecting the active group goes here"),
        # fluidRow("Options for covariates goes here. Needs to be made clear that WINS just uses this for ipcw stuff"),
        fluidRow(actionButton("DATAANALYSIS__analysis_go","Analyse!"))
      )
      
    })
    
    
    output$DATAANALYSIS__wins_output <- renderTable({
      
      
      req(SYMBOLIC_LINK__data_sheet())
      req(SYMBOLIC_LINK__preference_export())
      
      input$DATAANALYSIS__analysis_go
      
      data_sheet <- isolate(SYMBOLIC_LINK__data_sheet())
      preferences <- isolate(SYMBOLIC_LINK__preference_export())
      arm <- isolate(input$DATAANALYSIS__arm)
      
      #TODO: At present, this only supports heirarchical output.
      # This needs fixed for use with e.g. PIM
    
      if(isolate(input$SYMBOLIC_LINK__preferenceType)=="heirarchical"){
        
        #TODO: Add options for stratification, ipcw and covariate ipcw adjustment etc
        
        out <- wins_wrapper(data = data_sheet,
                     outcomes=preferences$heirarchical,
                     arm=arm,
                     covariates = NULL,
                     method = "unadjusted"
                     )
        
        return(out)
        
      } else {
        return(NULL)
      } 
      
    })
    
    observeEvent(input$DATAANALYSIS__save_button,{
      out <- list(
        data_sheet <- isolate(SYMBOLIC_LINK__data_sheet()),
        preferences <- isolate(SYMBOLIC_LINK__preference_export())
      )
      saveRDS(out,file="tmp.RDS")
    })
    
    
  })
)
