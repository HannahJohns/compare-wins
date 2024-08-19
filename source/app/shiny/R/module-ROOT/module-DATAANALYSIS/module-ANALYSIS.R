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
      tags$script("$(document).on('click', '.DATAANALYSIS__ui_updater', function () {
                              Shiny.onInputChange('DATAANALYSIS__uiUpdateId',this.id);
                              Shiny.onInputChange('DATAANALYSIS__uiUpdateId_update',Math.random());
                             });"),
      
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
      
      levels <- isolate(input$DATAANALYSIS__arm_active_selectInput)
      levels <- c(
       levels,
        setdiff(levels(formatted_data$arm),levels)
      )
        
      
      out <- WINS::win.stat(data = formatted_data,
                     ep_type = ep_type,
                     priority = 1:length(outcomes),
                     arm.name = levels,
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
                             label = "Intervention variable is:",
                             choices = colnames(data_sheet)
        )),
        fluidRow(
          uiOutput("DATAANALYSIS__arm_active") # TODO: probably swap this out with something in data import module for formatting stuff
        ),
        fluidRow(
          uiOutput(
            "DATAANALYSIS__covariates_components"            
          )
        ),
        fluidRow(actionButton("DATAANALYSIS__analysis_go","Analyse!"))
      )
      
    })
    
    output$DATAANALYSIS__arm_active <- renderUI({
      req(SYMBOLIC_LINK__data_sheet())
      
      data_sheet <- SYMBOLIC_LINK__data_sheet()
      
      
      levels <- levels(as.factor(data_sheet[,input$DATAANALYSIS__arm]))
      
      selectInput(inputId = "DATAANALYSIS__arm_active_selectInput",
                  label = "Intervention group is:",
                  choices = levels
      )
      
    })
    
    
    # TODO: This will be the same architecture as the heirarchical stuff, we can
    # copy the same code structure.
    
    # Observer for forcing a UI update 
    DATAANALYSIS__force_UI_update <- reactiveVal(0)
    
    DATAANALYSIS__covariates <- reactiveVal(list())
    
    # If an action was taken that causes structural change to 
    # Preference list (i.e. added/removed/re-ordered details)
    # Make these changes as neccesary 
    observe({
      
      req(SYMBOLIC_LINK__data_sheet())
      data_sheet <- SYMBOLIC_LINK__data_sheet()
      
      input$DATAANALYSIS__uiUpdateId
      
      print(input$DATAANALYSIS__uiUpdateId)
      
      if (!is.null(input$DATAIMPORT__uiUpdateId)) {
        
        # Get preference list to modify
        covariates_tmp <- isolate(DATAANALYSIS__covariates())
        
        selectedId <- isolate(input$DATAIMPORT__uiUpdateId)
        
        #Update preference list structure (e.g. shuffle, delete, etc) and then signal to update UI elements
        
        if(selectedId == "DATAANALYSIS__add_covariate"){
          
          # Add new preference component
          covariates_tmp[[length(covariates_tmp)+1]] <- list(
            var=colnames(data_sheet)[1]
          )
          DATAANALYSIS__covariates(covariates_tmp)
          
        } else {
          
          # Pressed button is in the dynamic rows.
          # Action taken depends on the button type
          
          # actIdLabels <- c(
          #   up="PREFDEF__component_up_",
          #   down="PREFDEF__component_down_",
          #   delete="PREFDEF__component_delete_"
          # )    
          # 
          # actType <- which(sapply(actIdLabels,grepl,x=selectedId))
          # actType <- names(actIdLabels)[actType]
          # if(length(actType)!=1){
          #   stop("Something is wrong")
          # }
          # 
          # change_number <- as.numeric(gsub(actIdLabels[actType],"",selectedId))
          # 
          # if(is.na(change_number)) stop("unexpeced non-numeric value")
          # 
          # #TODO: Write this
          # 
          # if(actType == "up" & change_number > 1){
          #   new_order <- 1:length(preferenceList_tmp)
          #   
          #   new_order[change_number] <- change_number-1
          #   new_order[change_number-1] <- change_number
          #   
          #   preferenceList_tmp <- preferenceList_tmp[new_order]
          # } else if (actType == "down" & change_number < length(preferenceList_tmp)){
          #   new_order <- 1:length(preferenceList_tmp)
          #   
          #   new_order[change_number] <- change_number+1
          #   new_order[change_number+1] <- change_number
          #   
          #   preferenceList_tmp <- preferenceList_tmp[new_order]
          #   
          # } else if(actType == "delete"){
          #   preferenceList_tmp <- preferenceList_tmp[-change_number]
          # }
          # 
          # # redundant
          # # class(preferenceList_tmp) <- c("list","heirarchical")
          # 
          # PREFDEF__preferenceHeirarchy(preferenceList_tmp)
          
        }
        
        # # Signal to UI to update
        tmp <- isolate(DATAANALYSIS__force_UI_update())
        tmp <- tmp+1
        tmp <- tmp %% 2
        DATAANALYSIS__force_UI_update(tmp)
        
      }
    })
    
    
    output$DATAANALYSIS__covariates_components <- renderUI({
      
      req(SYMBOLIC_LINK__data_sheet())
      
      # Trigger update of this section only when signaled to do so.
      # There is probably a better way to do this, but it works
      DATAANALYSIS__force_UI_update()
      
      data_sheet <- isolate(SYMBOLIC_LINK__data_sheet())
      
      new_covariates <- isolate(DATAANALYSIS__covariates())
      
      add_covariate_button <-  fluidRow(
        actionButton("DATAANALYSIS__add_covariate",
                     "Add Covariate",
                     class="DATAANALYSIS__ui_updater")
        )  
        
        if(length(new_covariates)==0){
          out <- tagList(add_covariate_button)
        } else {

          out <- tagList(
            do.call("tagList", lapply(1:length(new_covariates), function(i){
              fluidRow(
                flowLayout(
                  div(class="DATAANALYSIS__preference_value",
                      selectInput(inputId = sprintf("DATAANALYSIS__component_var_%d",i),
                                  label = "Outcome variable is:",
                                  choices = colnames(data_sheet),
                                  selected=new_preferences[[i]][["var"]]

                      )),
                  div(
                    actionButton(inputId = sprintf("DATAANALYSIS__component_up_%d",i),
                                 label = "Up",
                                 class="DATAANALYSIS__ui_updater"
                    ),
                    actionButton(inputId = sprintf("DATAANALYSIS__component_down_%d",i),
                                 label = "Down",
                                 class="DATAANALYSIS__ui_updater"
                    ),
                    actionButton(inputId = sprintf("DATAANALYSIS__component_delete_%d",i),
                                 label = "Delete",
                                 class="DATAANALYSIS__ui_updater"
                    )
                  )
                ),
                tags$br(),
                hr()
              )
            })),
            fluidRow(add_covariate_button)
          )
      }
      
      out
      
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
