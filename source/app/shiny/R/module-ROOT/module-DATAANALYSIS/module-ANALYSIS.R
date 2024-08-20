# Demonstration of a module to be loaded into shiny

# List of packages to implement:

# genodds
# PIM
# Some custom code for detecting non-transitivity

# Win Ratio
# WINS
# WR %>% 
# WRestimates
# EventWinRatios


# This is a test of some code:
# https://stackoverflow.com/questions/51621348/r-shiny-store-the-user-input-from-multiple-dynamically-generated-textareainput-f/51622007#51622007

# TODO: need some way to select multiple components for this
# Can use a wrapper function that obscures some of the nonsense here



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
    
    
    # Convenience function for getting a list of reactive inputs
    inputCollection <- function(input_names,input_index){
        c(t(outer(input_names,input_index,paste0)))
    }
    
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
                     choices=c("Win Ratio Analysis"="wins")), #"Probabilistic Index Model Analysis"
        # All of this should be nested inside conditional panels
        fluidRow(selectInput(inputId = "DATAANALYSIS__arm",
                             label = "Intervention variable is:",
                             choices = colnames(data_sheet)
        )),
        fluidRow(
          uiOutput("DATAANALYSIS__arm_active") # TODO: probably swap this out with something in data import module for formatting stuff
        ),
        fluidRow(tags$h4("Adjust for the following:")), # TODO
        fluidRow(
          uiOutput(
            "DATAANALYSIS__covariates_components"            
          )
        ),
        # TODO: The following really should be uiOutput and 
        # only shown if there's survival data in the preference
        # heirarchy.
        fluidRow(tags$h4("Censoring-related covariates")),
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
    
    
 
    
    DATAANALYSIS__covariates <- reactiveVal(list())
    
    # If an action was taken that causes structural change to 
    # Preference list (i.e. added/removed/re-ordered details)
    # Make these changes as neccesary 
    
    # Observer for forcing a UI update
    DATAANALYSIS__force_UI_update <- reactiveVal(0)
    
    observe({
      
      req(SYMBOLIC_LINK__data_sheet())
      data_sheet <- SYMBOLIC_LINK__data_sheet()
      
      input$DATAANALYSIS__uiUpdateId_update
      
      print(input$DATAANALYSIS__uiUpdateId)
      
      if (!is.null(input$DATAANALYSIS__uiUpdateId)) {
        
        currentMethod <- isolate(input$DATAANALYSIS__method)
        
        # Get preference list to modify
        covariates_tmp <- isolate(DATAANALYSIS__covariates())
        
        selectedId <- isolate(input$DATAANALYSIS__uiUpdateId)
        
        #Update preference list structure (e.g. shuffle, delete, etc) and then signal to update UI elements
        
        if(selectedId == "DATAANALYSIS__add_covariate"){
          
          # Add new preference component
          covariates_tmp[[length(covariates_tmp)+1]] <- list(
            var=colnames(data_sheet)[1],
            stratify=ifelse(currentMethod=="wins",TRUE,FALSE), # Default depends on method being used
            also_adjust=FALSE # used by PIM 
          )
          DATAANALYSIS__covariates(covariates_tmp)
          
        } else {
          
          # Pressed button is in the dynamic rows.
          # Action taken depends on the button type
          
          actIdLabels <- c(
            up="DATAANALYSIS__component_up_",
            down="DATAANALYSIS__component_down_",
            delete="DATAANALYSIS__component_delete_"
          )
          
          actType <- which(sapply(actIdLabels,grepl,x=selectedId))
          actType <- names(actIdLabels)[actType]
          if(length(actType)!=1){
            stop("module-ANALYSIS: Something is wrong")
          }
          
          change_number <- as.numeric(gsub(actIdLabels[actType],"",selectedId))
           
          if(is.na(change_number)) stop("unexpeced non-numeric value")
 
          if(actType == "up" & change_number > 1){
            new_order <- 1:length(covariates_tmp)

            new_order[change_number] <- change_number-1
            new_order[change_number-1] <- change_number

            covariates_tmp <- covariates_tmp[new_order]
          } else if (actType == "down" & change_number < length(covariates_tmp)){
            new_order <- 1:length(covariates_tmp)

            new_order[change_number] <- change_number+1
            new_order[change_number+1] <- change_number

            covariates_tmp <- covariates_tmp[new_order]

          } else if(actType == "delete"){
            covariates_tmp <- covariates_tmp[-change_number]
          }
          
          DATAANALYSIS__covariates(covariates_tmp)
          
        }

        # # Signal to UI to update
        tmp <- isolate(DATAANALYSIS__force_UI_update())
        tmp <- tmp+1
        tmp <- tmp %% 2
        DATAANALYSIS__force_UI_update(tmp)
        
      }
    })
    
    
    # Observer for changing DATAANALYSIS__covariates in response to UI input
    # This doesn't require the same janky JS script hooks as there's not really
    # any need to track what the last button press was
    observeEvent(
      lapply(
        inputCollection(
          c("DATAANALYSIS__component_var_",
            "DATAANALYSIS__component_stratify_"),
          1:length(isolate(DATAANALYSIS__covariates()))
        ),
        function(x) input[[x]]
      ),{

          # This is much cleaner than using JS tags, my god.
          # rewrite interactive bits for this module using this and then we can port
          # back over to preferencedefinition.

          # Get a list of all relevant components
          inputNames <- inputCollection(c("DATAANALYSIS__component_var_",
                                          "DATAANALYSIS__component_stratify_"),
                                        1:length(isolate(DATAANALYSIS__covariates())))

          
      
          obj <- lapply(inputNames,function(x){input[[x]]})
          names(obj) <- inputNames
          
          # Update the covariates list based on current inputs

          covariates_tmp <- isolate(DATAANALYSIS__covariates())
          
          if(length(covariates_tmp)>0){
            
            lapply(inputNames, function(i){
              
              # Extract out what the name "i" corresponds to
              varIdLabels <- c(
                var="DATAANALYSIS__component_var_",
                stratify="DATAANALYSIS__component_stratify_"
              )
              
              changeType <- which(sapply(varIdLabels,grepl,x=i))
              changeType <- names(varIdLabels)[changeType]
              if(length(changeType)!=1){
                stop("Something is wrong")
              }
              
              change_number <- as.numeric(gsub(varIdLabels[changeType],"",i))
              
              # Violating scope is not ideal but it works
              covariates_tmp[[change_number]][[changeType]] <<- obj[[i]]
              
              NULL
            })
            
            print(covariates_tmp)
            DATAANALYSIS__covariates(covariates_tmp)
            
          }
         


    })
    
    
    
    observe({
      
     #input$DATAANALYSIS_covariateId
      input$DATAANALYSIS_covariateId_update
      
      print(input$DATAANALYSIS_covariateId)
      
      
      if (!is.null(input$DATAANALYSIS_covariateId)) {
        
        changed_id <- isolate(input$DATAANALYSIS_covariateId)
        changed_value <- isolate(input[[changed_id]])
         
      # Parse out the type of change
        varIdLabels <- c(
          var="DATAANALYSIS__component_var_",
          stratify="DATAANALYSIS__component_stratify_"
        )
      
      changeType <- which(sapply(varIdLabels,grepl,x=changed_id))
      changeType <- names(varIdLabels)[changeType]
      if(length(changeType)!=1){
           stop("Something is wrong")
      }

      change_number <- as.numeric(gsub(varIdLabels[changeType],"",changed_id))

      if(is.na(change_number)) stop("unexpeced non-numeric value")
         
      # This should be superceded if everything is working
      # covariates_tmp <- isolate(DATAANALYSIS__covariates())
      # covariates_tmp[[change_number]][[changeType]] <- changed_value
      # print(covariates_tmp)
      # DATAANALYSIS__covariates(covariates_tmp)
      
      }
      
    })
    
    
    output$DATAANALYSIS__covariates_components <- renderUI({
      
      req(SYMBOLIC_LINK__data_sheet())
      
      # Trigger update of this section only when signaled to do so.
      # There is probably a better way to do this, but it works
      DATAANALYSIS__force_UI_update()
      
      
      currentMethod <- isolate(input$DATAANALYSIS__method)
      
        
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
              
              # Default values for this covariate are taken from whatever the currently
              # stored values are.
              
              
              # THERE'S A BUG IN THIS CURRENTLY:
              
              # Warning: Error in &&: invalid 'y' type in 'x && y'
              # It appears to be caused by incorrect typing, we're getting it as a string.
              
              # This may be a downstream bug from earlier handling of how we get the covariate list,
              # we'll chase it down from there.
              
              
              thisRow <- new_covariates[[i]]
              print(thisRow)
              if(is.null(thisRow)) return(NULL)
              
              fluidRow(
                flowLayout(
                  div(class="DATAANALYSIS_covariate_value",
                      selectInput(inputId = sprintf("DATAANALYSIS__component_var_%d",i),
                                  label = "",
                                  choices = colnames(data_sheet),
                                  selected=thisRow[["var"]]

                  )),
                  div(class="DATAANALYSIS_covariate_value",
                      checkboxInput(inputId = sprintf("DATAANALYSIS__component_stratify_%d",i),
                                    label = "Stratify?",
                                    value=thisRow[["stratify"]] #ifelse(currentMethod=="wins",TRUE,FALSE) # TODO: THIS SHOULD GRAB FROM THE COVARIATE LIST
                                  
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
