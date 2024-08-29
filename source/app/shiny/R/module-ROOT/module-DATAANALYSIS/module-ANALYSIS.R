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
      tags$script("$(document).on('click', '.DATAANALYSIS__surv_ui_updater', function () {
                              Shiny.onInputChange('DATAANALYSIS__surv_uiUpdateId',this.id);
                              Shiny.onInputChange('DATAANALYSIS__surv_uiUpdateId_update',Math.random());
                             });"),
      uiOutput("DATAANALYSIS__ui_options")
      # fluidRow(
      #   actionButton("DATAANALYSIS__save_button","DEV_ONLY: Save state for interactive testing")
      # )
    ),
    mainPanel(
      fluidRow(uiOutput("DATAANALYSIS__wins_output_ui"))
    )
  ),
  server_element = substitute({
    
    # Convenience function for getting a list of reactive inputs
    inputCollection <- function(input_names,input_index){
        c(t(outer(input_names,input_index,paste0)))
    }
    
    output$DATAANALYSIS__ui_options <- renderUI({
      req(SYMBOLIC_LINK__data_sheet())
      data_sheet <- isolate(SYMBOLIC_LINK__data_sheet())

      tagList(
        # Need a better way of describing these
        radioButtons("DATAANALYSIS__method",
                     label="Select analysis method:",
                     choices=c("Win Ratio Analysis"="wins",
                               "Probabilistic Index Model Analysis"="pim"
                               )),
        # All of this should be nested inside conditional panels
        fluidRow(selectInput(inputId = "DATAANALYSIS__arm",
                             label = "Intervention variable is:",
                             choices = c(colnames(data_sheet)) #TODO: add option to specify no variable, add everything in covariates
        )),
        fluidRow(
          uiOutput("DATAANALYSIS__arm_active") # TODO: probably swap this out with something in data import module for formatting stuff
        ),
        fluidRow(
          bsCollapsePanel("Statistical controls",
                          fluidPage(
                            fluidRow(
                              numericInput("DATAANALYSIS__alpha",label = "Type-I error (α)",
                                           value = 0.05,min = 0,max = 1,step = 0.01)
                            )
                          )
          )
        ),
        fluidRow(
          bsCollapsePanel("Adjust for Covariates",
          fluidPage(
            uiOutput(
              "DATAANALYSIS__covariates_components"            
            ),
            uiOutput(
              "DATAANALYSIS__covariate_options"            
            )
          )
          )
        ),
        fluidRow(
          conditionalPanel("input.DATAANALYSIS__method=='wins'",
                           bsCollapsePanel("Adjust for Censoring",
                                           fluidPage(
                                             # TODO: The following really should be uiOutput and 
                                             # only shown if there's survival data in the preference
                                             # heirarchy.
                                             # We can just make this element a variable and set it
                                             # as NULL when not relevant
                                             uiOutput(
                                               "DATAANALYSIS__surv_covariates_components"            
                                             ),
                                             uiOutput(
                                               "DATAANALYSIS__surv_covariate_options"            
                                             )
                                           )
                           )
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
    
    ### Dynamic Analysis Options #################################################
    
    # Covariate Control ---------------------------------------------------------
    
    DATAANALYSIS__covariates <- reactiveVal(list())
  
    # Observer for forcing a UI update
    DATAANALYSIS__force_covar_UI_update <- reactiveVal(0)
    
    output$DATAANALYSIS__covariates_components <- renderUI({
      
      req(SYMBOLIC_LINK__data_sheet())
      
      # Trigger update of this section only when signaled to do so.
      # There is probably a better way to do this, but it works
      DATAANALYSIS__force_covar_UI_update()
      
      currentMethod <- isolate(input$DATAANALYSIS__method)
      
      data_sheet <- isolate(SYMBOLIC_LINK__data_sheet())
      
      new_covariates <- isolate(DATAANALYSIS__covariates())
      
      add_covariate_button <-  fluidRow(
        actionButton("DATAANALYSIS__add_covariate",
                     "Add Covariate",
                     class="DATAANALYSIS__ui_updater")
      )  
      
      if(length(new_covariates)==0){
        out <- tagList(
          fluidRow(column(width=12,
                          add_covariate_button
          ))
        )
      } else {
        
        out <- tagList(
          do.call("tagList", lapply(1:length(new_covariates), function(i){
            
            # Default values for this covariate are taken from whatever the currently
            # stored values are.
            
            thisRow <- new_covariates[[i]]
            if(is.null(thisRow)) return(NULL)
            
            tagList(
              fluidRow(
                column(width=5,
                       selectInput(inputId = sprintf("DATAANALYSIS__component_var_%d",i),
                                   label = "",
                                   choices = colnames(data_sheet),
                                   selected=thisRow[["var"]]
                                   
                       )
                ),
                column(width=2,
                       checkboxInput(inputId = sprintf("DATAANALYSIS__component_stratify_%d",i),
                                     label = "Stratify?",
                                     value=thisRow[["stratify"]] #ifelse(currentMethod=="wins",TRUE,FALSE) # TODO: THIS SHOULD GRAB FROM THE COVARIATE LIST
                       )
                ),
                column(width=5,
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
              fluidRow(
                hr()
              )
            )
          })),
          
          fluidRow(column(width=12,
                          add_covariate_button
          )
          )
        )
      }
      
      out
      
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
            
            DATAANALYSIS__covariates(covariates_tmp)
            
          }
    })
    
    
    # If an action was taken that causes structural change to 
    # Preference list (i.e. added/removed/re-ordered details)
    # Make these changes as neccesary 
    
    observe({
      
      req(SYMBOLIC_LINK__data_sheet())
      data_sheet <- SYMBOLIC_LINK__data_sheet()
      
      input$DATAANALYSIS__uiUpdateId_update
      
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
        tmp <- isolate(DATAANALYSIS__force_covar_UI_update())
        tmp <- tmp+1
        tmp <- tmp %% 2
        DATAANALYSIS__force_covar_UI_update(tmp)
        
      }
    })
    
    output$DATAANALYSIS__covariate_options <- renderUI({
      
      out <- NULL

      # Only display options if covariates have been added
      covariates_tmp <- DATAANALYSIS__covariates()
      
      if(length(covariates_tmp)>0){
       out <- selectInput("DATAANALYSIS__covariate_strata_method",
                          label = "Stratification method",
                          choices =  c("Maentel-Haenszel"="MH-type",
                                       "Unstratified"="unstratified",
                                       "Proportional"="wt.stratum1",
                                       "Proportional (observed events)"="wt.stratum2",
                                       "Equal Weights"="equal")
                          )
       }
            
      out
      
    })
    
    
    # Survival Covariate Control ---------------------------------------------------------
    
    #TODO: This all needs reworked
    DATAANALYSIS__surv_covariates <- reactiveVal(list())
    
    # Observer for forcing a UI update
    DATAANALYSIS__force_surv_covar_UI_update <- reactiveVal(0)
    
    
    output$DATAANALYSIS__surv_covariates_components <- renderUI({
      
      req(SYMBOLIC_LINK__data_sheet())
      
      # Trigger update of this section only when signaled to do so.
      # There is probably a better way to do this, but it works
      DATAANALYSIS__force_surv_covar_UI_update()
      
      
      currentMethod <- isolate(input$DATAANALYSIS__method)
      
      
      data_sheet <- isolate(SYMBOLIC_LINK__data_sheet())
      
      new_covariates <- isolate(DATAANALYSIS__surv_covariates())
      
      add_covariate_button <-  fluidRow(
        actionButton("DATAANALYSIS__add_surv_covariate",
                     "Add Covariate",
                     class="DATAANALYSIS__surv_ui_updater")
      )  
      
      if(length(new_covariates)==0){
        out <- tagList(
          fluidRow(column(width=12,
                          add_covariate_button
          ))
        )
      } else {
        
        out <- tagList(
          do.call("tagList", lapply(1:length(new_covariates), function(i){
            
            # Default values for this covariate are taken from whatever the currently
            # stored values are.
            
            thisRow <- new_covariates[[i]]
            if(is.null(thisRow)) return(NULL)
            
            tagList(
              fluidRow(
                column(width=7,
                       selectInput(inputId = sprintf("DATAANALYSIS__surv_component_var_%d",i),
                                   label = "",
                                   choices = colnames(data_sheet),
                                   selected=thisRow[["var"]]
                                   
                       )
                ),
                column(width=5,
                       actionButton(inputId = sprintf("DATAANALYSIS__surv_component_up_%d",i),
                                    label = "Up",
                                    class="DATAANALYSIS__surv_ui_updater"
                       ),
                       actionButton(inputId = sprintf("DATAANALYSIS__surv_component_down_%d",i),
                                    label = "Down",
                                    class="DATAANALYSIS__surv_ui_updater"
                       ),
                       actionButton(inputId = sprintf("DATAANALYSIS__surv_component_delete_%d",i),
                                    label = "Delete",
                                    class="DATAANALYSIS__surv_ui_updater"
                       )   
                )
              ),
              fluidRow(
                hr()
              )
            )
          })),
          
          fluidRow(column(width=12,
                          add_covariate_button
          )
          )
        )
      }
      
      out
      
    })
    
    
    
    # If an action was taken that causes structural change to 
    # Preference list (i.e. added/removed/re-ordered details)
    # Make these changes as neccesary 
    
    observe({
      
      req(SYMBOLIC_LINK__data_sheet())
      data_sheet <- SYMBOLIC_LINK__data_sheet()
      
      input$DATAANALYSIS__surv_uiUpdateId_update
      

      if (!is.null(input$DATAANALYSIS__surv_uiUpdateId)) {
        
        # TODO: this is only relevant for WINS method.
        # We can probably cut this
        currentMethod <- isolate(input$DATAANALYSIS__method)
        
        # Get preference list to modify
        covariates_tmp <- isolate(DATAANALYSIS__surv_covariates())
        
        selectedId <- isolate(input$DATAANALYSIS__surv_uiUpdateId)
        
        #Update preference list structure (e.g. shuffle, delete, etc) and then signal to update UI elements
        
        if(selectedId == "DATAANALYSIS__add_surv_covariate"){
          
          # Add new preference component
          covariates_tmp[[length(covariates_tmp)+1]] <- list(
            var=colnames(data_sheet)[1]
          )
          DATAANALYSIS__surv_covariates(covariates_tmp)
          
        } else {
          
          # Pressed button is in the dynamic rows.
          # Action taken depends on the button type
          
          actIdLabels <- c(
            up="DATAANALYSIS__surv_component_up_",
            down="DATAANALYSIS__surv_component_down_",
            delete="DATAANALYSIS__surv_component_delete_"
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
          
          DATAANALYSIS__surv_covariates(covariates_tmp)
          
        }
        
        # # Signal to UI to update
        tmp <- isolate(DATAANALYSIS__force_surv_covar_UI_update())
        tmp <- tmp+1
        tmp <- tmp %% 2
        DATAANALYSIS__force_surv_covar_UI_update(tmp)
        
      }
    })
    
    # Observer for changing DATAANALYSIS__surv_covariates in response to UI input
    # This doesn't require the same janky JS script hooks as there's not really
    # any need to track what the last button press was
    observeEvent(
      lapply(
        inputCollection(
          c("DATAANALYSIS__surv_component_var_"),
          1:length(isolate(DATAANALYSIS__surv_covariates()))
        ),
        function(x) input[[x]]
      ),{
        
        # This is much cleaner than using JS tags, my god.
        # rewrite interactive bits for this module using this and then we can port
        # back over to preferencedefinition.
        
        # Get a list of all relevant components
        inputNames <- inputCollection(c("DATAANALYSIS__surv_component_var_"),
                                      1:length(isolate(DATAANALYSIS__surv_covariates())))
        
        
        
        obj <- lapply(inputNames,function(x){input[[x]]})
        names(obj) <- inputNames
        
        # Update the covariates list based on current inputs
        
        covariates_tmp <- isolate(DATAANALYSIS__surv_covariates())
        
        if(length(covariates_tmp)>0){
          
          lapply(inputNames, function(i){
            
            # Extract out what the name "i" corresponds to
            varIdLabels <- c(
              var="DATAANALYSIS__surv_component_var_"
            )
            
            changeType <- which(sapply(varIdLabels,grepl,x=i))
            changeType <- names(varIdLabels)[changeType]
            if(length(changeType)!=1){
              print(changeType)
              stop("DATAANALYSIS: Survival Value Change Observer: Something is wrong")
            }
            
            change_number <- as.numeric(gsub(varIdLabels[changeType],"",i))
            
            # Violating scope is not ideal but it works
            covariates_tmp[[change_number]][[changeType]] <<- obj[[i]]
            
            NULL
          })
          
          DATAANALYSIS__surv_covariates(covariates_tmp)
          
        }
      })
    
    
    output$DATAANALYSIS__surv_covariate_options <- renderUI({
      
      out <- NULL
      
      # Only display options if covariates have been added
      covariates_tmp <- DATAANALYSIS__surv_covariates()
      
      if(length(covariates_tmp)>0){
       choices <- c("Unadjusted"="unadjusted",
                    "IPCW"="ipcw",
                    "IPCW (Covariate Dependent)"="covipcw"
                    )
      } else {
        choices <- c("Unadjusted"="unadjusted",
                     "IPCW"="ipcw"
        )
      }
      
      out <- selectInput("DATAANALYSIS__surv_covariate_strata_method",
                         label = "Stratification method",
                         choices =  choices
      )
      
      out
    })
    
    
    # Analysis Outputs ---------------------------------------------------------
   
    # Relies on functions found in misc_functions.R
    
    # Store results of running analysis as a reactiveVal and then reference it
    # To get specific results in the UI
    
    DATAANALYSIS__results <- reactiveVal(NULL)
    
    observeEvent(input$DATAANALYSIS__analysis_go,{
      req(SYMBOLIC_LINK__data_sheet())
      req(SYMBOLIC_LINK__preference_export())
      
      data_sheet <- isolate(SYMBOLIC_LINK__data_sheet())
      preferences <- isolate(SYMBOLIC_LINK__preference_export())
      arm <- isolate(input$DATAANALYSIS__arm)

      # Parse out covariate details

      stratum <- NULL
      covariates <- NULL

      # print("Stratum are:")

      # print(isolate(DATAANALYSIS__covariates()))

      stratum <- sapply(isolate(DATAANALYSIS__covariates()), function(x){
        if(x[["stratify"]]){
          return(x[["var"]])
        } else {
          return(NULL)
        }
      })

      # print(stratum)

      # Need to get a list of strata
      if(length(stratum)>0){
        if(length(stratum)==1){
          n_strata <- length(unique(data_sheet[,stratum]))  
        } else {
          n_strata <- nrow(unique(data_sheet[,stratum]))          
        }
      } else {
        n_strata <- 0
      }

      # print("Covariates are:")

      # print(isolate(DATAANALYSIS__surv_covariates()))
      covariates <- sapply(isolate(DATAANALYSIS__surv_covariates()), function(x){
        return(x[["var"]])
      })

      # print(covariates)

      out <- NULL
      if(!is.null(arm)){
        #TODO: At present, this only supports heirarchical output.
        # This needs fixed for use with e.g. PIM
        
        
        alpha <- isolate(input$DATAANALYSIS__alpha)

        if(isolate(input$SYMBOLIC_LINK__preferenceType)=="heirarchical"){

          outcomes <- preferences$heirarchical

          levels <- isolate(input$DATAANALYSIS__arm_active_selectInput)
          levels <- c(
            levels,
            setdiff(levels(as.factor(data_sheet[,arm])),levels)
          )

          # Set up out objects and get total runs for progress bar
      
          total_run <- 1 # Main results
      
          if(length(outcomes)>1){
            decomposed_estimate <- lapply(1:length(outcomes), function(i){NULL})
      
            total_run <- total_run + 2*length(outcomes) # Run once for individual and once for cumulative
      
          } else {
            decomposed_estimate <- NULL
          }
      

          if(n_strata>0){
            if(length(outcomes)>1){

              estimates_by_stratum <- lapply(1:n_strata, function(i){NULL})

              total_run <- total_run + n_strata*(1+2*length(outcomes))

            } else {

              estimates_by_stratum <- lapply(1:n_strata, function(i){NULL})

              total_run <- total_run + n_strata

            }

          } else {
            estimates_by_stratum <- NULL
          }
          
          
          if(is.null(isolate(input$DATAANALYSIS__covariate_strata_method))){
            stratum.weight <- "unstratified"
          } else {
            stratum.weight <- isolate(input$DATAANALYSIS__covariate_strata_method)
          }
          
          if(is.null(isolate(input$DATAANALYSIS__surv_covariate_strata_method))){
            adjust.method <- "unadjusted"
          } else {
            adjust.method <- isolate(input$DATAANALYSIS__surv_covariate_strata_method)
          }
          
          withProgress(message = 'Analysing',detail = "Overall results", value = 0, {
            
            write(sprintf("Running Main Analysis"), stderr())

            estimate <- run_analysis(list(data = data_sheet,
                                          outcomes=outcomes,
                                          arm=arm,
                                          levels=levels,
                                          stratum = stratum,
                                          stratum.weight = stratum.weight,
                                          covariates = covariates,
                                          method = adjust.method,
                                          alpha = alpha
                                      ),
                                     "wins")

            write(sprintf("Done"), stderr())
            
            # Get decomposition of results by outcome facets
            if(length(outcomes)>1){
            
              write(sprintf("Decomposing outcomes by facets"), stderr())  
              for(i in 1:length(outcomes)){
                
                write(sprintf("Facet %d",i), stderr())  
                
                incProgress(1/total_run,detail = sprintf("Component %d..",i))
                
                estimate_by_outcome <- run_analysis(list(data = data_sheet,
                                                         outcomes=outcomes[i],
                                                         arm=arm,
                                                         levels=levels,
                                                         stratum = stratum,
                                                         stratum.weight = stratum.weight,
                                                         covariates = covariates,
                                                         method = adjust.method,
                                                         alpha = alpha
                                                  ),
                                                  "wins")

                colnames(estimate_by_outcome) <- paste(colnames(estimate_by_outcome))
                estimate_by_outcome <- cbind(level=i,
                                             level_var=outcomes[[i]]$var,
                                             estimate_by_outcome)
                
                write(sprintf("Done"), stderr())  
                write(sprintf("Facets 1 to %d",i), stderr())  
                
                incProgress(1/total_run,detail = sprintf("Component %d...",i))
                
                estimate_by_cumulative_outcome <- run_analysis(list(data = data_sheet,
                                                                    outcomes=outcomes[1:i],
                                                                    arm=arm,
                                                                    levels=levels,
                                                                    stratum = stratum,
                                                                    stratum.weight = stratum.weight,
                                                                    covariates = covariates,
                                                                    method = adjust.method,
                                                                    alpha = alpha
                                                                ),
                                                                "wins")

                
                colnames(estimate_by_cumulative_outcome)[-1] <- paste(colnames(estimate_by_cumulative_outcome)[-1],"cumulative",sep="_")
                estimate_by_cumulative_outcome <- cbind(level=i,
                                                        level_var=outcomes[[i]]$var,
                                                        estimate_by_cumulative_outcome)
                
                write(sprintf("Done"), stderr())  
                write(sprintf("Facets 1 to %d",i), stderr())  
                
                decomposed_estimate[[i]] <- left_join(
                  estimate_by_outcome,
                  estimate_by_cumulative_outcome,
                ) %>% arrange(outcome,level)
                
              }
              
              decomposed_estimate <- do.call("rbind",decomposed_estimate)
            } # End if decompose at top level
            
            
            if(n_strata>0){
              
              
              if(length(stratum)==1){
                strata_column <- data_sheet[,stratum]
              } else {
                strata_column <-apply(data_sheet[,stratum],1,paste)
              }
              


              stratified_data_sheet <- by(data = data_sheet,
                                          INDICES = strata_column,
                                          function(x){x}
              )
              
              
              # This is incredibly dumb but it will work
              strata_values <- by(data = data.frame(strata_column=strata_column),
                                  INDICES = strata_column,
                                  function(x){unique(x$strata_column)}
              )
              
              for(j in 1:length(stratified_data_sheet)){
                
                
                incProgress(1/total_run,detail = sprintf("Strata %d",j))
                
                strata_val <- strata_values[[j]]
                
                strata_estimate <-  run_analysis(list(data = stratified_data_sheet[[j]],
                                                      outcomes=outcomes,
                                                      arm=arm,
                                                      levels=levels,
                                                      stratum = stratum,
                                                      stratum.weight = stratum.weight,
                                                      covariates = covariates,
                                                      method = adjust.method,
                                                      alpha = alpha
                                                ),
                                                "wins")
                
                tmp_decomposed_estimate <- lapply(1:length(outcomes), function(i){NULL})
                
                if(length(outcomes)>1){
                  for(i in 1:length(outcomes)){
                    
                    incProgress(1/total_run,detail = sprintf("Strata %d Component %d..",j,i))
                    estimate_by_outcome <-  run_analysis(list(data = stratified_data_sheet[[j]],
                                                              outcomes=outcomes[i],
                                                              arm=arm,
                                                              levels=levels,
                                                              stratum = stratum,
                                                              stratum.weight = stratum.weight,
                                                              covariates = covariates,
                                                              method = adjust.method,
                                                              alpha = alpha
                                                        ),
                                                        "wins")
                      
                    
                    colnames(estimate_by_outcome) <- paste(colnames(estimate_by_outcome))
                    estimate_by_outcome <- cbind(level=i,
                                                 level_var=outcomes[[i]]$var,
                                                 estimate_by_outcome)
                    
                    incProgress(1/total_run,detail = sprintf("Strata %d Component %d...",j,i))
                    estimate_by_cumulative_outcome <-  run_analysis(list(data = stratified_data_sheet[[j]],
                                                                         outcomes=outcomes[1:i],
                                                                         arm=arm,
                                                                         levels=levels,
                                                                         stratum = stratum,
                                                                         stratum.weight = stratum.weight,
                                                                         covariates = covariates,
                                                                         method = adjust.method,
                                                                         alpha = alpha
                                                                    ),
                                                                    "wins")
                    
                    colnames(estimate_by_cumulative_outcome)[-1] <- paste(colnames(estimate_by_cumulative_outcome)[-1],"cumulative",sep="_")
                    estimate_by_cumulative_outcome <- cbind(level=i,
                                                            level_var=outcomes[[i]]$var,
                                                            estimate_by_cumulative_outcome)
                    
                    # THE INDEX IS WRONG HERE!!!!!!
                    
                    tmp_decomposed_estimate[[i]] <- left_join(
                      estimate_by_outcome,
                      estimate_by_cumulative_outcome
                    ) %>% arrange(outcome,level)
                    
                  }
                  
                  tmp_decomposed_estimate <- do.call("rbind",tmp_decomposed_estimate)
                  
                } else {
                  tmp_decomposed_estimate <- NULL
                }
                
                estimates_by_stratum[[j]] <- list(strata_val = strata_val,
                                                  estimate=strata_estimate,
                                                  decomposed_estimate=tmp_decomposed_estimate)
                
              } # End for strata
              
            } # End if strata
            
          }) # End with progress
         
          
          out <- list(estimate=estimate,decomposed_estimate=decomposed_estimate,estimates_by_stratum=estimates_by_stratum)
          
        } # End If Hierarchical
        
        
        
        
        
      } # End if not null arm
      
      print(out)
      DATAANALYSIS__results(out)
    })
    
    
    
    reactive_force_results_update <- reactiveVal(0)
    output$DATAANALYSIS__wins_output_ui <- renderUI({
      
      reactive_force_results_update()
      results <- DATAANALYSIS__results()
      print(results)
      
      print("Rendering UI output")
      
      out <- list()
      
      if(!is.null(results$estimate)){
        out[[length(out)+1]] <- fluidRow(tableOutput("DATAANALYSIS__wins_output"))
      }
    

      if(!is.null(results$decomposed_estimate)){
        out[[length(out)+1]] <- fluidRow(tableOutput("DATAANALYSIS__wins_output_decompsition"))
      }

      if(!is.null(DATAANALYSIS__results()$estimates_by_stratum)){
        out[[length(out)+1]] <- fluidRow(tableOutput("DATAANALYSIS__wins_output_by_stratum"))
      }

      
      out <- do.call("tagList",out)
      out
    })  
    
    output$DATAANALYSIS__wins_output <- renderTable({
      DATAANALYSIS__results()$estimate
    })
    
    output$DATAANALYSIS__wins_output_decompsition <- renderTable({
      DATAANALYSIS__results()$decomposed_estimate
    })
    
    
    output$DATAANALYSIS__wins_output_by_stratum <- renderTable({
      results <- DATAANALYSIS__results()
      out <- NULL
      
      if(length(results$estimates_by_stratum)>0){
        out <- do.call("rbind",lapply(1:length(results$estimates_by_stratum),function(i){
          cbind(strata=results$estimates_by_stratum[[i]]$strata_val,
                results$estimates_by_stratum[[i]]$estimate)
        }))
      }
      
      out
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
