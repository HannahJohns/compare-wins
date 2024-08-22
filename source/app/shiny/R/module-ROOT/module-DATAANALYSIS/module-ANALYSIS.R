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
      fluidRow(tableOutput("DATAANALYSIS__wins_output"))
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
                     choices=c("Win Ratio Analysis"="wins")), #"Probabilistic Index Model Analysis"
        # All of this should be nested inside conditional panels
        fluidRow(selectInput(inputId = "DATAANALYSIS__arm",
                             label = "Intervention variable is:",
                             choices = colnames(data_sheet)
        )),
        fluidRow(
          uiOutput("DATAANALYSIS__arm_active") # TODO: probably swap this out with something in data import module for formatting stuff
        ),
        fluidRow(
          bsCollapsePanel("Statistical controls (e.g. α)",
                          fluidPage(
                            fluidRow(
                              "TODO: OPTIONS GO HERE "            
                            )
                          )
          )
        ),
        fluidRow(
          bsCollapsePanel("Adjust for Covariates",
          fluidPage(
            uiOutput(
              "DATAANALYSIS__covariates_components"            
            )
          )
          )
        ),
        fluidRow(
          bsCollapsePanel("Adjust for Covariate-Dependent Censoring",
            fluidPage(
              # TODO: The following really should be uiOutput and 
              # only shown if there's survival data in the preference
              # heirarchy.
              # We can just make this element a variable and set it
              # as NULL when not relevant
              uiOutput(
                "DATAANALYSIS__surv_covariates_components"            
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
            
            print(covariates_tmp)
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
        tmp <- isolate(DATAANALYSIS__force_covar_UI_update())
        tmp <- tmp+1
        tmp <- tmp %% 2
        DATAANALYSIS__force_covar_UI_update(tmp)
        
      }
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
      
      print(input$DATAANALYSIS__surv_uiUpdateId)

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
          
          print(covariates_tmp)
          DATAANALYSIS__surv_covariates(covariates_tmp)
          
        }
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
      
      print("Stratum are:")
      
      print(isolate(DATAANALYSIS__covariates()))
      
      stratum <- sapply(isolate(DATAANALYSIS__covariates()), function(x){
        if(x[["stratify"]]){
          return(x[["var"]])
        } else {
          return(NULL)
        }
      })
      
      print(stratum)
      
      print("Covariates are:")
      
      print(isolate(DATAANALYSIS__surv_covariates()))      
      covariates <- sapply(isolate(DATAANALYSIS__surv_covariates()), function(x){
        return(x[["var"]])
      })
      
      print(covariates)
      
      out <- NULL
      if(!is.null(arm)){
        #TODO: At present, this only supports heirarchical output.
        # This needs fixed for use with e.g. PIM
        
        if(isolate(input$SYMBOLIC_LINK__preferenceType)=="heirarchical"){
          
          #TODO: Add options for stratification, ipcw and covariate ipcw adjustment etc
          
          levels <- isolate(input$DATAANALYSIS__arm_active_selectInput)
          levels <- c(
            levels,
            setdiff(levels(as.factor(data_sheet[,arm])),levels)
          )
          
          out <- wins_wrapper(data = data_sheet,
                              outcomes=preferences$heirarchical,
                              arm=arm,
                              levels=levels,
                              stratum = stratum,
                              stratum.weight = "MH-type",
                              covariates = covariates,
                              method = "unadjusted"
                              # pvalue = "two-sided",
                              # alpha = 0.05
          )

         
        }
    }
      
      print(out)
      DATAANALYSIS__results(out)
    })
    
    output$DATAANALYSIS__wins_output <- renderTable({
      DATAANALYSIS__results()$estimate
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
