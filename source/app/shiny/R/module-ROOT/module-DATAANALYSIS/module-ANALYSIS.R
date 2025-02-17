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
            "PREFDEF__preferenceType"="SYMBOLIC_LINK__preferenceType",
            "PREFDEF__preference_rank"="SYMBOLIC_LINK__preference_rank",
            "SETTINGS__settings" = "SYMBOLIC_LINK__settings"
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
    ### Server Elements #################################################
   
    ### Dynamic Analysis Options #################################################
    #' Large parts of this module are dynamicly generated
    #' based on what was supplied in terms of options, list-based approaches, etc
    
    #### Main UI Element ------------------
    #' The main UI control element
    output$DATAANALYSIS__ui_options <- renderUI({
      req(SYMBOLIC_LINK__data_sheet())
      data_sheet <- isolate(SYMBOLIC_LINK__data_sheet())
      
      tagList(
        radioButtons("DATAANALYSIS__method",
                     label="Select analysis method:",
                     choices=c("Win Ratio Analysis"="wins",
                               "Probabilistic Index Model Analysis"="pim"
                               # "DEBUG ONLY: SAVE TO DISK" = "debug"
                               )),
        uiOutput("DATAANALYSIS__effect_measure_ui"),
        # All of this should be nested inside conditional panels
        fluidRow(selectInput(inputId = "DATAANALYSIS__arm",
                             label = "Intervention variable is:",
                             choices = c(colnames(data_sheet))
        )),
        fluidRow(
          uiOutput("DATAANALYSIS__arm_active") # TODO: probably swap this out with something in data import module for formatting stuff
        ),
        fluidRow(
          bsCollapsePanel("Add Stratification/Adjust for Covariates",
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
        fluidRow(
          bsCollapsePanel("Statistical controls",
                          fluidPage(
                            
                            fluidRow(
                              numericInput("DATAANALYSIS__alpha",label = "Type-I error (α)",
                                           value = 0.05,min = 0,max = 1,step = 0.01)
                            ),
                            
                            # Some methods will have specific options they care about.
                            # Controls for these are here:
                            
                            conditionalPanel("input.DATAANALYSIS__method=='pim'",
                              fluidRow(
                                selectInput("DATAANALYSIS__pim_estimator",
                                            label = "Fitting Method",
                                            choices = c("Broyden/Newton"="estimator.nleqslv",
                                                        "Generalised Linear"="estimator.glm",
                                                        "Barzilai-Borwein"="estimator.BB"
                                                        )
                                            )
                              ),
                              # TODO: Need to add options here
                              # conditionalPanel("input.DATAANALYSIS__pim_estimator=='estimator.nleqslv'",
                              #                  fluidRow(
                              #                  "NLEQSV solver arguments here"
                              #                  )
                              # ),
                              # conditionalPanel("input.DATAANALYSIS__pim_estimator=='estimator.glm'",
                              #                  fluidRow(
                              #                    "GLM solver arguments here"
                              #                  )
                              # ),
                              conditionalPanel("input.DATAANALYSIS__pim_estimator=='estimator.BB' || input.DATAANALYSIS__pim_estimator=='estimator.glm'|",
                                               fluidRow(
                                                 numericInput("DATAANALYSIS__pim_estimator_max_iter",
                                                              label = "Maximum iterations",
                                                              value = 1500
                                                              )
                                               )
                              )
                            ) # End PIM controls
                            
                            
                            
                            
                          )
          )
        ),
        fluidRow(uiOutput("DATAANALYSIS__error_warning_ui")),
        fluidRow(uiOutput("DATAANALYSIS__go_button"))
        
      ) # End taglist
      
    })
    
    
    #### Go button -----------------------------
    #' Only shows if there's no pre-caught errors
    output$DATAANALYSIS__go_button <- renderUI({
      
      errors <- DATAANALYSIS__errors()
      
      if(length(errors)==0){
        label <- "Analyse!"
        disabled <- NULL
      } else {
        label <- "Check errors"
        disabled <- TRUE
      }
      
      actionButton("DATAANALYSIS__analysis_go",
                   label,
                   disabled=disabled)
    })
    
    #### Effect size Measure ------------------
    output$DATAANALYSIS__effect_measure_ui <- renderUI({
      
      #' Different methods support different effect size measures
      
      if(input$DATAANALYSIS__method == "wins"){
        choices <- c("Win Ratio"="winRatio",
                     "Win Odds" = "winOdds",
                     "Net Benefit" = "netBenefit"
        )
      } else {
        choices <- c("Win Odds" = "winOdds")
      }
      
      radioButtons("DATAANALYSIS__effect_measure",
                   label="Effect Size Measure:",
                   choices=choices
                   
      )
    })
    
    #### Selection active treatment arm ------------------
    output$DATAANALYSIS__arm_active <- renderUI({
      req(SYMBOLIC_LINK__data_sheet())
      
      data_sheet <- SYMBOLIC_LINK__data_sheet()
      
      
      levels <- levels(as.factor(data_sheet[,input$DATAANALYSIS__arm]))
      
      # Generally speaking,if coded 0/1, 1/2, etc we 
      # have the higher value is the intervention.
      levels <- rev(levels)
      
      selectInput(inputId = "DATAANALYSIS__arm_active_selectInput",
                  label = "Intervention group is:",
                  choices = levels
      )
      
      
      
    })

    
    
   
    #### Covariate Control ---------------------------------------------------------
    DATAANALYSIS__covariates <- reactiveVal(list())
  
    # Observer for forcing a UI update
    DATAANALYSIS__force_covar_UI_update <- reactiveVal(0)
    
    ##### Covariate UI update observer ---------------------------------------------------------
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
    
   
    ##### Covariate list UI  ---------------------------------------------------------
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
    
    
    ##### Covariate value change Observer   ---------------------------------------------------------
    # Observer for changing DATAANALYSIS__covariates in response to UI input
    # This doesn't require the same janky JS script hooks as there's not really
    # any need to track what the last button press was
    observeEvent(
      lapply(
        inputCollection(
          c("DATAANALYSIS__component_var_",                   # I don't know why it's necessary that we look up to
            "DATAANALYSIS__component_stratify_"),             # length()+1, but doing so fixes problems with this
          1:(length(isolate(DATAANALYSIS__covariates())) +1 ) # event not triggering elements of the final index get updated 
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
    
    ##### Covariate options UI ---------------------------------------------------------
    output$DATAANALYSIS__covariate_options <- renderUI({
      
      out <- NULL
      
      # Only display options if relevant
      covariates_tmp <- DATAANALYSIS__covariates()
      
      if(length(covariates_tmp)>0){
        
        n_strata <- sum(sapply(covariates_tmp,function(x){x$stratify}))
        
        if(n_strata>0){
          
          if(input$DATAANALYSIS__method=="wins"){
            
            # TODO: Add IVW to WINS wrapper
            
            choices <- c("Maentel-Haenszel"="MH-type",
                         "Unstratified"="unstratified",
                         "Proportional"="wt.stratum1",
                         "Proportional (observed events)"="wt.stratum2",
                         "Equal Weights"="equal")
          } else {
            
            # We should always be able to use inverse variance weighting
            
            choices <- c("Inverse Variance Weighting"="ivw",
                         "Unstratified"="unstratified"
            )
          }
          
          out <- selectInput("DATAANALYSIS__covariate_strata_method",
                             label = "Stratification method",
                             choices = choices
          )
          
        }
      }
      
      
      out
      
    })
    
    ### Survival Covariate Control ---------------------------------------------------------
    
    DATAANALYSIS__surv_covariates <- reactiveVal(list())
    
    # Observer for forcing a UI update
    DATAANALYSIS__force_surv_covar_UI_update <- reactiveVal(0)
    
    # If an action was taken that causes structural change to 
    # Preference list (i.e. added/removed/re-ordered details)
    # Make these changes as neccesary 
    
    ##### Survival covariates UI update observer ---------------------------------------------------------
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
    
    ##### Survival list UI ---------------------------------------------------------
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
    
    
    ##### Survival options UI ---------------------------------------------------------
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
                         label = "Censoring adjustment method",
                         choices =  choices
      )
      
      out
    })
    
    
    ##### Survival value change observer ---------------------------------------------------------
    # Observer for changing DATAANALYSIS__surv_covariates in response to UI input
    # This doesn't require the same janky JS script hooks as there's not really
    # any need to track what the last button press was
    observeEvent(
      lapply(
        inputCollection(
          c("DATAANALYSIS__surv_component_var_"),
          1:(length(isolate(DATAANALYSIS__surv_covariates()))+1)
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
    
 
    ### Errors and  Warnings -----------------------------------------------------
    
    #' Things that we know will cause a crash are useful to have as a reactive
    #' because they will let us hide the "Go" button if something looks fundamentally
    #' wrong. 
    #' Each error/warning is compiled as a list of <li> tags, and then combined
    #' together in a renderUI()
    DATAANALYSIS__errors <- reactive({
      
      DATAANALYSIS__method <- input$DATAANALYSIS__method
      
      data_sheet <- SYMBOLIC_LINK__data_sheet()
      preference.method <- input$SYMBOLIC_LINK__preferenceType
      preferences <- SYMBOLIC_LINK__preference_export()
      rankName <- input$SYMBOLIC_LINK__preference_rank
      
      arm <- input$DATAANALYSIS__arm
      
      out <- list()
      
      
      #### Error: More than two groups --------------------------------------- 
      #' COMPARE WINS is built for two-group problems.
      #' If treatment group has more than two unique values, warn about error
      #' 
      
      if(all(sapply(
        list(data_sheet,
             arm),
        length)>0)
      ) {
        if(length(unique(data_sheet[,arm]))!=2){
          out[[length(out)+1]] <- tags$li("Treatment group does not contain exactly 2 groups")
        }
      }
      
      #### Error: No rank name --------------------------------------- 
      #' 
      if(preference.method=="list" & rankName == ""){
        out[[length(out)+1]] <- tags$li("Invalid rank name for list-based preferences")
      }
      
      
      #### Error: PIM Incompatible --------------------------------------- 
      #' If running PIM, but we have incompatible heirarchical method in use:
      if(all(sapply(
        list(DATAANALYSIS__method,
             preference.method,
             preferences),
        length)>0)
      ) {
        
        if( DATAANALYSIS__method=='pim' & preference.method=="heirarchical" & length(preferences$heirarchical)>1 ){
          
          
          if(prod(sapply(preferences$heirarchical, function(x){x$type})=="numeric")!=1){
            out[[length(out)+1]] <- tags$li("Survival outcomes not supported by PIM")
          }
          
          if(prod(sapply(preferences$heirarchical, function(x){x$tau})==0)!=1){
            out[[length(out)+1]] <- tags$li("Non-zero thresholds for clinical difference not supported by PIM")
          }
        }
      }
      
      
      
  
      
      
      # End error checks
      out
      
    })
    
    DATAANALYSIS__warnings <- reactive({
      
      # Things that don't look like errors, but look problematic, are flagged here  
      
      out <- list()
      
      #### Warning: Win Odds and Censoring --------------------------------------- 
      #' If win odds is being calculated, and we have survival endpoints,
      #'  but no IPCW method is set, warn about this.
      preference.method <- input$SYMBOLIC_LINK__preferenceType
      preferences <- SYMBOLIC_LINK__preference_export()
      
      DATAANALYSIS__method <- input$DATAANALYSIS__method
      DATAANALYSIS__effect_measure <- input$DATAANALYSIS__effect_measure
      DATAANALYSIS__surv_covariate_strata_method <- input$DATAANALYSIS__surv_covariate_strata_method
      
      # When null we default to unadjusted
      if(is.null(DATAANALYSIS__surv_covariate_strata_method)){
        DATAANALYSIS__surv_covariate_strata_method <- "unadjusted"
      }
      
      if(all(sapply(
        list(DATAANALYSIS__method,
             DATAANALYSIS__effect_measure,
             DATAANALYSIS__surv_covariate_strata_method,
             preference.method),
        length)>0)
      ) {
        if( DATAANALYSIS__method=='wins'&
            DATAANALYSIS__effect_measure=='winOdds' &
            DATAANALYSIS__surv_covariate_strata_method=='unadjusted' &
            preference.method == "heirarchical"
        ){
          if("surv" %in% sapply(preferences$heirarchical,function(x){x$type})){
            out[[length(out)+1]] <- tags$li("The Win Odds may be biased in the presence 
                                             of censored survival data.
                                             Consider applying IPCW adjustment.
                                            ")
          }
        } # end if check fails
      } # End if check win odds censor is possible
      
      
      out
      
    })
    
    output$DATAANALYSIS__error_warning_ui <- renderUI({
      
      preferences <- SYMBOLIC_LINK__preference_export()
      
      errors <- DATAANALYSIS__errors() # All errors are included in message
      warnings <- DATAANALYSIS__warnings()
    
      
      
      if(length(errors)>0){
        
        if(length(warnings)>0){
          warnings <- tagList("Additional warnings:",
                              tags$ul(do.call("tagList",warnings))
                              )
        } else {
          warnings <- ""
        }
        
        out <-  bsCollapse(id = "analysis_warning",
                           bsCollapsePanel("Error",
                                           tags$ul(do.call("tagList",errors)),
                                           warnings,
                                           style="danger"),
                           open = "Error"
        )
      } else {
        
        if(length(warnings)>0){
          
          out <-  bsCollapse(id = "analysis_warning",
                             bsCollapsePanel("Warning",
                                             tags$ul(do.call("tagList",warnings)),
                                             style="warning"),
                             open = "Warning"
          )
          
        } else {
          out <- ""          
        }
      }

      out
      
    })
    
    # Analysis Outputs #####################
   
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
      covariates_effect <- NULL
      covariates_censor <- NULL

      # print("Stratum are:")
      # print(isolate(DATAANALYSIS__covariates()))

      stratum <- lapply(isolate(DATAANALYSIS__covariates()), function(x){
        if(x[["stratify"]]){
          return(x[["var"]])
        } else {
          return(NULL)
        }
      })
      stratum <- do.call("c",stratum)
      
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
      
      covariates_effect <- lapply(isolate(DATAANALYSIS__covariates()), function(x){
        if(x[["stratify"]]){
          return(NULL)
        } else {
          return(x[["var"]])
        }
      })
      covariates_effect <- do.call("c",covariates_effect)
      
      
      # print("Covariates are:")

      # print(isolate(DATAANALYSIS__surv_covariates()))
      covariates_censor <- sapply(isolate(DATAANALYSIS__surv_covariates()), function(x){
        return(x[["var"]])
      })

      # print(covariates_censor)

      out <- NULL
      if(!is.null(arm)){

        
        # Set up any inputs we want to reference/transform/etc
        
        alpha <- isolate(input$DATAANALYSIS__alpha)
        
        levels <- isolate(input$DATAANALYSIS__arm_active_selectInput)
        levels <- c(
          levels,
          setdiff(levels(as.factor(data_sheet[,arm])),levels)
        )
        
        preference.method <- isolate(input$SYMBOLIC_LINK__preferenceType)
        statistical.method <- isolate(input$DATAANALYSIS__method)
        effect.measure <- isolate(input$DATAANALYSIS__effect_measure)
        
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
        
        estimator_method <- isolate(input$DATAANALYSIS__pim_estimator)
        
        max_iter <- isolate(input$DATAANALYSIS__pim_estimator_max_iter)

        settings <- SYMBOLIC_LINK__settings()
        
        # If no log file is to be created, set logFile to "NUL" and windows will
        # dump the output

        logFile <- "" # Write to console for the time being
        if(settings$log_enabled){
          logFile <- sprintf(
            "%s/log-%s-%s.txt",
            settings$log_dir,
            Sys.info()["login"],
            gsub("[[:space:][:space:][:punct:]]+","",Sys.time())
          )
        } else {
         logFile <- "NUL" #Discard logs as they're made.
        }
        
        cat(sprintf("%s\nBegin COMPARE WINS log file at %s\n\tAnalysis run by %s on machine %s\n%s\n",
                    paste(rep("=",80),collapse = ""),
                    as.character(Sys.time()),
                    Sys.info()["user"],
                    Sys.info()["nodename"],
                    paste(rep("=",80),collapse = "")
        ),
        file = logFile, append = FALSE)
        
        # Example of what summary of data frame will look like.
        # Just need to get all results 
        
        cat(
          sprintf("Preferences are defined using the %s method and are as follows:\n",
                  preference.method),
          file=logFile,
          append = TRUE
        )
        
        if(preference.method == "heirarchical"){
          
          cat(paste(capture.output({
            pander::pandoc.table(
              do.call("rbind",
                      lapply(preferences[[preference.method]], function(x){do.call("data.frame",x)})
              ),
              style="grid",
              split.tables=Inf
            )
          }),
          collapse="\n"),
          file=logFile,
          append = TRUE
          )
          
          
        } else if (preference.method == "list"){
          
          cat(paste(capture.output({
            pander::pandoc.table(
              preferences[[preference.method]],
              style="grid",
              split.tables=Inf
            )
          }),
          collapse="\n"),
          file=logFile,
          append = TRUE
          )
          
        }

        cat(sprintf("\n\nAnalysis compares preferences across %s, where %s is the intervention group\n",
                    arm,
                    levels[length(levels)]
                    ),
            file=logFile,
            append = TRUE
        )
        
        if(length(stratum)>0){
          cat("Analysis is stratified by %s\n",
              paste(stratum, collapse="; "),
              file=logFile,
              append = TRUE
              )
          cat("Stratum are combined using %s weights\n",
              stratum.weight,
              file=logFile,
              append = TRUE
              )
        }

        if(length(covariates_effect)>0){
          cat("Effects are adjusted for %s\n",
              paste(covariates_effect, collapse="; "),
              file=logFile,
              append = TRUE
          )
        }
        
        
        cat(sprintf("Any censoring in outcomes is adjusted for using %s\n",
            adjust.method),
            file=logFile,
            append = TRUE
        )

        if(length(covariates_censor)>0){
          cat(sprintf("The following variables are used to adjust for censoring: %s",
                paste(covariates_censor, collapse="; ")),
              file=logFile,
              append = TRUE
                )
        }
        
        cat(sprintf("Allowable Type-I error (alpha) set at %0.4f\n",
            alpha),
            file=logFile,
            append = TRUE
        )
        
        cat(sprintf("Estimation method is %s\n",
            estimator_method),
            file=logFile,
            append = TRUE
        )
        
        cat(sprintf("Maximum iterations is %d\n",
            max_iter),
            file=logFile,
            append = TRUE
        )
        
        
        cat("Summary of input data:\n",
            file = logFile,
            append = TRUE
            )
        
        table1_formula <- arm
        
        if(preference.method == "heirarchical"){
          
          table1_formula <- paste0(
            table1_formula,"+",
            paste(
              sapply(preferences[[preference.method]], function(x){x$var}),
              collapse="+"
            )
          )
          
        } else if(preference.method == "list"){
          # TODO: This needs written
          
          table1_formula <- paste0(
            table1_formula,"+",
            paste(
              intersect(colnames(data_sheet),colnames(preferences[[preference.method]])),
              collapse="+"
            )
          )
          
        }
        
        if(length(stratum)>0){
          table1_formula <- paste0(
            table1_formula,"+",
            paste(
              stratum,
              collapse="+"
            )
          )
        }
        
        if(length(covariates_effect)>0){
          table1_formula <- paste0(
            table1_formula,"+",
            paste(
              covariates_effect,
              collapse="+"
            )
          )
        }
        
        if(length(covariates_censor)>0){
          table1_formula <- paste0(
            table1_formula,"+",
            paste(
              covariates_censor,
              collapse="+"
            )
          )
        }
        
        table1_formula <- sprintf("~%s|%s",table1_formula,arm)
        
        
        # saveRDS(list(table1_formula=table1_formula,tab1_data_sheet = tab1_data_sheet),"tmp.RDS")
        # tmp <- readRDS("./source/app/shiny/tmp.RDS")
        # attach(tmp)
        
        cat(paste(capture.output({
          pander::pandoc.table(
            as.data.frame(table1::table1(as.formula(table1_formula),
                                 data= (function(x){
                                   
                                   to_factorise <- c(arm,stratum)
                                  
                                   if(preference.method == "list"){
                                     to_factorise <- c(to_factorise,
                                                       intersect(
                                                         colnames(data_sheet),
                                                         colnames(preferences[[preference.method]])
                                                       ) 
                                     )
                                   }

                                   
                                   for( i in to_factorise)
                                     x[,i] <- as.factor(x[,i])
                                   x
                                 })(data_sheet),
                                 render.continuous = c(.="Mean (SD)",
                                                       .="Median [Q1 - Q3]")
            )
            ),
            style="grid",
            split.tables=Inf
          )
        }),collapse="\n"),
        file=logFile,
        append = TRUE
        )
        
        cat("\n\n",
            file = logFile,
            append = TRUE
        )
        
        
        # All methods should should show a progress bar
        
        # Delay showing errors/warnings until after all results are run.
        errorList <- list()
        warningList <- list()

        withProgress(message = 'Analysing',detail = "Overall results", value = 0, {
        
        ### Analysis loop (heirarchy-based) --------------    
        if(preference.method=="heirarchical"){
          
          outcomes <- preferences$heirarchical
          
          # Set up progress bar total runs for progress bar
          
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
          
          # write(sprintf("Running Main Analysis"), stderr())
          
          estimate <- run_analysis(list(data = data_sheet,
                                        outcomes=outcomes,
                                        arm=arm,
                                        levels=levels,
                                        stratum = stratum,
                                        stratum.weight = stratum.weight,
                                        covariates_effect = covariates_effect,
                                        covariates_censor = covariates_censor,
                                        method = adjust.method,
                                        alpha = alpha,
                                        estimator_method = estimator_method,
                                        max_iter = max_iter
          ),
          statistical.method,
          effect.measure)
          
          cat("Primary Analysis Results:\n",
              file = logFile,
              append = TRUE
              )
          
          if(!is.null(estimate$out)){
            cat(
              paste(capture.output({
                pander::pandoc.table(
                  estimate$out,
                  style="grid",
                  split.tables=Inf
                )
              }),collapse="\n"),
            file=logFile,
            append = TRUE
            )
          }
          
          if(!is.null(estimate$error)){
            cat("Returned error:\n",
                file = logFile,
                append = TRUE
                )
            cat(sprintf("%s\n",estimate$error$message),
                file = logFile,
                append = TRUE
                )
          }
          
          if(!is.null(estimate$warning)){
            cat("Produced warning:\n",
                file = logFile,
                append = TRUE
                )
            cat(sprintf("%s\n",estimate$warning$message),
                file = logFile,
                append = TRUE
                )
          }
          
          errorList[[length(errorList)+1]] <- estimate$error
          warningList[[length(warningList)+1]] <- estimate$warning
          estimate <- estimate$out
          
          write(sprintf("Done"), stderr())
          
          ####Decomposed results -----------------------
          if(length(outcomes)>1){
            
            write(sprintf("Decomposing outcomes by facets"), stderr())  
            cat("Breakdown by individual outcomes:\n",
                file = logFile,
                append = TRUE
                )
            
            for(i in 1:length(outcomes)){
              
              
              #### Individual Results by outcome -----------------------
              
              write(sprintf("Facet %d",i), stderr())  
              incProgress(1/total_run,detail = sprintf("Component %d..",i))
              cat(sprintf("Facet %d\n",i),
                  file = logFile,
                  append = TRUE
                  )
              
              
              estimate_by_outcome <- run_analysis(list(data = data_sheet,
                                                       outcomes=outcomes[i],
                                                       arm=arm,
                                                       levels=levels,
                                                       stratum = stratum,
                                                       stratum.weight = stratum.weight,
                                                       covariates_effect = covariates_effect,
                                                       covariates_censor = covariates_censor,
                                                       method = adjust.method,
                                                       alpha = alpha,
                                                       estimator_method = estimator_method,
                                                       max_iter = max_iter
              ),
              statistical.method,
              effect.measure)
              
              if(!is.null(estimate_by_outcome$out)){
                cat(paste(capture.output({
                  pander::pandoc.table(
                    estimate_by_outcome$out,
                    style="grid",
                    split.tables=Inf
                  )
                }),collapse="\n"),
                file=logFile,
                append = TRUE
                )
              }
              
              if(!is.null(estimate_by_outcome$error)){
                cat("Returned error:\n",
                    file = logFile,
                    append = TRUE
                    )
                cat(sprintf("%s\n",estimate_by_outcome$error$message),
                    file = logFile,
                    append = TRUE
                    )
              }
              
              if(!is.null(estimate_by_outcome$warning)){
                cat("Produced warning:\n",
                    file = logFile,
                    append = TRUE
                    )
                cat(sprintf("%s\n",estimate_by_outcome$warning$message),
                    file = logFile,
                    append = TRUE
                    )
              }
              
              
              errorList[[length(errorList)+1]] <- estimate$error
              warningList[[length(warningList)+1]] <- estimate$warning
              estimate <- estimate$out
              
              write(sprintf("Done"), stderr())
              
              
              
              
              errorList[[length(errorList)+1]] <- estimate_by_outcome$error
              warningList[[length(warningList)+1]] <- estimate_by_outcome$warning
              estimate_by_outcome <- estimate_by_outcome$out
              
              
              colnames(estimate_by_outcome) <- paste(colnames(estimate_by_outcome))
              estimate_by_outcome <- cbind(level=i,
                                           level_var=outcomes[[i]]$var,
                                           estimate_by_outcome)
              
              write(sprintf("Done"), stderr())  
              
              #### Cumulative Results by outcome -----------------------
              
              # Note: We're not putting this in the log file, it's only
              # being calculated so we can produce our plots.
              
              write(sprintf("Facets 1 to %d",i), stderr())  
              
              incProgress(1/total_run,detail = sprintf("Component %d...",i))
              
              estimate_by_cumulative_outcome <- run_analysis(list(data = data_sheet,
                                                                  outcomes=outcomes[1:i],
                                                                  arm=arm,
                                                                  levels=levels,
                                                                  stratum = stratum,
                                                                  stratum.weight = stratum.weight,
                                                                  covariates_effect = covariates_effect,
                                                                  covariates_censor = covariates_censor,
                                                                  method = adjust.method,
                                                                  alpha = alpha,
                                                                  estimator_method = estimator_method,
                                                                  max_iter = max_iter
              ),
              statistical.method,
              effect.measure)
              
              errorList[[length(errorList)+1]] <- estimate_by_cumulative_outcome$error
              warningList[[length(warningList)+1]] <- estimate_by_cumulative_outcome$warning
              estimate_by_cumulative_outcome <- estimate_by_cumulative_outcome$out
              
              
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
          
          
          ##### Stratified Analysis ----- 
          
          if(n_strata>0){
            cat("Breakdown by strata:\n",
                file = logFile,
                append = TRUE
                )

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
              cat(sprintf("Strata %d\n",j),
                  file = logFile,
                  append = TRUE
                  )
              
              strata_val <- strata_values[[j]]
              
              strata_estimate <-  run_analysis(list(data = stratified_data_sheet[[j]],
                                                    outcomes=outcomes,
                                                    arm=arm,
                                                    levels=levels,
                                                    stratum = NULL,
                                                    stratum.weight = "unstratified",
                                                    covariates_effect = covariates_effect,
                                                    covariates_censor = covariates_censor,
                                                    method = adjust.method,
                                                    alpha = alpha,
                                                    estimator_method = estimator_method,
                                                    max_iter = max_iter
              ),
              statistical.method,
              effect.measure)
              
              
              if(!is.null(strata_estimate$out)){
                cat(paste(capture.output({
                  pander::pandoc.table(
                    strata_estimate$out,
                    style="grid",
                    split.tables=Inf
                  )
                }),collapse="\n"),
                file=logFile,
                append = TRUE
                )
              }
              
              if(!is.null(strata_estimate$error)){
                cat("Returned error:\n",
                    file = logFile,
                    append = TRUE
                    )
                cat(sprintf("%s\n",strata_estimate$error$message),
                    file = logFile,
                    append = TRUE
                    )
              }
              
              if(!is.null(strata_estimate$warning)){
                cat("Produced warning:\n",
                    file = logFile,
                    append = TRUE
                    )
                cat(sprintf("%s\n",strata_estimate$warning$message),
                    file = logFile,
                    append = TRUE
                    )
              }
              
              errorList[[length(errorList)+1]] <- strata_estimate$error
              warningList[[length(warningList)+1]] <- strata_estimate$warning
              strata_estimate <- strata_estimate$out
              
              
              ###### Decomposed outcomes by strata --------
              # NOTE: We're not returning this in the analysis log
              # it's far too much detail.
              
              tmp_decomposed_estimate <- lapply(1:length(outcomes), function(i){NULL})
              
              if(length(outcomes)>1){
                for(i in 1:length(outcomes)){
                  
                  incProgress(1/total_run,detail = sprintf("Strata %d Component %d..",j,i))
                  estimate_by_outcome <-  run_analysis(list(data = stratified_data_sheet[[j]],
                                                            outcomes=outcomes[i],
                                                            arm=arm,
                                                            levels=levels,
                                                            stratum = NULL,
                                                            stratum.weight = "unstratified",
                                                            covariates_effect = covariates_effect,
                                                            covariates_censor = covariates_censor,
                                                            method = adjust.method,
                                                            alpha = alpha,
                                                            estimator_method = estimator_method,
                                                            max_iter = max_iter
                  ),
                  statistical.method,
                  effect.measure)
                  
                  errorList[[length(errorList)+1]] <- estimate_by_outcome$error
                  warningList[[length(warningList)+1]] <- estimate_by_outcome$warning
                  estimate_by_outcome <- estimate_by_outcome$out
                  
                  
                  colnames(estimate_by_outcome) <- paste(colnames(estimate_by_outcome))
                  estimate_by_outcome <- cbind(level=i,
                                               level_var=outcomes[[i]]$var,
                                               estimate_by_outcome)
                  
                  incProgress(1/total_run,detail = sprintf("Strata %d Component %d...",j,i))
                  estimate_by_cumulative_outcome <-  run_analysis(list(data = stratified_data_sheet[[j]],
                                                                       outcomes=outcomes[1:i],
                                                                       arm=arm,
                                                                       levels=levels,
                                                                       stratum = NULL,
                                                                       stratum.weight = "unstratified",
                                                                       covariates_effect = covariates_effect,
                                                                       covariates_censor = covariates_censor,
                                                                       method = adjust.method,
                                                                       alpha = alpha,
                                                                       estimator_method = estimator_method,
                                                                       max_iter = max_iter
                  ),
                  statistical.method,
                  effect.measure)
                  
                  errorList[[length(errorList)+1]] <- estimate_by_cumulative_outcome$error
                  warningList[[length(warningList)+1]] <- estimate_by_cumulative_outcome$warning
                  estimate_by_cumulative_outcome <- estimate_by_cumulative_outcome$out
                  
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
          
          out <- list(estimate=estimate,
                      decomposed_estimate=decomposed_estimate,
                      estimates_by_stratum=estimates_by_stratum,
                      logFile = logFile)
         
        ### Analysis loop (list-based) --------------
        } else if (preference.method=="list"){
          
          # The easiest way to get list-based analysis working is to treat it
          # like it's heirarchical with only a single facet.
          
          rankName <- isolate(input$SYMBOLIC_LINK__preference_rank)
          preference_list <- preferences$list
          
          # If rankName is already in the data sheet or not in preference list,
          # we should stop
          
          if(rankName %in% colnames(data_sheet) | !(rankName %in% colnames(preference_list))){
            
            errorList[[length(errorList)+1]] <- tryCatch(stop("Invalid rank column name"),
                                                         error=function(e){e})
            
            
          } else {
            
            # Set up outcomes as if it were a singular continuous outcome facet
            
            #TODO: This is is dangerous and needs some checks added
            # There's a bug that occurs when we have duplicated names.
            # Set name to Rankin and see what happens.
            data_sheet <- left_join(
              data_sheet,
              preference_list
            )
            
            # With the data sheet built and before analysis, update any reactives that depend on it
            DATAANALYSIS__xtab( table(data_sheet[,arm],data_sheet[,rankName])[levels,] )
            
            outcomes <- list( list(
              type="numeric",
              var=rankName,
              tau=0,
              indicator=NULL,
              direction="<" # Rank 1 is best
            ))
            
            # if(statistical.method != "debug"){
            #   errorList[[length(errorList)+1]] <- tryCatch({stop("List method currently in debug mode only")},error=function(e){e})
            # }
            
            total_run <- 1 # Main results
            
            if(n_strata>0){
              estimates_by_stratum <- lapply(1:n_strata, function(i){NULL})
              total_run <- total_run + n_strata
            } else {
              estimates_by_stratum <- NULL
            }
            
            
            write(sprintf("Running Main Analysis"), stderr())
            
            estimate <- run_analysis(list(data = data_sheet,
                                          outcomes=outcomes,
                                          arm=arm,
                                          levels=levels,
                                          stratum = stratum,
                                          stratum.weight = stratum.weight,
                                          covariates_effect = covariates_effect,
                                          covariates_censor = covariates_censor,
                                          method = adjust.method,
                                          alpha = alpha,
                                          estimator_method = estimator_method,
                                          max_iter = max_iter
            ),
            statistical.method,
            effect.measure)
            
            cat("Primary Analysis Results:\n",
                file = logFile,
                append = TRUE
                )
            
            if(!is.null(estimate$out)){
              cat(paste(capture.output({
                pander::pandoc.table(
                  estimate$out,
                  style="grid",
                  split.tables=Inf
                )
              }),collapse="\n"),
              file=logFile,
              append = TRUE
              )
            }
            
            if(!is.null(estimate$error)){
              cat("Returned error:\n",
                  file = logFile,
                  append = TRUE
                  )
              cat(sprintf("%s\n",estimate$error$message),
                  file = logFile,
                  append = TRUE
                  )
            }
            
            if(!is.null(estimate$warning)){
              cat("Produced warning:\n",
                  file = logFile,
                  append = TRUE
                  )
              cat(sprintf("%s\n",estimate$warning$message),
                  file = logFile,
                  append = TRUE
                  )
            }
            
            
            errorList[[length(errorList)+1]] <- estimate$error
            warningList[[length(warningList)+1]] <- estimate$warning
            estimate <- estimate$out
            
            write(sprintf("Done"), stderr())
            
            ##### Stratified Analysis ------
            
            if(n_strata>0){
              
              write(sprintf("Running Stratification"), stderr())
              cat("Breakdown by strata:\n",
                  file = logFile,
                  append = TRUE
                  )
              
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
                
                cat(sprintf("Strata %d: %s\n",j, strata_val),
                    file = logFile,
                    append = TRUE
                    )
                
                strata_estimate <-  run_analysis(list(data = stratified_data_sheet[[j]],
                                                      outcomes=outcomes,
                                                      arm=arm,
                                                      levels=levels,
                                                      stratum = stratum,
                                                      stratum.weight = stratum.weight,
                                                      covariates_effect = covariates_effect,
                                                      covariates_censor = covariates_censor,
                                                      method = adjust.method,
                                                      alpha = alpha,
                                                      estimator_method = estimator_method,
                                                      max_iter = max_iter
                ),
                statistical.method,
                effect.measure)
                
                
                if(!is.null(strata_estimate$out)){
                  cat(paste(capture.output({
                    pander::pandoc.table(
                      strata_estimate$out,
                      style="grid",
                      split.tables=Inf
                    )
                  }),collapse="\n"),
                  file=logFile,
                  append = TRUE
                  )
                }
                
                if(!is.null(strata_estimate$error)){
                  cat("Returned error:\n",
                      file = logFile,
                      append = TRUE
                      )
                  cat(sprintf("%s\n",strata_estimate$error$message),
                      file = logFile,
                      append = TRUE
                      )
                }
                
                if(!is.null(strata_estimate$warning)){
                  cat("Produced warning:\n",
                      file = logFile,
                      append = TRUE
                      )
                  cat(sprintf("%s\n",strata_estimate$warning$message),
                      file = logFile,
                      append = TRUE
                      )
                }
                
                errorList[[length(errorList)+1]] <- strata_estimate$error
                warningList[[length(warningList)+1]] <- strata_estimate$warning
                strata_estimate <- strata_estimate$out
                
                estimates_by_stratum[[j]] <- list(strata_val = strata_val,
                                                  estimate=strata_estimate,
                                                  decomposed_estimate=NULL)
                
              } # End for strata
              
            } # End if strata
            
            out <- list(estimate=estimate,
                        decomposed_estimate=NULL,
                        estimates_by_stratum=estimates_by_stratum,
                        logFile = logFile)
            
          } # End if valid rank variable name

        } else {
          
          # Default error message
          errorList[[length(errorList)+1]] <- tryCatch({stop("Unrecognised preference method")},error=function(e){e})
          
        }
          
          
        }) # End progress bar
        
        write(sprintf("Analysis done. Collating error reports"), stderr())
        
        # After progress bar, show collected error messages
        n_errors <- length(errorList)
        n_warnings <- length(warningList)
        
        if(n_errors+n_warnings>0){
          
          
          # print(errorList)
          # print(warningList)
          
          messageTitle <- ifelse(n_errors>0,"Error","Warning")
          
          if(n_errors>0){
            errorText <-  table(sapply(errorList,function(x){x$message}))
            errorText <- sapply(1:length(errorText),function(i){sprintf("<b>%d Error%s:</b> %s",errorText[i],ifelse(errorText[i]>1,"s",""), names(errorText)[i])})
            errorText <- paste(errorText,collapse="<br>")
          } else {
            errorText <- ""
          }
          
          if(n_warnings>0){
            warningText <-  table(sapply(warningList,function(x){x$message}))
            warningText <- sapply(1:length(warningText),function(i){sprintf("<b>%d Warning%s:</b> %s",warningText[i],ifelse(warningText[i]>1,"s",""), names(warningText)[i])})
            warningText <- paste(warningText,collapse="<br>")
          } else {
            warningText <- ""
          }
          
          text <- sprintf("%s%s%s",
                          errorText,
                          ifelse(n_errors>0 & n_warnings>0, "<br>",""),
                          warningText
          )
          
          showModal(modalDialog(
            title=messageTitle,
            HTML(text),
            easyClose=TRUE,
            footer=NULL
          ))
          
        }
        
        write(sprintf("Done"), stderr())
        
      } # End if not null arm
      
      write(sprintf("Analysis finished. Returning results to reactives."), stderr())
      # print(out)
      DATAANALYSIS__results(out)
      
      write(sprintf("Done"), stderr())
    })
    
    
    #### Output UI dynamic elements ---------------------------------------------
    
    methods_changed <- reactiveVal(T)
    
    methods_writeup <- reactive({
      
      
      # TODO:
      # Construct a text summary of analysis method to be performed.
      # Tag it as preliminary or not based on methods_changed
      # (which needs its own checks, to be implemented next)
    
      textBody <- ""
      
      arm <- input$DATAANALYSIS__arm
      if(is.null(arm)){
        textBody <-  paste(textBody,"There is no intervention variable declared.")
      } else {
        textBody <-  paste(textBody,
                           sprintf("We estimated difference in outcome preference between <b>[%s]</b> groups",
                                   arm
                           ))
        
        # levels <- isolate(input$DATAANALYSIS__arm_active_selectInput)
        # levels <- c(
        #   levels,
        #   setdiff(levels(as.factor(data_sheet[,arm])),levels)
        # )
        
      }
      
      preference.method <- input$SYMBOLIC_LINK__preferenceType
      if(is.null(preference.method)){
        textBody <-  paste(textBody,"There is no preference method loaded.")
      } else {
        textBody <-  paste(textBody,
                           sprintf(", where all-to-all preferences are defined using a %s approach",
                                   c(heirarchical="heirarchical (rule) based",list="list based")[preference.method]
                                   ))
        
        preferences <- SYMBOLIC_LINK__preference_export()
        if(is.null(preference.method)){
          textBody <-  paste0(textBody,"No preference specification is loaded")
        } else {
          
          # TODO: Get summary of preference specification
          if(preference.method=="heirarchical"){
            
            preference_specification <- sprintf(
              ". Outcome facets are, in decreasing order of importance, %s",
              paste(do.call("c",
                lapply(preferences$heirarchical, function(x){
                  sprintf("<b>[%s]</b> (%s%s)",
                          x$var,
                          ifelse(x$direction==">","Higher is better","Lower is better"),
                          ifelse(x$tau>0,sprintf(", Min. important difference %f",x$tau),"")
                          )
                })
              ), collapse="; ")
            )
            
          } else if (preference.method=="list") {
            
            preference_specification <- "(Table X)"
            
          } else {
            preference_specification <- "<b>[UNRECOGNISED PREFERENCE SPECIFICATION]</b>"
          }
          
          textBody <-  sprintf("%s%s",
                               textBody,
                               preference_specification
                               )
          
        }
      }
      
      
      effect.measure <- input$DATAANALYSIS__effect_measure
      if(!is.null(effect.measure)){
        textBody <-  sprintf("%s. Difference in outcome preference is summarised using the %s statistic",
                             textBody,
                             c(winRatio="Win Ratio",
                               winOdds="Win Odds",
                               netBenefit="Net Benefit")[effect.measure]
        )
      }
      
      statistical.method <- input$DATAANALYSIS__method
      
      if(statistical.method=="wins"){
        statistical.method <- "Win Statistics, provided by the WINS package"
      } else if (statistical.method=="pim"){
        statistical.method <- "Probabilistic Index Models, provided by the pim package"
      }
      
      if(!is.null(statistical.method)){
        textBody <-  sprintf("%s, calculated using %s",
                             textBody,
                             statistical.method
        )
      }

      stratum <- NULL
      covariates_effect <- NULL
      stratum <- lapply(DATAANALYSIS__covariates(), function(x){
        if(x[["stratify"]]){
          return(x[["var"]])
        } else {
          return(NULL)
        }
      })
      stratum <- do.call("c",stratum)

      covariates_effect <- lapply(DATAANALYSIS__covariates(), function(x){
        if(x[["stratify"]]){
          return(NULL)
        } else {
          return(x[["var"]])
        }
      })
      covariates_effect <- do.call("c",covariates_effect)
      
      
      if(length(stratum)>0){
        textBody <- sprintf("%s. Analysis was stratified by <b>[%s]</b>",
                            textBody,
                            paste(stratum,collapse=", ")
                            )
        
        if(length(covariates_effect)>0){
          textBody <- sprintf("%s and adjusted for differences in <b>[%s]</b>",
                              textBody,
                              paste(covariates_effect,collapse=", ")
          )
        }
        
        stratum.weight <- input$DATAANALYSIS__covariate_strata_method
        if(is.null(stratum.weight)){
           stratum.weight <- "unstratified"
        } 
        
        if(stratum.weight!="unstratified"){
          textBody <- sprintf("%s. Strata were pooled using %s weights",
                              textBody,
                              stratum.weight
          )
        }
  
      } else {
        if(length(covariates_effect)>0){
          textBody <- sprintf("%s. Analysis was adjusted for differences in <b>[%s]</b>",
                              textBody,
                              paste(covariates_effect,collapse=", ")
          )
        }
      }
      
      adjust.method <- input$DATAANALYSIS__surv_covariate_strata_method
      if(is.null(adjust.method)){
        adjust.method <- "unadjusted"
      }
      
      if(adjust.method != "unadjusted"){

        textBody <- sprintf("%s. Analysis adjusts for censoring using the %s method",
                            textBody,
                            adjust.method
        )

        covariates_censor <- NULL
        covariates_censor <- sapply(DATAANALYSIS__surv_covariates(), function(x){
          return(x[["var"]])
        })

        if(length(covariates_censor)>0){
          textBody <- sprintf("%s based on <b>[%s]</b>",
                              textBody,
                              paste(covariates_censor, collapse=", ")
          )
        }

      }
           
      # alpha <- isolate(input$DATAANALYSIS__alpha)
      # estimator_method <- isolate(input$DATAANALYSIS__pim_estimator)
      # max_iter <- isolate(input$DATAANALYSIS__pim_estimator_max_iter)
      
      if(methods_changed()){
        frontTags <- "<font color='#0000ff'>" # This really should be CSS
        endTags <- "</font>"
      } else {
        frontTags <- ""
        endTags <- ""         
      }
      
      out <- sprintf("%s %s %s",frontTags,textBody,endTags)
      
      HTML(out)
    })
    
    
    reactive_force_results_update <- reactiveVal(0)
    output$DATAANALYSIS__wins_output_ui <- renderUI({
      
      settings <- isolate(SYMBOLIC_LINK__settings())
      
      reactive_force_results_update()
      results <- DATAANALYSIS__results()
      # print(results)

      # Any reactives we might be interested in are defined here and should
      # be isolated

      alpha <- isolate(input$DATAANALYSIS__alpha)

      data_sheet <- isolate(SYMBOLIC_LINK__data_sheet())
      preferences <- isolate(SYMBOLIC_LINK__preference_export())
      arm <- isolate(input$DATAANALYSIS__arm)
 
      levels <- isolate(input$DATAANALYSIS__arm_active_selectInput)
      if(!is.null(levels)){
        levels <- c(
          levels,
          setdiff(levels(as.factor(data_sheet[,arm])),levels)
        )
      }

      stratum <- lapply(isolate(DATAANALYSIS__covariates()), function(x){
        if(x[["stratify"]]){
          return(x[["var"]])
        } else {
          return(NULL)
        }
      })
      stratum <- do.call("c",stratum)
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


      covariates_effect <- lapply(isolate(DATAANALYSIS__covariates()), function(x){
        if(x[["stratify"]]){
          return(NULL)
        } else {
          return(x[["var"]])
        }
      })
      covariates_effect <- do.call("c",covariates_effect)
      # print(covariates_effect)


      adjust.method <- isolate(input$DATAANALYSIS__surv_covariate_strata_method)
      if(is.null(adjust.method)) adjust.method <- "unadjusted"

      # print("Rendering UI output")

      out <- list()

      out[[length(out)+1]] <- fluidRow(column(width=8,
                                              tags$h4("Methods Reporting Template"),
                                              methods_writeup()
                                              ))
      
      if(!is.null(results$estimate)){

        out[[length(out)+1]] <- fluidRow(tags$h4("Results"))
        out[[length(out)+1]] <- fluidRow(tableOutput("DATAANALYSIS__wins_output"))

        # Report of results template following Howard's approach

        if(n_strata==0){
          resultTemplate <- sprintf("Of 100 people, %0.2f will have a better outcome if they are treated with %s while %0.2f will have a better outcome if they are treated with %s. %0.2f will have equivalent outcomes under both treatments (%s%s = %0.2f, %2.0f%%CI %0.2f - %0.2f).%s",
                                    100*results$estimate$win,
                                    levels[1],
                                    100* results$estimate$loss,
                                    levels[2],
                                    100* results$estimate$tie,
                                    ifelse(length(covariates_effect)>0,"Adj. ",""),
                                    results$estimate$outcome,
                                    results$estimate$estimate,
                                    100*(1-alpha),
                                    results$estimate$lower,
                                    results$estimate$upper,
                                    ifelse(adjust.method!="unadjusted","This effect size is adjusted for censoring.","")
          )

        } else {

          resultTemplate <- sprintf("%s%s = %0.2f, %2.0f%%CI %0.2f - %0.2f. %s",
                                    ifelse(length(covariates_effect)>0,"Adj. ",""),
                                    results$estimate$outcome,
                                    results$estimate$estimate,
                                    100*(1-alpha),
                                    results$estimate$lower,
                                    results$estimate$upper,
                                    ifelse(adjust.method!="unadjusted","This effect size is adjusted for censoring.","")
          )

        }


        out[[length(out)+1]] <- fluidRow(
          column(width=2,"Template Results for Reporting:"),
          column(width=8,
                 HTML("<font color='#0000ff'>",
                      resultTemplate,
                      "</font>")
                 )
        )
        
        if(settings$log_enabled){
          out[[length(out)+1]] <- fluidRow(
            tags$hr(),
            HTML(sprintf("Analysis log available at %s", results$logFile)),
            actionButton("DATAANALYSIS__results_log_open","Open analysis log file"),
            tags$hr()
          )
        }
        
        # TODO: PLOT/OUTPUT SETTINGS GO HERE
        

        if(length(covariates_effect)>0){
          out[[length(out)+1]] <- fluidRow("NOTE: Proportion of pairs in template shows raw estimates for wins/losses/ties,
                                           not adjusted values")
        }

      }
      
      
      if(input$SYMBOLIC_LINK__preferenceType=="list"){
        out[[length(out)+1]] <- fluidRow(plotOutput("DATAANALYSIS__wins_output_ranked_plot",height = "600px"))
      }
      

      if(!is.null(results$decomposed_estimate)){

        out[[length(out)+1]] <- fluidRow(hr(),tags$h4("Breakdown of outcome components"))

        if(length(covariates_effect)>0){
          out[[length(out)+1]] <- fluidRow("NOTE: Proportion of pairs (Left plot) shows raw estimates for wins/losses/ties")
        }

        out[[length(out)+1]] <- fluidRow(plotOutput("DATAANALYSIS__wins_output_decompsition_plot"))

        out[[length(out)+1]] <- bsCollapsePanel("Details",
                                  fluidRow(tableOutput("DATAANALYSIS__wins_output_decompsition"))
                                )
      }
      
      
      

      if(!is.null(DATAANALYSIS__results()$estimates_by_stratum)){

        out[[length(out)+1]] <- fluidRow(hr(),tags$h4("Results by Strata"))
        out[[length(out)+1]] <- fluidRow(tableOutput("DATAANALYSIS__wins_output_by_stratum"))
        out[[length(out)+1]] <- fluidRow(plotOutput("DATAANALYSIS__wins_output_by_stratum_decompsition_plot"))

      }


      out <- do.call("tagList",out)
      out
    })
    
    
    observeEvent(input$DATAANALYSIS__results_log_open,{
      results <- isolate(DATAANALYSIS__results())
      berryFunctions::openFile(results$logFile)
    })
    
    
    output$DATAANALYSIS__wins_output <- renderTable({
      DATAANALYSIS__results()$estimate
    })

    output$DATAANALYSIS__wins_output_decompsition <- renderTable({
      DATAANALYSIS__results()$decomposed_estimate %>%
        arrange(outcome)
    })

    
    ##### Rank-based elements ---------------------

    DATAANALYSIS__xtab <- reactiveVal({})
    
    output$DATAANALYSIS__wins_output_ranked_plot <- renderPlot({
      xtab <- DATAANALYSIS__xtab()
      
      if(sum(xtab)>0){
        
        effect.measure <- input$DATAANALYSIS__effect_measure
        
        ties_method <- ifelse(effect.measure %in% c("winOdds","netBenefit"),"split","drop")
        
        dimnames(xtab) <- list(arm=rownames(xtab),
                               rank=colnames(xtab))
        
        x1 <- xtab[1,]
        x0 <- xtab[2,]
        ranks <- as.numeric(colnames(xtab))
        
        plot_percentile <- pp_plot(x0,x1,ranks=ranks,
                labels=NULL,
                ties_method=ties_method,
                tie_display="horizontal",
                show_proportions = F,
                show_dichot_ci = F,
                show_cOR = F,
                show_labels = T
        )
        
        # Reverse the direction of xtab to get control on bottom, match with
        # the percentile-percentile plot.
        
        # This is hacky but it works
        
        plot_grotta <- grottaBar(xtab[nrow(xtab):1,],groupName = "arm",scoreName="rank",
                                             colorScheme="custom",printNumbers = "none") +
          guides(fill="none")+
          theme(legend.position = "right")+
          labs(fill="Rank")
        
        out <- plot_percentile + plot_grotta + plot_layout(guides = "collect")
        
      } else {
        out <- NULL
      }
      
      out
     
    })
    
    ##### Heirarchical elements ---------------------
    ###### DATAANALYSIS__wins_output_decompsition_plot-----
    output$DATAANALYSIS__wins_output_decompsition_plot <- renderPlot({

      df <- DATAANALYSIS__results()$decomposed_estimate
      df_overall <- DATAANALYSIS__results()$estimate


      # Need to know about stratification, covariate adjustments, etc
      # for rendering plot with appropriate tags

      stratum <- lapply(isolate(DATAANALYSIS__covariates()), function(x){
        if(x[["stratify"]]){
          return(x[["var"]])
        } else {
          return(NULL)
        }
      })
      stratum <- do.call("c",stratum)
      # print(stratum)

      covariates_effect <- lapply(isolate(DATAANALYSIS__covariates()), function(x){
        if(x[["stratify"]]){
          return(NULL)
        } else {
          return(x[["var"]])
        }
      })
      covariates_effect <- do.call("c",covariates_effect)
      # print(covariates_effect)


      # print(isolate(DATAANALYSIS__surv_covariates()))
      adjust.method <- isolate(input$DATAANALYSIS__surv_covariate_strata_method)
      if(is.null(adjust.method)) adjust.method <- "unadjusted"


      estimate_name <- unique(df$outcome)
      if(estimate_name=="Win Ratio"){
        neutral_point <- 1
        tie_handling <- "drop"
      } else if (estimate_name=="Win Odds") {
        neutral_point <- 1
        tie_handling <- "split"
      } else if (estimate_name=="Net Benefit"){
        neutral_point <- 0
        tie_handling <- "split"
      }


      # Add tags for if results are stratified, etc:

      # If covariate adjustment at any point, adjusted [effect]
      if(length(covariates_effect)>0){
        estimate_name <- sprintf("Adj. %s",estimate_name)
      } else {
        # If stratification no adjust : stratified [effect]
        if(length(stratum)>0){
          estimate_name <- sprintf("Stratified %s",estimate_name)
        }
      }

      # If censoring methods used: [method] [effect], adjusted for [covariate-dependent] censoring
      if(adjust.method!= "unadjusted"){
        estimate_name <- sprintf("%s, adjusted for censoring",estimate_name)
      }

      plot_data <- analysis_results_to_wr_df(df,df_overall)

      plot <- winRatioPlot(plot_data,tie_handling=tie_handling,neutral_point = neutral_point,estimate_name=estimate_name)

      plot$combined

    })
    
    ###### DATAANALYSIS__wins_output_by_stratum_decompsition_plot-----
    output$DATAANALYSIS__wins_output_by_stratum_decompsition_plot <- renderPlot({

      results <- DATAANALYSIS__results()$estimates_by_stratum

      # Need to know about stratification, covariate adjustments, etc
      # for rendering plot with appropriate tags

      covariates_effect <- lapply(isolate(DATAANALYSIS__covariates()), function(x){
        if(x[["stratify"]]){
          return(NULL)
        } else {
          return(x[["var"]])
        }
      })
      covariates_effect <- do.call("c",covariates_effect)
      # print(covariates_effect)


      # print(isolate(DATAANALYSIS__surv_covariates()))

      # TODO: Add an additional checks that we didn't set up on WINS then
      # swap to PIM
      adjust.method <- isolate(input$DATAANALYSIS__surv_covariate_strata_method)
      if(is.null(adjust.method)) adjust.method <- "unadjusted"


      plotList <- lapply(results, function(this_result){
        df <- this_result$decomposed_estimate
        df_overall <- this_result$estimate

        estimate_name <- unique(df$outcome)
        if(estimate_name=="Win Ratio"){
          neutral_point <- 1
          tie_handling <- "drop"
        } else if (estimate_name=="Win Odds") {
          neutral_point <- 1
          tie_handling <- "split"
        } else if (estimate_name=="Net Benefit"){
          neutral_point <- 0
          tie_handling <- "split"
        }

        # Add tags for if results are adjusted, etc:
        # Note: we don't need to tag stratification in estimate name

        # If covariate adjustment at any point, adjusted [effect]
        if(length(covariates_effect)>0){
          estimate_name <- sprintf("Adj. %s",estimate_name)
        }

        # If censoring methods used: [method] [effect], adjusted for [covariate-dependent] censoring
        if(adjust.method!="unadjusted"){
          estimate_name <- sprintf("%s, adjusted for censoring",estimate_name)
        }

        plot_data <- analysis_results_to_wr_df(df,df_overall)

        plot <- winRatioPlot(plot_data,tie_handling=tie_handling,neutral_point = neutral_point,estimate_name=estimate_name)

        plot$combined
      })

      # plotList <- lapply(1:4,function(i){
      #   ggplot(data.frame(x=rnorm(100),y=rnorm(100)),aes(x=x,y=y))+geom_point()+geom_smooth()+plot_annotation(title=i)
      #   })

      # TODO: This is dumb. We'd be better off constructing explicitly using
      # str2eval or something.

      plotList[["ncol"]] <- 1

      do.call("wrap_plots",plotList)

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
    
  })
)
