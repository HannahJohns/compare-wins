# This module requires dynamicly adding/deleting/etc UI elements.
# This requires some workarounds which are described
# https://stackoverflow.com/questions/40631788/shiny-observe-triggered-by-dynamicaly-generated-inputs
# https://stackoverflow.com/questions/40168801/r-shiny-last-clicked-button-id

list(
  module_name = "",
  module_label = "Define Preferences",
  imports = c("DATAIMPORT__data_sheet"="SYMBOLIC_LINK__data_sheet"),
  ui_element = fluidPage(
    tags$script("$(document).on('click', '.PREFDEF__ui_updater', function () {
                              Shiny.onInputChange('PREFDEF__uiUpdateId',this.id);
                              Shiny.onInputChange('PREFDEF__uiUpdateId_update',Math.random());
                             });"),
    tags$script("$(document).on('change', '.PREFDEF__preference_value select', function () {
                              Shiny.onInputChange('PREFDEF__preferenceValueId',this.id);
                              Shiny.onInputChange('PREFDEF__preferenceValueId_update',Math.random());
                             });"),
    fluidRow(
      radioButtons("PREFDEF__preferenceType", "Preferences are based on:",
                   choices = c("Heirarchy" = "heirarchical") #, "List" = "list")
      )
    ),
    fluidRow(
        uiOutput("PREFDEF__components")
    )
  ),
  
  server_element = substitute({
    
    # Any reference to reactive objects from other sections is handled
    # by last-minute substitution at the top level based on whatever is
    # specified in the imports field above.
    
    # Observer for forcing a UI update 
    PREFDEF__force_UI_update <- reactiveVal(0)

    # Preference list stores rules for determining "better off"
    # NOTE: We store each method separately because having it as a dynamic list
    # was causing problems. This means that if we want to add new methods,
    # they need to be implemented as individual reactive objects.
    
    PREFDEF__preferenceHeirarchy <- reactiveVal(list())
    PREFDEF__preferenceList <- PREFDEF__preference_sheet <- reactive({
                                if (is.null(input$PREFDEF__preference_file)) {
                                  return("")
                                }
                                
                                # actually read the file
                                data <- read.csv(file = input$PREFDEF__preference_file$datapath)
                                
                                # Any auto cleaning should go here
                                
                              })
    
    
    # Instead, we provide a bundled version of all preference objects for other
    # modules to access
    PREFDEF__preference_export <- reactive(
      list(
        heirarchical=PREFDEF__preferenceHeirarchy(),
        list=PREFDEF__preferenceList()
      )
    )
    
    
    
                                           
    # If an action was taken that causes structural change to 
    # Preference list (i.e. added/removed/re-ordered details)
    # Make these changes as neccesary 
    observe({
      
      req(SYMBOLIC_LINK__data_sheet())
      data_sheet <- SYMBOLIC_LINK__data_sheet()
      
      input$PREFDEF__uiUpdateId_update
      
      if (!is.null(input$PREFDEF__uiUpdateId)) {
       
        # Get preference list to modify
        preferenceList_tmp <- isolate(PREFDEF__preferenceHeirarchy())
         
        # # If we've swapped the preference type, wipe the preference object
        # if(!(input$PREFDEF__preferenceType %in% class(preferenceList_tmp))){
        #   preferenceList_tmp <- list()
        #   class(preferenceList_tmp) <- c(class(preferenceList_tmp),input$PREFDEF__preferenceType)
        # }
        
        
        selectedId <- isolate(input$PREFDEF__uiUpdateId)
        
        #Update preference list structure (e.g. shuffle, delete, etc) and then signal to update UI elements
        if(selectedId == "PREFDEF__add_component"){
          
            # Add new preference component
            preferenceList_tmp[[length(preferenceList_tmp)+1]] <- list(
              type="numeric",
              var=colnames(data_sheet)[1],
              indicator=colnames(data_sheet)[1],
              direction=">"
            )
            PREFDEF__preferenceHeirarchy(preferenceList_tmp)
           
        } else {
          
          # Pressed button is in the dynamic rows.
          # Action taken depends on the button type
          
          actIdLabels <- c(
            up="PREFDEF__component_up_",
            down="PREFDEF__component_down_",
            delete="PREFDEF__component_delete_"
          )    
          
          actType <- which(sapply(actIdLabels,grepl,x=selectedId))
          actType <- names(actIdLabels)[actType]
          if(length(actType)!=1){
            stop("Something is wrong")
          }
          
          change_number <- as.numeric(gsub(actIdLabels[actType],"",selectedId))
          
          if(is.na(change_number)) stop("unexpeced non-numeric value")
          
          #TODO: Write this
          
          if(actType == "up" & change_number > 1){
            new_order <- 1:length(preferenceList_tmp)
            
            new_order[change_number] <- change_number-1
            new_order[change_number-1] <- change_number
            
            preferenceList_tmp <- preferenceList_tmp[new_order]
          } else if (actType == "down" & change_number < length(preferenceList_tmp)){
            new_order <- 1:length(preferenceList_tmp)
            
            new_order[change_number] <- change_number+1
            new_order[change_number+1] <- change_number
            
            preferenceList_tmp <- preferenceList_tmp[new_order]
            
          } else if(actType == "delete"){
            preferenceList_tmp <- preferenceList_tmp[-change_number]
          }
          
          # redundant
          # class(preferenceList_tmp) <- c("list","heirarchical")
          
          PREFDEF__preferenceHeirarchy(preferenceList_tmp)
          
        }
        
        # Signal to UI to update
        tmp <- isolate(PREFDEF__force_UI_update())
        tmp <- tmp+1
        tmp <- tmp %% 2
        PREFDEF__force_UI_update(tmp)
      }
    })
    
    # Observer for changing PREFDEF__preferenceHeirarchy in response to UI input
    observe({
      input$PREFDEF__preferenceValueId_update
      
      if (!is.null(input$PREFDEF__preferenceValueId)) {
        
        changed_id <- isolate(input$PREFDEF__preferenceValueId)
        changed_value <- isolate(input[[changed_id]])

        # Parse out the type of change
        varIdLabels <- c(
          type="PREFDEF__component_type_",
          var="PREFDEF__component_var_",
          indicator="PREFDEF__component_censor_",
          direction="PREFDEF__component_direction_"
        )

        changeType <- which(sapply(varIdLabels,grepl,x=changed_id))
        changeType <- names(varIdLabels)[changeType]
        if(length(changeType)!=1){
          stop("Something is wrong")
        }

        change_number <- as.numeric(gsub(varIdLabels[changeType],"",changed_id))

        if(is.na(change_number)) stop("unexpeced non-numeric value")

        preferenceList_tmp <- isolate(PREFDEF__preferenceHeirarchy())
        preferenceList_tmp[[change_number]][[changeType]] <- changed_value
        PREFDEF__preferenceHeirarchy(preferenceList_tmp)
      }
      
    })

    # UI output for preference list 
    output$PREFDEF__components <- renderUI({
      
      # Trigger UI change
      req(SYMBOLIC_LINK__data_sheet())
      
      # Trigger update of this section only when signaled to do so.
      # There is probably a better way to do this, but it works
      PREFDEF__force_UI_update()
      
      data_sheet <- isolate(SYMBOLIC_LINK__data_sheet())
      
      new_preferences <- isolate(PREFDEF__preferenceHeirarchy())
     
      if(input$PREFDEF__preferenceType == "heirarchical"){
        
        add_component_button <-  fluidRow(
          actionButton("PREFDEF__add_component",
                       "Add Preference Component",
                       class="PREFDEF__ui_updater")
        )  
        
        if(length(new_preferences)==0){
          out <- tagList(add_component_button)
        } else {
          
          out <- tagList(
            do.call("tagList", lapply(1:length(new_preferences), function(i){
              fluidRow(
              flowLayout(
                       div(class="PREFDEF__preference_value",
                       selectInput(inputId = sprintf("PREFDEF__component_type_%d",i),
                                   label = "This outcome is:",
                                   choices = c("Numeric"="numeric","Ordinal"="ordinal","Survival"="surv"),
                                   selected=new_preferences[[i]][["type"]]
                       )),
                       div(class="PREFDEF__preference_value",
                       selectInput(inputId = sprintf("PREFDEF__component_var_%d",i),
                                   label = "Outcome variable is:",
                                   choices = colnames(data_sheet),
                                   selected=new_preferences[[i]][["var"]]
                                   
                       )),
                       # Conditional Panel for specifying second variable
                       conditionalPanel(sprintf("input.PREFDEF__component_type_%d=='surv'",i),
                                        div(class="PREFDEF__preference_value",
                                        selectInput(inputId = sprintf("PREFDEF__component_censor_%d",i),
                                                    label = "Censor indicator is:",
                                                    choices = colnames(data_sheet),
                                                    selected=new_preferences[[i]][["indicator"]]
                                        )
                                        )
                       ),
                       div(class="PREFDEF__preference_value",
                       # This should really be radiobuttons but I couldn't work out how to get
                       # javascript to behave. Making it dropdown was the easiest solution
                       selectInput(inputId = sprintf("PREFDEF__component_direction_%d",i),
                                    label = "Preference direction:",
                                    choices = c("Lower is better" = "<", "Higher is better" = ">"),
                                    selected=new_preferences[[i]][["direction"]]
                       )),
                       div(
                       actionButton(inputId = sprintf("PREFDEF__component_up_%d",i),
                                    label = "Up",
                                    class="PREFDEF__ui_updater"
                       ),
                       actionButton(inputId = sprintf("PREFDEF__component_down_%d",i),
                                    label = "Down",
                                    class="PREFDEF__ui_updater"
                       ),
                       actionButton(inputId = sprintf("PREFDEF__component_delete_%d",i),
                                    label = "Delete",
                                    class="PREFDEF__ui_updater"
                       )
                       )
              ),
              tags$br(),
              hr()
              )
            })),
            fluidRow(add_component_button)
          )
        }
      } else {
        out <- sidebarLayout(
          sidebarPanel(
            fileInput("PREFDEF__preference_file","Select a file") 
          ),
          mainPanel(
            DT::dataTableOutput("PREFDEF__tbl")
          )
        )
      }

      out
    })
    
    
    output$PREFDEF__tbl <- DT::renderDT({
      
      req(PREFDEF__preference_sheet())
      data <- PREFDEF__preference_sheet()
      
      data
    })
    
    
  })
)