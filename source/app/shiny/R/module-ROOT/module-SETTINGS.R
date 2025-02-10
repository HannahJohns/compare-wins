# Demonstration of a module to be loaded into shiny
list(
  module_name = "SETTINGS",
  module_label = "Settings and Privacy",
  imports=NULL,
  ui_element = fluidPage(
    navlistPanel(
      widths = c(2,10),
      tabPanel(
        title="General Settings",
        fluidPage(
          fluidRow(
            column(width=6,
                   tags$h3("Create analysis logs"),
                   "If enabled, running analysis will produce a text file containing logs of all settings used in analysis and corresponding results."
            ),
            column(width=1,
                   uiOutput("SETTINGS__ui_log_analysis")
            )
          ),
          fluidRow(
            column(width=6,
                   tags$h3("Save logs to:"),
                   textOutput("SETTINGS__ui_log_path")
            ),
            column(width=1,
                   actionButton("SETTINGS__log_dir", "Choose Log Directoy")
            )
          ),
          fluidRow(
            column(width=6,
                   tags$h3("Check for updates at startup"),
                   "If enabled, COMPARE WINS will notify you of feature improvements and bug fixes."
            ),
            column(width=1,
                   uiOutput("SETTINGS__ui_update_check")
            )
          ), 
          fluidRow(
            column(width=6,
                   tags$h3("Send anonymous usage ping at startup"),
             "If enabled, COMPARE WINS will send an anonymous usage ping at startup, containing the time since this software was last opened on this device. 
             This information is used to estimate the number of users for COMPARE WINS. This data is used to justify allocating resources for maintenance and 
             to support grant applications that would expand the features of COMPARE WINS. No personal information or analysis data is transmitted."
            ),
            column(width=1,
                   uiOutput("SETTINGS__ui_heartbeat")
            )
            
          )
        )
      )
    )
  ),
  server_element = substitute({


    SETTINGS__settings_file <- "settings.RDS"
    SETTINGS__settings <- reactiveVal({
      
      if(!(SETTINGS__settings_file %in% dir())){
        # Generate a default settings file if none exists
        new_settings <- list(
          log_enabled = T,
          log_dir = paste0(getwd(),"/logs"),
          update_check = T,
          heartbeat = T
        )
        saveRDS(new_settings,SETTINGS__settings_file)
      }
      
      readRDS(SETTINGS__settings_file)
    })
   
    observe({
      current_settings <- SETTINGS__settings()
      print("Current Settings are:")
      print(current_settings)
      
      saveRDS(current_settings,SETTINGS__settings_file)
    })
    
    output$SETTINGS__ui_log_analysis <- renderUI({
       current_settings <- isolate(SETTINGS__settings())
       checkboxInput("SETTINGS__log_analysis", "", value = current_settings$log_enabled, width = NULL)      
    })
    observeEvent(input$SETTINGS__log_analysis,{
      current_settings <- SETTINGS__settings()
      current_settings$log_enabled <- input$SETTINGS__log_analysis
      SETTINGS__settings(current_settings)
    })
    
    output$SETTINGS__ui_log_path <- renderText({
      current_settings <- SETTINGS__settings()
      current_settings$log_dir
    })
    observeEvent(input$SETTINGS__log_dir,{
      
      dir <- utils::choose.dir("")
      if(!is.na(dir)){
        
        dir <- gsub("\\\\","/",dir)
        
        current_settings <- isolate(SETTINGS__settings())
        current_settings$log_dir <- dir
        SETTINGS__settings(current_settings)
        
      }
      
    })
    
    
    output$SETTINGS__ui_update_check <- renderUI({
      current_settings <- isolate(SETTINGS__settings())
      checkboxInput("SETTINGS__update_check", "", value = current_settings$update_check, width = NULL)      
    })
    observeEvent(input$SETTINGS__update_check,{
      
      current_settings <- isolate(SETTINGS__settings())
      current_settings$update_check <- input$SETTINGS__update_check
      SETTINGS__settings(current_settings)
    })
    
    
    output$SETTINGS__ui_heartbeat <- renderUI({
      current_settings <- isolate(SETTINGS__settings())
      checkboxInput("SETTINGS__heartbeat", "", value = current_settings$heartbeat, width = NULL)      
    })
    observeEvent(input$SETTINGS__heartbeat,{
      
      current_settings <- isolate(SETTINGS__settings())
      current_settings$heartbeat <- input$SETTINGS__heartbeat
      SETTINGS__settings(current_settings)
    })
    
    
    
    
    
    
    
  }) # End server
)
