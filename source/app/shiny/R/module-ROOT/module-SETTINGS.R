# Demonstration of a module to be loaded into shiny
list(
  module_name = "SETTINGS",
  module_label = "Settings and Privacy",
  imports=NULL,
  ui_element = fluidPage(
    "TODO: THESE NEED TO BE RENDERUI ELEMENTS. DEFAULT BEHAVIOURS DEPEND ON SAVED SETTINGS.",
    fluidRow(
      column(width=4,
             tags$h5("Create analysis logs"),
             "If enabled, "
             ),
      column(width=1,
             checkboxInput("SETTINGS__log_analysis", "", value = TRUE, width = NULL)
      )
    ),
    fluidRow(
      column(width=4,
             tags$h5("Check for updates at startup"),
             "If enabled,... "
             ),
      column(width=4,
             checkboxInput("SETTINGS__check_for_updates", "", value = TRUE, width = NULL),
      )
    ), 
    fluidRow(
      column(width=4,
             tags$h5("Send anonymous usage ping at startup"),
             "If enabled, COMPARE WINS will send an anonymous usage ping at startup, containing the time since this software was last opened. 
             This information is used to estimate the number of users for COMPARE WINS for the purposes of supporting grant. No personal information or analysis data is transmitted."
             ),
      column(width=4,
             checkboxInput("SETTINGS__monitor_usage", "", value = TRUE, width = NULL)
      )
      
    )
  ),
  server_element = substitute({
    
    # 
    print("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
    print(dir())
    
    
  }) # End server
)
