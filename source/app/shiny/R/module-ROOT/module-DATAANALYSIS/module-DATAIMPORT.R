# Demonstration of a module to be loaded into shiny
list(
  module_name = "DATAIMPORT",
  module_label = "Data Import",
  imports=NULL,
  ui_element = sidebarLayout(
           sidebarPanel(
             fileInput("DATAIMPORT__file", "Select a file"),
             uiOutput("DATAIMPORT__format")
           ),
           mainPanel(
             DT::dataTableOutput("DATAIMPORT__tbl")
           )
  ),
  server_element = substitute({
    
    DATAIMPORT__data_sheet <- reactive({
      if (is.null(input$DATAIMPORT__file)) {
        return("")
      }
      
      # actually read the file
      data <- read.csv(file = input$DATAIMPORT__file$datapath)
      
      # Any auto cleaning should go here
      
    })
    
    # output$DATAIMPORT_format <- renderUI({
    #  
    #   req(DATAIMPORT_input_file())
    #   data <- DATAIMPORT_input_file()
    #   
    # })
    
    output$DATAIMPORT__tbl <- DT::renderDT({

      req(DATAIMPORT__data_sheet())
      data <- DATAIMPORT__data_sheet()

      data
    })
    
  })
)
