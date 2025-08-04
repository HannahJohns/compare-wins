# Demonstration of a module to be loaded into shiny
list(
  module_name = "VOTEPOOL",
  module_label = "Consensus Voting",
  imports=NULL,
  ui_element = sidebarLayout(
           sidebarPanel(
             fileInput("VOTEPOOL__file", "Select a file"),
             uiOutput("VOTEPOOL__columnSelect")
           ),
           mainPanel(
             DT::dataTableOutput("VOTEPOOL__tbl"),
             fluidRow(actionButton("VOTEPOOL__go",label = "Synthesise votes")),
             DT::dataTableOutput("VOTEPOOL__tbl_results")
           )
  ),
  server_element = substitute({
    
    VOTEPOOL__data_sheet <- reactive({
      if (is.null(input$VOTEPOOL__file)) {
        return("")
      }
      
      # actually read the file
      data <- read.csv(file = input$VOTEPOOL__file$datapath)
      
      # Any auto cleaning should go here
      
    })
    
    output$VOTEPOOL__columnSelect <- renderUI({
      
      req(VOTEPOOL__data_sheet())
      data <- VOTEPOOL__data_sheet()
      
  
      out <- lapply(0:ncol(data), function(i){
        
        if(i==0) return(shiny::tags$h4("Column types"))
        
        shiny::radioButtons(sprintf("VOTEPOOL__columnSelect_%d",i),
                            label = sprintf(colnames(data)[i]),
                            choices = c("Option"="option","Name/ID of voter"="voteName","Ignore"="ignore"),
                            selected = "option", inline =T
                            )
      })
      
      out <- c(out)
      
      out <- do.call("tagList",out)

      out
      
    })
    
    
    output$VOTEPOOL__tbl <- DT::renderDT({

      req(VOTEPOOL__data_sheet())
      data <- VOTEPOOL__data_sheet()

      colTypes <- sapply(1:ncol(data),function(i){
        input[[sprintf(sprintf("VOTEPOOL__columnSelect_%d",i))]]
      })
      names(colTypes) <- colnames(data)

      if(sum(colTypes=="voteName")>1) stop("Only one column can be set as the name/id of a voter")

      if(sum(colTypes=="voteName")==1){
          rownames(data) <- data[,names(colTypes)[colTypes=="voteName"]]
      }
      
      if(sum(colTypes=="option") < 2) stop("At least two options are needed for voting to occur")

      data <- data[,names(colTypes)[colTypes=="option"]]
      
      data
    })
    
    
    VOTEPOOL__results <- reactive({
      
      input$VOTEPOOL__go
      
      req(isolate(VOTEPOOL__data_sheet()))
      data <- isolate(VOTEPOOL__data_sheet())
      
      colTypes <- sapply(1:ncol(data),function(i){
        isolate(input[[sprintf(sprintf("VOTEPOOL__columnSelect_%d",i))]])
      })
      names(colTypes) <- colnames(data)
      
      if(sum(colTypes=="voteName")>1) return(data.table::data.table()) #stop("Only one column can be set as the name/id of a voter")
      
      if(sum(colTypes=="voteName")==1){
        rownames(data) <- data[,names(colTypes)[colTypes=="voteName"]]
      }
      
      if(sum(colTypes=="option") < 2) return(data.table::data.table()) # stop("At least two options are needed for voting to occur")
      
      data <- data[,names(colTypes)[colTypes=="option"]]
      
      # Check here that all columns are numeric
      if(!all(apply(data,2,is.numeric))) stop("All votes should be numeric - check for letters and spaces in data sheet.")
      
      # If we add other methods for synthesising votes, this this will be a case switch statement 
      out <- condorcet(data)
      
      as.data.frame(out)
    })
    
    output$VOTEPOOL__tbl_results <- DT::renderDT({
      
      req(VOTEPOOL__results())
      data <- VOTEPOOL__results()
      
      data
      
    })
    
  })
)



