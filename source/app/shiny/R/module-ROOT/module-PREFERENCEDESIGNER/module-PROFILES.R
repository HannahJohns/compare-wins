# Demonstration of a module to be loaded into shiny
list(
  module_name = "PROFILES",
  module_label = "Patient Profiles",
  imports=NULL,
  ui_element = fluidPage(
    sidebarPanel(
      fileInput("PROFILES__file", "Select file containing patient profiles")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Raw data",DT::dataTableOutput("PROFILES__tbl_profiles_raw")),
        tabPanel("Data Processing",uiOutput("PROFILES__data_processing")),
        tabPanel("Processed Data",DT::dataTableOutput("PROFILES__tbl_profiles_processed"))
      ),
      fluidRow(uiOutput("PROFILES__link_ui")),
      fluidRow(actionButton("EXMAPLERANK__go",label = "Action Button")),
      DT::dataTableOutput("PROFILES__results")
    )
  ),
  server_element = substitute({
     
    PROFILES__data_sheet_profiles_raw <- reactive({
      if (is.null(input$PROFILES__file)) {
        return("")
      }

      # actually read the file
      data <- read.csv(file = input$PROFILES__file$datapath)

      # Any auto cleaning should go here

    })
    
    output$PROFILES__tbl_profiles_raw <- DT::renderDT({

      req(PROFILES__data_sheet_profiles_raw())
      data <- PROFILES__data_sheet_profiles_raw()

      data
    })
   
    
    output$PROFILES__data_processing <- renderUI({
      
      req(PROFILES__data_sheet_profiles_raw())
      data_raw <- PROFILES__data_sheet_profiles_raw()
      
      # At minimum, we need to get a list of each column type
      # if survival, give dropdown for censoring column
      
      out <- lapply(1:ncol(data_raw),function(i){

        fluidRow(
          column(width=3,
                 selectInput(sprintf("PROFILES__data_processing_%d",i),
                              label = colnames(data_raw)[i],
                              choices = c("character","logical","factor","integer","numeric","survival","ignore"),
                              selected = class(data_raw[,i])
          )
          ),
          column(width=3,
            conditionalPanel(
            condition=sprintf("input.PROFILES__data_processing_%d == 'survival'",i),
            selectInput(sprintf("PROFILES__data_processing_%d_censor",i),
                        label = "Event (Censoring variable)",
                        choices = colnames(data_raw),
                        selected = colnames(data_raw)[i]
            )
          )
          )
        )
        
      })
      
      do.call("tagList",out)
    })
    
    PROFILES__data_sheet_profiles_processed <- reactive({
      
      req(PROFILES__data_sheet_profiles_raw())
      
      data_raw <- PROFILES__data_sheet_profiles_raw()
      
      
      # There are so many things that can go wrong here.
      # catch errors and report them when the results of this get called
      
      out <- tryCatch({
        
        tmp_out <- {}
        
        for(i in 1:ncol(data_raw)){
          
          thisClass <- input[[sprintf("PROFILES__data_processing_%d",i)]]
          thisClass_raw <- class(data_raw[,i])
          
          # If we skip over processing, thisClass isn't initialised and we
          # get a null value. This corrects to a default behaviour
          if(is.null(thisClass)) thisClass <- thisClass_raw
          
          if(thisClass == "ignore") next
          
          if(thisClass == thisClass_raw){
            
            if(is.null(tmp_out)){
              tmp_out <- data_raw[,i, drop=F]
            } else {
              tmp_out <- cbind(
                tmp_out, 
                data_raw[,i, drop=F]
              )
            }
            
          } else {
            thisCol <- data_raw[,i, drop=F]       
            
            # This is not a great solution but it'll have to do
            
            if(thisClass == "character"){
              
              thisCol[,1] <- as.character(thisCol[,1])
              
            } else if(thisClass == "logical"){
              
            print("THIS NEEDS WRITTEN!")
              
            } else if(thisClass == "factor"){

              print("THIS NEEDS WRITTEN!")
              
              
            } else if(thisClass == "integer"){
              
              print("THIS NEEDS WRITTEN!")
              
            } else if(thisClass == "numeric"){
              
              print("THIS NEEDS WRITTEN!")
         
              
            } else if(thisClass == "survival"){
     
              censorVar <- input[[sprintf("PROFILES__data_processing_%d_censor",i)]]         

              print("THIS NEEDS ERROR CHECKS")
              
              thisCol[,1] <- survival::Surv(thisCol[,1],data_raw[,censorVar])

            } else {
              stop("Unrecognised type")
            }
              
            if(is.null(tmp_out)){
              tmp_out <- thisCol
            } else {
              tmp_out <- cbind(
                tmp_out, 
                thisCol
              )
            }
            
          } # End if need to do anything
          
        } # End for columns
        
        tmp_out
        
      }, error=function(e){e})
      
      out
      
    })
    
    output$PROFILES__tbl_profiles_processed <- DT::renderDT({
      
      req(PROFILES__data_sheet_profiles_processed())
      data <- PROFILES__data_sheet_profiles_processed()
      
      if("error" %in% class(data)) stop(data$message)

      # Rendering survival data is breaking things.
      # To fix, just cheat and render it as a factor before rendering
      # This is very stupid
      
      survVars <- colnames(data)[
        sapply(1:ncol(data),function(i){
          "Surv" %in% class(data[,i])
        })        
      ]

      for(i in survVars){
        thisVar <- data[,i]
        
        thisVar_as_char <- as.character(thisVar)
        
        thisVar <- factor(
          thisVar_as_char,
          levels=unique(thisVar_as_char[
            order(thisVar[,1],-thisVar[,2])
          ])
        )
        
        data[,i] <- thisVar 
        
      }
      
      data
    })
    
    
    # GPCT is exactly what we need here, which means I need to get that
    # uploaded to CRAN before we can push this.
    
    # output$PROFILES__columnSelect <- renderUI({
    #   
    #   req(PROFILES__data_sheet())
    #   data <- PROFILES__data_sheet()
    #   
    #   
    #   out <- lapply(0:ncol(data), function(i){
    #     
    #     if(i==0) return(shiny::tags$h4("Column types"))
    #     
    #     shiny::radioButtons(sprintf("PROFILES__columnSelect_%d",i),
    #                         label = sprintf(colnames(data)[i]),
    #                         choices = c("Option"="option","Name/ID of voter"="voteName","Ignore"="ignore"),
    #                         selected = "option", inline =T
    #     )
    #   })
    #   
    #   out <- c(out)
    #   
    #   out <- do.call("tagList",out)
    #   
    #   out
    #   
    # })
     
   
    # 
    # output$PROFILES__results <- DT::renderDT({
    #   
    #   input$PROFILES__go
    #   
    #   req(isolate(PROFILES__data_sheet()))
    #   data <- isolate(PROFILES__data_sheet())
    #   
    #   colTypes <- sapply(1:ncol(data),function(i){
    #     isolate(input[[sprintf(sprintf("PROFILES__columnSelect_%d",i))]])
    #   })
    #   names(colTypes) <- colnames(data)
    #   
    #   if(sum(colTypes=="voteName")>1) return(data.table::data.table()) #stop("Only one column can be set as the name/id of a voter")
    #   
    #   if(sum(colTypes=="voteName")==1){
    #     rownames(data) <- data[,names(colTypes)[colTypes=="voteName"]]
    #   }
    #   
    #   if(sum(colTypes=="option") < 2) return(data.table::data.table()) # stop("At least two options are needed for voting to occur")
    #   
    #   data <- data[,names(colTypes)[colTypes=="option"]]
    #   
    #   
    #   # Check here that all columns are numeric
    #   if(!all(apply(data,2,is.numeric))) stop("All votes should be numeric - check for letters and spaces in data sheet.")
    #   
    #   # If we add other methods for synthesising votes, this this will be a case switch statement 
    #   out <- condorcet(data)
    #   
    #   out
    # })
    
  })
)



