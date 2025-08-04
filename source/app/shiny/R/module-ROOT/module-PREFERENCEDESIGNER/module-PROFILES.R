# Demonstration of a module to be loaded into shiny
list(
  module_name = "PROFILES",
  module_label = "Patient Profiles",
  imports=c("VOTEPOOL__results"="SYMBOLIC_LINK__ranks_processed"),
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
      fluidRow(tags$hr(),
               uiOutput("PROFILES__link_ui"),
               column(width=2,actionButton("PROFILES__go",label = "Go!"))
               ),
      fluidRow(uiOutput("PROFILES__door_select_ui")),
      plotOutput("PROFILES__results")
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
          ),
          column(width=3,
                 selectInput(sprintf("PROFILES__data_processing_%d_direction",i),
                             label = "Preference direction ",
                             choices = c("Lower is better"="<", "Higher is better" = ">"),
                             selected = ">"
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
    
    
    

    
    output$PROFILES__link_ui <- renderUI({
      
      
      req(PROFILES__data_sheet_profiles_processed())
      data_profiles <- PROFILES__data_sheet_profiles_processed()
      
      req(SYMBOLIC_LINK__ranks_processed())
      data_ranks <- SYMBOLIC_LINK__ranks_processed()
      
      
      tagList(
        
        column(width=3,
          selectInput("PROFILES__link_id_profiles",
                      label = "Select column containing Profile ID from above",
                      choices = colnames(data_profiles))          
        ),
        column(width=3,
          selectInput("PROFILES__link_id_rank",
                      label = "Select column containing ranks from Consensus Voting tab",
                      choices = colnames(data_ranks))       
        )
      )
      
      
    })
    
    
    
    
    PROFILES_candidateMethods <- reactive({

      input$PROFILES__go
      
      data_profiles <- isolate(PROFILES__data_sheet_profiles_processed())
      
      data_raw <- isolate(PROFILES__data_sheet_profiles_raw())
      data_profiles_direction <- sapply(1:ncol(data_raw),function(i){
        isolate(input[[sprintf("PROFILES__data_processing_%d_direction",i)]])        
      })
      names(data_profiles_direction) <- colnames(data_raw)
      
      data_profiles_id_col <- isolate(input$PROFILES__link_id_profiles)
      data_ranks <- isolate(SYMBOLIC_LINK__ranks_processed())
      data_ranks_rank_col <- isolate(input$PROFILES__link_id_rank)
      
      out <- NULL
      
      if(!is.null(data_profiles) & !is.null(data_ranks)){
        
        
        print(data_ranks)
        
        # First, extract rank information.
        
        profile_id <- unlist(data_profiles[,data_profiles_id_col])
        
        # Ranks will have been checks when they were loaded into the sheet.
        profile_id <- make.names(profile_id)
        profile_ranks <- as.numeric(sapply(profile_id, function(i){
          i <<- i
          unlist(data_ranks[,data_ranks_rank_col])[data_ranks$Option==i]
        }))
        
        print(profile_id)
        print(profile_ranks)
        
        
        direction <- data_profiles_direction[setdiff(colnames(data_profiles), data_profiles_id_col)]
        
        direction
        
        candidateDOORS <- getDOORList( data_profiles[,names(direction)], direction)
        
        out <- lapply(1:length(candidateDOORS), function(i){NULL})
        
        startTime <- Sys.time()
        total_run <- length(out)
        
        withProgress(message="Evaluating Candidate DOOR",{
          for(i in 1:length(out)){
            
            currTime <- Sys.time()
            
            elapsedTime <- as.numeric(difftime(currTime,startTime, units = "mins"))
            remainingTime <- elapsedTime * (total_run/(i) - 1 )   
            unitsTime <- "minutes"
            
            if(remainingTime > 60){
              remainingTime <- remainingTime/60
              unitsTime <- "hours"
            }
            
            detail <- sprintf("%d/%d %0.2f %s remaining",
                              i,
                              total_run,
                              remainingTime,
                              "mins"
            )
            
            incProgress(1/total_run,detail= detail)
            
            x <- candidateDOORS[[i]]
            
            transformed_df <- build_transformed_df(x,data_profiles[,names(direction)])
            
            K <- construct_K(x,transformed_df)
            
            out[[i]] <- list(DOOR=x,eval = gpct(K,profile_ranks))
            
          }
        })
        
        # Get the best candidates at the front
        out <- out[order(-sapply(out, function(x){x$eval[1]}))]
        
        out
      
      }
    })
    
    
    output$PROFILES__door_select_ui <- renderUI({
      
      req(PROFILES_candidateMethods())
      candidates <- PROFILES_candidateMethods()
      
      fluidRow(column(width=3, numericInput("PROFILES__door_select",label = "Display Candidate DOOR",value = 1,min = 1, max=length(candidates))))
    })
    
    
    
    output$PROFILES__door_selected <- DT::renderDataTable({
      
      
      req(PROFILES_candidateMethods())
      candidates <- PROFILES_candidateMethods()
      
      # which(sapply(candidates, function(x){
      #   length(x$DOOR$ordering)
      # }) ==5)      
      # 
      # thisCandidate <- candidates[[55]]
      # 
      # 
      # i <- names(sort(thisCandidate$DOOR$ordering))[1]
      # 
      # 
      # 
      # lapply(names(sort(thisCandidate$DOOR$ordering)), function(i){
      #   
      #   theseVars <- names(
      #     thisCandidate$DOOR$combine[thisCandidate$DOOR$combine == i]
      #   )
      #   
      #   if(grepl("Surv",i)){
      #     combineMethod  <- "First event out of"
      #   } else if(grepl("Surv",i)){
      #     
      #     
      #     
      #   } else {
      #     ""
      #   }
      #   
      #     
      #   data.frame(
      #     variables = paste(
      #       ,
      #       collapse = ", "
      #     )
      #   )
      #   
      # })
      
     as.data.frame(diag(3)) 
    })

    
    
    output$PROFILES__results  <- renderPlot({
      
      req(PROFILES_candidateMethods())
      candidates <- PROFILES_candidateMethods()
      
      saveRDS(candidates,
              file="TRACE.RDS")
      
      # candidates <- readRDS("source/app/shiny/TRACE.RDS")
      
      tmp <- as.data.frame(do.call("rbind",lapply(candidates, function(x){x$eval})))
      tmp$step <- 1:nrow(tmp)
      tmp$lower = tmp$effect - tmp$se * qnorm(p = 1-0.025)
      tmp$upper = tmp$effect + tmp$se * qnorm(p = 1-0.025)
      
      tmp$effect <- exp(tmp$effect)
      tmp$lower <- exp(tmp$lower)
      tmp$upper <- exp(tmp$upper)
      
      
      ggplot2::ggplot(tmp,ggplot2::aes(x=step,y=effect,ymin=lower,ymax=upper)) +
        ggplot2::geom_ribbon(alpha=0.6)+
        ggplot2::geom_line()+
        ggplot2::theme_bw()+
        ggplot2::labs(x="Candidate DOOR",y="Agreement")+
        ggplot2::scale_y_log10()
      
    })
    
    
    
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



