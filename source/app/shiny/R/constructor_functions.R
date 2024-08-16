
construct_tabset_ui <- function(path){
  
  directory <- dir(path)
  
  
  # If there's a metadata file at the current path, load it
  if("metadata.R" %in% directory){
    metadata <-  tryCatch({source(sprintf("%s/metadata.R",path))$value}, error=\(e){NULL})
    
    names(metadata) <- sapply(metadata,\(x){x$path})
  } else {
    metadata <- NULL
  }
  
  # Ignore anything that doesn't look like a module
  directory <- directory[grepl("^module-",directory)]
  
  # Reorder modules based on what's specified in metadata
  
  directory <- c(
    intersect(names(metadata),directory), #Ignore metadata for which there is no corresponding file
    setdiff(directory,names(metadata))  #Append any unlisted modules in alphabetical order at end
  )
  
  
  
  
  #Get next step on path before calling recursively
  # directory <- sprintf("%s/%s",path,directory)
  
  lapply(directory, function(f){
    
    title <- metadata[[f]]$label
    if(is.null(title)) title <- f
    
    f <- sprintf("%s/%s",path,f)
    
    if(fs::is_file(f)){
      
      module <- tryCatch({source(f)$value$ui_element})
      
      # Error checks should go here
      
      
    } else if(fs::is_dir(f)){
      module <- tabPanel(f,
                do.call("tabsetPanel",construct_tabset_ui(f))
                )
      
    } else {
      module <- NULL
    }
    
    tabPanel(title,module)
  })
}

