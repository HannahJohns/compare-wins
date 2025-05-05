
source("R/load_packages.R")

source("R/misc_functions.R")

software_version <- c(major=0,
                      minor=5,
                      patch=3,
                      build=NULL)

module_list <- dir("R/module-ROOT/",recursive = T)
# Get things that have filenames that look like modules.
# There is probably a more sensible way of doing this,
# but this works.
module_list <- module_list[grepl("module-[[:alnum:][:space:]_-]+.R",module_list,)]

# last-minute substitution of variables here.
write("Loading server module expressions:", stderr())
module_server_expr <- sapply(module_list,\(f){
  
  write(sprintf("\t%s",f), stderr())
  
  this_module <- source(sprintf("R/module-ROOT/%s",f))$value
  server_element <- rlang::expr_text(this_module$server_element)

  # If we've specified any symbolic links/imports between modules, perform
  if(length(this_module$imports)>0){
    for(i in 1:length(this_module$imports)){
      server_element <- gsub(
                             unname(this_module$imports[i]),
                             names(this_module$imports)[i],
                             server_element,
                             fixed=TRUE
                             )
    }
  }
  server_element
})
write("Done!", stderr())

module_server_expr <- paste(module_server_expr,collapse="\n")
module_server_expr <- str2expression(module_server_expr)

# Convenience functions used across multiple modules

# Gets a list of reactive inputs based on names and an index
inputCollection <- function(input_names,input_index){
  c(t(outer(input_names,input_index,paste0)))
}


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  session$onSessionEnded(function() {
    cat("Session ended",file=stderr())
    stopApp()
  })
  
  # Insert module server code
  eval(module_server_expr)
  
  # Check for updates. This happens after modules (including settings) are loaded
  # so we know if we should bypass this.
  
  # See module-SETTINGS.R for definition of SETTINGS__settings()
  update_flagged <- check_update(software_version,isolate(SETTINGS__settings()))
  heartbeat(software_version,isolate(SETTINGS__settings()))
  
  observe({
    update_flagged
    
    if(update_flagged$needed){
      showModal(modalDialog(
        title=sprintf("Update available: %s",update_flagged$new_tag),
        markdown(update_flagged$message),
        tags$a(
          href=update_flagged$url,
          HTML(
            "<button style='color:white; background-color: DodgerBlue;'><i class='fa fa-download'></i> Download</button>"
          ),
          target="_blank"
        )
        
      ))
    }
  })
  
  output$software_version_display <- renderText(
    sprintf("v%s",paste(software_version,collapse="."))
  )
  
}
