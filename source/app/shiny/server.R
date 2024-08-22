

library(shiny)
library(DT)

# Demonstration of taking code from outside of server and injecting it into server
# on startup.

# This will let us build things in a modular fashion


library(rlang)
# a <- substitute({x <- 1})
# b <- substitute({x <- x+1})
# 
# # Combine expressions from each server 
# eval(
#   str2expression(paste(lapply(list(a,b),\(x){expr_text(x)}),collapse="\n"))
#   )

source("R/misc_functions.R")

module_list <- dir("R/module-ROOT/",recursive = T)
module_list <- module_list[grepl("/module-",module_list)]

write("Loading server module expressions:", stderr())
module_server_expr <- sapply(module_list,\(f){
  
  write(sprintf("\t%s",f), stderr())
  
  this_module <- source(sprintf("R/module-ROOT/%s",f))$value
  server_element <- rlang::expr_text(this_module$server_element)

  # If we've specified any symbolic links/imports between modules, perform
  # last-minute substitution of variables here.
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

# print(module_server_expr)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  eval(module_server_expr)
}
