
library(shiny)
library(shinyBS)
library(DT)

source("R/constructor_functions.R")

# Demonstration of invoking separate UI elements from elsewhere
# NOTE: We can construct this computationally and also extract metadata, etc for a given module from here

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Generalised Pairwise Comparisons"),
  
  # UI is constructed recursively based on some root module
  do.call("tabsetPanel",construct_tabset_ui("R/module-ROOT")),
  
  fluidRow(hr()),
  fluidRow("Version 0.0.1.9006")
)
