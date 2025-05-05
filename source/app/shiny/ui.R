
source("R/load_packages.R")

source("R/constructor_functions.R")

# Demonstration of invoking separate UI elements from elsewhere
# NOTE: We can construct this computationally and also extract metadata, etc for a given module from here

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  withMathJax(),
  tags$div(HTML("<script type='text/x-mathjax-config' >
    MathJax.Hub.Config({
    tex2jax: {inlineMath: [['$','$']]}
    });
    </script >
    ")),
  tags$head(tags$style(
    HTML(".shiny-notification {
              height: 100px;
              width: 800px;
              position:fixed;
              top: calc(50% - 50px);;
              left: calc(50% - 400px);;
            }
           "
    )
  )),

  # Application title
  titlePanel(tags$img(width=300,src="compare-wins-logo.svg"),windowTitle = "COMPARE WINS"),
  
  # UI is constructed recursively based on some root module
  do.call("tabsetPanel",construct_tabset_ui("R/module-ROOT")),
  
  fluidRow(hr()),
  fluidRow(column(width=1,textOutput("software_version_display"))),
  fluidRow(hr())
)
