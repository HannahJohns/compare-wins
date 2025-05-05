if(!"shiny" %in% rownames(installed.packages())){
  # This command will fail if shiny is not installed. If so, install it now.
  install.packages("shiny",repos = "https://cloud.r-project.org/")
}

shiny::runApp('./app/shiny',launch.browser = TRUE)