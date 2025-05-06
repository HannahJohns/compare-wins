

cat("Library paths are:")
print(.libPaths())

# Set default CRAN mirror for if we need to install anything
chooseCRANmirror(ind=1)

if(!"shiny" %in% rownames(installed.packages())){

  # This command will fail if shiny is not installed. If so, install it now.
  install.packages("shiny", dependencies = T)
}

shiny::runApp('./app/shiny',launch.browser = TRUE)
