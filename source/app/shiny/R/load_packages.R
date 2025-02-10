
# Master list of packages to load/install
package_list <- c(
  "MASS",
  "jsonlite",
  "curl",
  
  "shiny",
  "shinyBS",
  "DT",
  
  "simsalapar",
  "pim",
  "WINS",
  
  "patchwork",
  "rankinPlot",
  
  "rlang",
  "tidyverse",
  "httr"
)


# If we've already loaded packages we don't need to check them, bypass this
already_loaded <- c(
  names(sessionInfo()$basePkgs),
  names(sessionInfo()$otherPkgs),
  names(sessionInfo()$loadedOnly)
)

package_list <- setdiff(package_list,already_loaded)

# If there's any packages unavailable, make them available

to_install <- setdiff(package_list, rownames(installed.packages()) )

if(length(to_install)>0){
  
    pb = winProgressBar(
      title = sprintf('Installing Packages'),
      label = 'Initializing ...'
    )

    for( i in to_install){
      setWinProgressBar(pb, i/(length(to_install)+1), label = sprintf('Installing package: %s', i))
      install.packages(i)
    }
    
    close(pb)
    
}

if(length(package_list)>0){
  
  for( i in package_list){
    eval(
      bquote(library(.(i)))
    )
  }
  
}

