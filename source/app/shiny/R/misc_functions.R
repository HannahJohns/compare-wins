
# TODO: This is not a scalable way of having external functions for modules but
# it will work for now

################################################################################
# source("R/generate_data.R")
# df$age_strt <- ifelse(df$age < median(df$age),0,1)
# 
# my_outcomes <- list(
#   list(var="deathTime",indicator="deathStatus",type="surv",direction=">"),
#   list(var="hospTime",indicator="hospStatus",type="surv",direction=">"),
#   list(var="rankin",type="numeric",direction="<")
# )
# 
# 
# 
# x <- wins_wrapper(df,outcomes = my_outcomes,
#              arm = "trt",
#              stratum = "age_strt",
#              covariates = "age",
#              levels = 1:0,
#              method = "unadjusted",
#              stratum.weight = "MH-type",
#              pvalue = "two-sided"
#              )


# Wrapper function for interfacing with underlying stats software.
# Output should be a data frame with the following criteria:
# First column is method name
# After that currently nothing but this is stupid and will cause things to
# break once we start adding e.g. plots, so it needs standardised.

run_analysis <- function(args, method){
  
  # Some sort of default value
  out <- data.frame(outcome=NA,estimate=NA)
  
  if(method=="wins"){
    out <- wins_wrapper(data=args$data,
                        outcomes=args$outcomes,
                        arm=args$arm,
                        levels=args$levels,
                        stratum=args$stratum,
                        covariates=args$covariates,
                        method = args$method,
                        stratum.weight = args$stratum.weight,
                        pvalue = "two-sided",
                        alpha=args$alpha
                      )
    
  } else if (method=="pim"){
    out <- pim_wrapper(data=args$data,
                       outcomes=args$outcomes,
                       arm=args$arm,
                       levels=args$levels,
                       stratum=args$stratum,
                       covariates=args$covariates,
                       pvalue = "two-sided",
                       alpha=args$alpha)
  }
  
  out
  
}




# pim::pim
pim_wrapper <-  function(data, outcomes, arm, levels,
                         preference_method="heirarchical", #This absolutely shouldn't have default arguments in production, but for now it's fine
                         stratum=NULL,
                         covariates=NULL,
                         alpha=0.05,
                         ){
  
  # Add save results stuff here
  
  
  # This should ultimately be arguments
  link <- "logit"
  model  <- "difference"
  
  #TODO: Give options for this stuff here 
  estim <- "estimator.nleqslv"
  
  ?estimators
  
  # Get data into the correct structure
  
  # Reasonably easy to get the RHS of the regression model working
  formatted_data <- data[,c(arm,sapply(covariates,function(x){x$var}))]
  formatted_data$arm <- factor(data[,arm])
  
  # LHS needs some massaging depending on preference definition
  if(preference_method=="heirarchical"){
    
    # Need to take preferences and convert it into a numeric rank
    
    # First check that this is safe to do
    
    # Get grid of possible outcome combinations and rank them
    
    # Join this with the formatted data
    
  } else {
    stop("NON-HEIRARCHICAL METHODS NOT IMPLEMENTED YET")
  }
  
  browser()
  stop("PIM isn't implemented yet!")
  
}



# WINS::win.stat
wins_wrapper <- function(data, outcomes, arm, levels,
                         preference_method="heirarchical", 
                         stratum=NULL,
                         covariates=NULL,
                         method = "unadjusted",
                         stratum.weight = "MH-type",
                         pvalue = "two-sided",
                         alpha=0.05
){
  
  # print("ATTEMPTING TO RUN WINS_WRAPPER")
  # # This needs deleted before pull request to merge with main
  # trace <- list(data = data,
  #              outcomes=outcomes,
  #              arm=arm,
  #              levels=levels,
  #              stratum = stratum,
  #              stratum.weight = stratum.weight,
  #              covariates = covariates,
  #              method = method,
  #              pvalue = "two-sided"
  #              # alpha = 0.05
  # )
  # saveRDS(trace,file="DEBUG_TRACE_INPUTS.RDS")
  
  # print(
  #   list(outcomes=outcomes,
  #        arm=arm,
  #        levels=levels,
  #        stratum = stratum,
  #        stratum.weight = stratum.weight,
  #        covariates = covariates,
  #        method = method,
  #        pvalue = "two-sided"
  #        # alpha = 0.05
  #   )
  # )


  # First, we need to convert the data sheet into the correct format
  
  formatted_data <- data.frame(id=1:nrow(data))
  formatted_data$arm <- factor(data[,arm])
  
  if(length(stratum)>0){
    if(length(stratum)==1){
      formatted_data$stratum <- data[,stratum]          
    } else {
      formatted_data$stratum <- do.call("paste", lapply(stratum, function(x){data[,x]}))   
    }
  } else {
    stratum.weight <- "unstratified"
  }
  
  if(preference_method=="heirarchical"){
    
    ep_type <- {}
    np_direction <- {}
    tau <- {}
    for(i in 1:length(outcomes)){
      ep_type[i] <- outcomes[[i]]$type
      tau[i] <- outcomes[[i]]$tau
      np_direction[i] <- c(`>`="larger",`<`="smaller")[outcomes[[i]]$direction]
      
      formatted_data[,sprintf("Y_%d",i)] <- data[,outcomes[[i]]$var]
      if(ep_type[i] %in% c("surv","tte")){
        ep_type[i] <- "tte"
        formatted_data[,sprintf("Delta_%d",i)]  <- data[,outcomes[[i]]$indicator]
        
        if(length(
          setdiff(
            unique(formatted_data[,sprintf("Delta_%d",i)]),
            0:1
          )
        ) >0 ){
          
          # should be a pop-up but can't see it at present
          write(sprintf(
            "%s includes values other than 1 and 0. All values not 1 assumed censored.",
            outcomes[[i]]$var
          ), stderr())
          
          formatted_data[,sprintf("Delta_%d",i)] <- ifelse(formatted_data[,sprintf("Delta_%d",i)]==1,1,0)        
        } 
        
        
      } 
    }
    
  } else {
    stop("NON-HEIRARCHICAL METHODS NOT IMPLEMENTED YET")
  }
  
  
  # Only support covariates at baseline for the time being. 
  
  # NOTE: This needs added
  if(length(covariates)==0){
    Z_t_trt <- Z_t_con <- NULL
  } else {
    # At present only support covariates at baseline
    Z_t_con <- data.frame(id=1:nrow(formatted_data), time=0) 
    for(i in 1:length(covariates)){
      Z_t_con[,covariates[i]] <- data[,covariates[i]]
    }
    Z_t_trt <-Z_t_con
    
    # Z_t_trt/con must contain only controls and treatments respectively
    Z_t_trt <- Z_t_trt[which(formatted_data$arm==levels[1]),]
    Z_t_con <- Z_t_con[which(formatted_data$arm==levels[2]),]
  }
  
  
  didError <- tryCatch({
    
    out <- WINS::win.stat(data = formatted_data,
                          ep_type = ep_type,
                          priority = 1:length(outcomes),
                          arm.name = levels,
                          tau = tau,
                          np_direction = np_direction,
                          Z_t_trt = Z_t_trt,
                          Z_t_con = Z_t_con, 
                          method = method,
                          stratum.weight = stratum.weight,
                          pvalue = pvalue,
                          alpha=alpha,
                          summary.print=FALSE
    )
    
    FALSE
  }, error=function(e){e})

  if("error" %in% class(didError)){
    
    showModal(modalDialog(
      title="Error",
      sprintf("%s",didError$message),
      easyClose=TRUE,
      footer=NULL
    ))
    
    estimates <- data.frame(outcome=NA,
                            estimate=NA,
                            lower=NA,
                            upper=NA,
                            p=NA,
                            win=NA,
                            loss=NA,
                            tie=NA)
    
  } else {
    
    # Format results into a table to report back
    
    estimates <- do.call("rbind",lapply(names(out$Win_statistic),function(i){
      cbind(outcome=i,
            as.data.frame(matrix(out$Win_statistic[[i]],nrow=1))
      )
    }))
    
    colnames(estimates)[-1] <- c("estimate","lower","upper")
    
    estimates$p <- c(out$p_value)
    
    # Win/loss/tie proportions only make sense to report if unstratified
    if(length(stratum)==0){
      estimates$win <- out$Win_prop$P_trt
      estimates$loss <- out$Win_prop$P_con    
      estimates$tie <- 1 - (out$Win_prop$P_trt + out$Win_prop$P_con)
      
    } else {
      estimates$win <- NA
      estimates$loss <- NA    
      estimates$tie <- NA
    }
    
  }
  
  estimates
  
}

