
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



# Wrapper function for interfacing with WINS::win.stat
# Gives the following:
# Estimate: Overall results. If unstratified, also gives us explicit win/loss/tie proportions
# Decomposed Estimate: Decomposition of the preference heirarchy with results individually and cumulative.
                       # Only calculated if unstratified.
# Estimates_by_stratum: Results by stratum including explicit win/loss/tie proportion and decomposition

wins_wrapper <- function(data, outcomes, arm, levels,
                         stratum=NULL,
                         covariates=NULL,
                         method = "unadjusted",
                         stratum.weight = "MH-type",
                         pvalue = "two-sided",
                         decompose = TRUE 
){

  # First, we need to convert the data sheet into the correct format
  
  formatted_data <- data.frame(id=1:nrow(data))
  formatted_data$arm <- factor(data[,arm])
  
  if(length(stratum)>0){
    if(length(stratum)==1){
      formatted_data$stratum <- data[,stratum]          
    } else {
      formatted_data$stratum <- do.call("paste", lapply(stratum, function(x){data[,x]}))   
    }
    
    #TODO: This should be user-input
    stratum.weight <- "MH-type"
    
  } else {
    stratum.weight <- "unstratified"
  }
  
  ep_type <- {}
  np_direction <- {}
  for(i in 1:length(outcomes)){
    ep_type[i] <- outcomes[[i]]$type
    tau <- 0 # TODO: Need to add this
    np_direction[i] <- c(`>`="larger",`<`="smaller")[outcomes[[i]]$direction]
    
    formatted_data[,sprintf("Y_%d",i)] <- data[,outcomes[[i]]$var]
    if(ep_type[i] %in% c("surv","tte")){
      ep_type[i] <- "tte"
      formatted_data[,sprintf("Delta_%d",i)]  <- data[,outcomes[[i]]$indicator]
    } 
  }
  
  # Only support covariates at baseline for the time being. 
  
  # NOTE: This needs added
  if(length(covariates)==0){
    Z_t_trt <- Z_t_con <- NULL
  } else {
    # At present only support covariates at baseline
    Z_t_con <- data.frame(id=1:nrow(data), time=0) 
    for(i in 1:length(covariates)){
      Z_t_con[,covariates[i]] <- data[,covariates[i]]
    }
    Z_t_trt <-Z_t_con
  }
  
  out <- WINS::win.stat(data = formatted_data,
                        ep_type = ep_type,
                        priority = 1:length(outcomes),
                        arm.name = levels,
                        tau = tau,
                        np_direction = np_direction,
                        Z_t_trt = Z_t_trt,
                        Z_t_con = Z_t_con, 
                        alpha = 0.05,
                        method = method,
                        stratum.weight = stratum.weight,
                        pvalue = pvalue,
                        summary.print=FALSE
  )
  
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
  
  # Recursively re-run to get results by stratum
  
  # Get results by stratum
  if(length(stratum)>0){
    
    estimates_by_stratum <- by(data = data,INDICES = formatted_data$stratum, function(tmpdf){
      wins_wrapper(
        tmpdf,
        outcomes=outcomes,
        arm=arm,
        levels=levels,
        stratum=NULL,
        covariates=covariates,
        method = method,
        stratum.weight = "unstratified",
        pvalue = pvalue,
        decompose=decompose
      )
    })
  } else {
    estimates_by_stratum <- NULL
  }
  
  # Get decomposition of results by outcome facets
  if(length(outcomes)>1 & decompose){
    
    estimates_by_outcome <- lapply(1:length(outcomes),function(i){
      
      out <- wins_wrapper(data=data,
                   outcomes=outcomes[i],
                   arm=arm,
                   levels=levels,
                   stratum=stratum,
                   covariates=covariates,
                   method = method,
                   stratum.weight = stratum.weight,
                   pvalue = pvalue,
                   decompose=FALSE
      )
      
      out <- out$estimate
      
      colnames(out) <- paste(colnames(out))
      
      out <- cbind(level=i,out)
      out
      
    })
    
    estimates_by_cumulative_outcome <- lapply(1:length(outcomes),function(i){
      out <- wins_wrapper(data=data,
                   outcomes=outcomes[1:i],
                   arm=arm,
                   levels=levels,
                   stratum=stratum,
                   covariates=covariates,
                   method = method,
                   stratum.weight = stratum.weight,
                   pvalue = pvalue,
                   decompose=FALSE
      )
      
      out <- out$estimate
      
      
      colnames(out)[-1] <- paste(colnames(out)[-1],"cumulative",sep="_")
      out <- cbind(level=i,out)
      out
      
    })
    
    decomposed_estimate <- left_join(
      do.call("rbind",estimates_by_outcome),
      do.call("rbind",estimates_by_cumulative_outcome)
    ) %>% arrange(outcome,level)
    
  } else {
    decomposed_estimate <- NULL
  }
  
  list(
    estimate = estimates %>% arrange(outcome),
    decomposed_estimate = decomposed_estimate,
    estimates_by_stratum = estimates_by_stratum
  )
  
}

