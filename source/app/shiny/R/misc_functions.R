
# TODO: This is not a scalable way of having external functions for modules but
# it will work for now

# Wrapper function for interfacing with WINS::win.stat

# We can probably move this to an external R file rather than have it sit within the server definition
wins_wrapper <- function(data, outcomes, arm,
                         stratum=NULL,
                         covariates=NULL,
                         method = "unadjusted",
                         stratum.weight = "unstratified",
                         pvalue = "two-sided"
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
  
  levels <- isolate(input$DATAANALYSIS__arm_active_selectInput)
  levels <- c(
    levels,
    setdiff(levels(formatted_data$arm),levels)
  )
  
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
                        summary.print = FALSE 
  )
  
  
  
  # Format results into a table to report back
  
  estimates <- do.call("rbind",lapply(names(out$Win_statistic),function(i){
    cbind(outcome=i,
          as.data.frame(matrix(out$Win_statistic[[i]],nrow=1))
    )
  }))
  
  colnames(estimates)[-1] <- c("Estimate","Lower CI","Upper CI")
  
  estimates$p.value <- c(out$p_value)
  
  # # Get endpoint wins/losses/ties/etc and format neatly for reporting as well
  # 
  # winTab <- out$summary_ep[
  # names(out$summary_ep)[grepl("^Trt_",names(out$summary_ep))]
  # ]
  # winTab <- do.call("rbind",winTab)
  # winTab$endpoint <- gsub("Trt_Endpoint","",rownames(winTab))
  # winTab <- winTab[,c("Stratum","endpoint","Proportion")]
  # colnames(winTab)[3] <- "Proportion_Win"
  # 
  # lossTab <- out$summary_ep[    
  # names(out$summary_ep)[grepl("^Con_",names(out$summary_ep))]
  # ]
  # lossTab <- do.call("rbind",lossTab)
  # lossTab$endpoint <- gsub("Con_Endpoint","",rownames(lossTab))
  # lossTab <- lossTab[,c("Stratum","endpoint","Proportion")]
  # colnames(lossTab)[3] <- "Proportion_Loss"
  # 
  # dplyr::left_join(winTab,lossTab)
  
  estimates
}




  