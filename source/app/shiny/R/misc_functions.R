
# TODO: This is not a scalable way of having external functions for modules but
# it will work for now

################################################################################
# 
# Debug inputs based on captured results
# 
# hashList <- c(
#   all = "837a52a7d897c1413ccad206d238e34f",
#   indiv_1 = "2835ffb294f682cf0cdd1fc09c1fef87",
#   indiv_2 = "b08781921ab059c048c6c2fb7c806387",
#   cum_1 = "2835ffb294f682cf0cdd1fc09c1fef87",
#   cum_2 = "837a52a7d897c1413ccad206d238e34f"
# )
# # 
# tmp <- readRDS(sprintf("source/app/shiny/tmp_%s.RDS",
# #                        hashList[2]
# #                        ))
# data <- tmp$data
# args <- tmp
# # 
# outcomes <- tmp$outcomes
# arm <- tmp$arm
# levels <- tmp$levels
# stratum <- tmp$stratum
# covariates <- tmp$covariates_effect
# alpha <- tmp$alpha


# method <- "pim"


################################################################################

# Wrapper function for interfacing with underlying stats software.
# Output should be a data frame with the following criteria:
# First column is method name
# After that currently nothing but this is stupid and will cause things to
# break once we start adding e.g. plots, so it needs standardised.

run_analysis <- function(args, method){
  
  print(sprintf("Running %s with args",method))
  print(args)
  
  
  # Some sort of default value
  out <- data.frame(outcome=NA,
                    estimate=NA,
                    lower=NA,
                    upper=NA,
                    p=NA,
                    win=NA,
                    loss=NA,
                    tie=NA)

  
  # TODO: add sanity checks and warnings for unused controls etc
  
  # TODO: Also add interpretation of common error messages rather than feeding
  # back raw messages to the user
  
  didError <- simsalapar::tryCatch.W.E({
    
    if(length(args$levels)!=2) stop("There should be exactly 2 treatment groups")

    if(method=="wins"){
      
      out <- wins_wrapper(data=args$data,
                          outcomes=args$outcomes,
                          arm=args$arm,
                          levels=args$levels,
                          stratum=args$stratum,
                          covariates=args$covariates_censor,
                          method = args$method,
                          stratum.weight = args$stratum.weight,
                          pvalue = "two-sided",
                          alpha=args$alpha
      )
      
    } else if (method=="pim"){
      
      if(args$estimator_method=="estimator.BB"){
        controlList <- list(maxit = args$max_iter)
      } else if(args$estimator_method=="estimator.glm") {
        controlList <- list(maxit = args$max_iter)
      } else {
        controlList <- NULL
      }

      out <- pim_wrapper(data=args$data,
                         outcomes=args$outcomes,
                         arm=args$arm,
                         levels=rev(args$levels), # levels is c(treat,control)
                         stratum=args$stratum,
                         covariates=args$covariates_effect,
                         stratum.weight = args$stratum.weight,
                         alpha=args$alpha,
                         estim = args$estimator_method,
                         controlList = controlList
                         )
      
    } else if(method=="debug"){
      
      filehash <- digest::digest(args)
      saveRDS(
        args,
        file=sprintf("tmp_%s.RDS",filehash)
      )
      
      out$hash <- filehash
      
      print("Results saved to disk")
      
    }
    
    NULL
  })
  
  print(list(out=out,
             warning=didError$warning,
             error=didError$value
  ))
  
  
  return(
    list(out=out,
         warning=didError$warning,
         error=didError$value
         )
  )
  
}


pim_wrapper <-  function(data, outcomes, arm, levels,
                         preference_method="heirarchical", #This absolutely shouldn't have default arguments in production, but for now it's fine
                         stratum=NULL,
                         covariates=NULL,
                         stratum.weight="ivw",
                         alpha=0.05,
                         estim,
                         controlList = NULL
                         ){
  
  # This should ultimately be arguments the user can make
  link <- "logit"
  model  <- "difference"

  # Get data into the correct structure
  
  # Reasonably easy to get the RHS of the regression model working
  formatted_data <- data.frame(
    factor(data[,arm],levels=levels)
  )
  colnames(formatted_data) <- arm
  # Need to convert to factor with specified active arm
  
  
  
  if(length(covariates)>0){
    for(i in 1:length(covariates)){
      formatted_data[,covariates[i]] <- data[,covariates[i]]
    }
  }
  
  
  if(length(stratum)>0 & stratum.weight != "unstratified"){
    if(length(stratum)==1){
      stratum <- data[,stratum]          
    } else {
      stratum <- do.call("paste", lapply(stratum, function(x){data[,x]}))   
    }
  } else {
    stratum.weight <- "unstratified"
  }
  
  
  # LHS needs some massaging depending on preference definition
  if(preference_method=="heirarchical"){
    
    # First check that this is safe to do
    
    if(prod(sapply(outcomes, function(x){x$type})=="numeric")!=1){
      stop("Survival outcomes not supported")
    }

    
    if(prod(sapply(outcomes, function(x){x$tau})==0)!=1){
      stop("Non-zero thresholds for clinical difference not supported")
    }
    
    # Take preferences and convert it into a numeric rank
    
    # This is a bit cursed but it works.

    # First, get how each individual outcome should be ranked
    outcome_rank <- lapply(outcomes,function(x){
      new_outcome_var <- data[,x$var]
      # Cast as factor so we can manipulate it
      new_outcome_var <- factor(new_outcome_var)
      
      if(x$direction == "<"){
        new_outcome_var <- factor(new_outcome_var,levels=rev(levels(new_outcome_var)))
      }
      as.numeric(new_outcome_var)
    })
    
    # Then get this in a data frame that we can reference later. This provides
    # a target for us to attach ranks to once they're calculated
    outcome_rank_frame <- do.call("data.frame",outcome_rank)
    colnames(outcome_rank_frame) <-  sapply(outcomes,function(x){x$var})

    # Calculate ranks based unique values each outcome rank. High values are good.    
    outcome_rank_value <- unique(outcome_rank_frame[do.call("order",outcome_rank),, drop=FALSE])
    outcome_rank_value$`__ranked_result` <- 1:nrow(outcome_rank_value)
    
    # Join this to the data frame from before to get which values should be correct
    outcome_rank_frame <- left_join(
      outcome_rank_frame,
      outcome_rank_value
    )
    
    
    # TODO: This will break if RESERVED_ranked_result is a name of a covariate
    formatted_data$`RESERVED__ranked_result` <- outcome_rank_frame$`__ranked_result`
   
    
  } else {
    stop("NON-HEIRARCHICAL METHODS NOT IMPLEMENTED YET")
  }
  
  # # Validation of this for debugging
  # outcome_vars <- do.call("data.frame",
  #                         lapply(outcomes,function(x){
  #                           new_outcome_var <- data[,x$var]
  #                           new_outcome_var
  #                         })
  # )
  # colnames(outcome_vars) <- sapply(outcomes,function(x){x$var})
  # cbind(outcome_vars,outcome_rank_frame$`__ranked_result`) %>% View()
  
  # Join this with the formatted data
 
  
  
  
  # Run PIM
  if(link=="logit"){
    formula <- sprintf("%s~%s",
                       "RESERVED__ranked_result",
                       paste(c(arm,covariates),collapse="+")
    )
    formula <- as.formula(formula)
  } else {
    
    # If we want risk difference etc we need different link functions here.
    # This may require adding an intercept term to centre at no difference
    stop("link functions other than logit not supported")
    
  }
  
  if(stratum.weight=="ivw"){

    fit <- by(formatted_data,stratum, function(tmpdf){
      pim(formula,
          data=tmpdf,
          link = link,
          model = model,
          estim=estim)
    })
    
    fit <- do.call("rbind",lapply(fit,function(x){
      data.frame(
        estimate=coef(x)[paste0(arm,levels[2])],
        var=diag(vcov(x))[paste0(arm,levels[2])]
        )
    }))

    coefs <- sum(fit[,"estimate"]/fit[,"var"])/sum(1/fit[,"var"])
    var <- 1/sum(1/fit[,"var"])
    
    conf_interval <- coefs + c(lower=-1,upper=1)*sqrt(var)*qnorm(1-alpha/2)
    pval <- 2*pnorm(abs(coefs/sqrt(var)),lower.tail = F)
    
    # Transform results to be correct scale
    if(link=="logit"){
      coefs <- exp(coefs)
      conf_interval <- exp(conf_interval)
    }
    
    out <- data.frame(outcome="Win Odds",
                      estimate=coefs,
                      lower=conf_interval[1],
                      upper=conf_interval[2],
                      p=pval,
                      win=NA,
                      loss=NA,
                      tie=NA)
    
  } else if (stratum.weight=="unstratified"){
    
    
    fit <- pim(formula,
               data=formatted_data,
               link = link,
               model = model,
               estim=estim,
               control=controlList
    )
    
    coefs <- coef(fit)[paste0(arm,levels[2])]
    conf_interval <- confint(fit,level = 1-alpha)[paste0(arm,levels[2]),]
    pval <- summary(fit)@pr[paste0(arm,levels[2])]
   
    # Transform results to be correct scale
    if(link=="logit"){
      coefs <- exp(coefs)
      conf_interval <- exp(conf_interval)
    }
    
    # Get win/losses/ties. This can be done by getting sign(diff(treatment, control))
    wlt_count <- table(c(sign(outer(
      formatted_data[formatted_data[,arm]==levels[2],"RESERVED__ranked_result"],
      formatted_data[formatted_data[,arm]==levels[1],"RESERVED__ranked_result"],
      "-"
    ))))
    names(wlt_count) <- c("-1"="loss","0"="tie","1"="win")[names(wlt_count)]
    wlt_count <- wlt_count/sum(wlt_count)


    out <- data.frame(outcome="Win Odds",
                      estimate=coefs,
                      lower=conf_interval[1],
                      upper=conf_interval[2],
                      p=pval,
                      win=wlt_count["win"],
                      loss=wlt_count["loss"],
                      tie=wlt_count["tie"]
                      )
  } else {
    stop("Unrecognised stratification method")
  }
  
  rownames(out) <- NULL
  out
  
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
          
          warning(sprintf(
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
  
  estimates
  
}

