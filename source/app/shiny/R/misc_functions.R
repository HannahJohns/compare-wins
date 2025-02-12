
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


check_update <- function(software_version, settings){
  
  if(!settings$update_check){
    print("Update check skipped")
    return(list(needed = F,new_tag=NULL,message=NULL,url=NULL))
  } 
  
  
  print("Checking for updates")
  startTime <- Sys.time()
  
    
  update_flagged <- FALSE
  
  # Make API call to github to check for updates on launch
  api_results <- tryCatch({
    httr::content(httr::GET("https://api.github.com/repos/HannahJohns/compare-wins/releases/latest"))
  }, error=function(e){list(status="ERROR")})
  
  
  if(is.null(api_results$status)){
    
    release_version_number <- api_results$tag_name
    
    release_version_number
    release_version_number <- strsplit(gsub("v","",api_results$tag_name),"\\.")[[1]]
    release_version_number <- as.numeric(release_version_number)
    
    
    # If we have a build number, note this
    while(length(release_version_number) < length(software_version)) release_version_number <- c(release_version_number,0)
    
    for(i in 1:length(software_version)){
      if(release_version_number[i] < software_version[i]){
        break    
      } else if(release_version_number[i] > software_version[i]){
        update_flagged <- TRUE
        break
      }
    }
    
    # We're behind in version number. Check that we have a download link, etc
    if(update_flagged){
      
      release_notes <- api_results$body
      
      download_url <- sapply(api_results$assets,function(x){
        
        if(x$content_type != "application/x-msdownload") return(NULL)
        
        x$browser_download_url
      })
      
      if(length(download_url) != 1) update_flagged <- FALSE
    } 
    
  }
  
  if(update_flagged){
    update_check <- list(needed = T,new_tag=api_results$tag_name,message=release_notes,url=download_url)
  } else {
    update_check <- list(needed = F,new_tag=NULL,message=NULL,url=NULL)
  }
  
  
  endTime <- Sys.time()
  print(sprintf("Done, took %s", difftime(endTime,startTime)))
  
  update_check
  
}


heartbeat <- function(software_version,settings){
  
  if(settings$heartbeat){
    
    print("Sending usage ping")
    startTime <- Sys.time()
    
    prev_time <- tryCatch({
      tmp <- readRDS("heartbeat_time.RDS")
      if(!is.POSIXct(tmp)) stop("heartbeat is not POSIXct")
      tmp
    }, error=function(e){NA})
    
    
    time <- Sys.time()
    tryCatch({
      saveRDS(time,"heartbeat_time.RDS")
    })
    
    if(!is.na(prev_time) & !is.na(time)){
      timeDiff <- as.numeric(difftime(time,prev_time, units = "hours"))      
    }
    
    # This is not good. It really should use a proper REST API but building one
    # is currently beyond my skillset.
    
    usage_data <- jsonlite::toJSON(
      list(version=paste(software_version,collapse="."), timeDiff=timeDiff)
    )
    url <- sprintf("https://htjohns.com/pings/ping.php?app=comparewins&message=%s",usage_data)
    
    tryCatch({
      con <- curl::curl(url)
      readLines(con, warn = FALSE)  
      close(con)
    })
    
    endTime <- Sys.time()
    print(sprintf("Done, took %s", difftime(endTime,startTime)))

  } else {
    print("Ping disabled, skipped")
  }
  
}



# Infrastructure functions ##################################



# Analysis Functions ##################################

# Wrapper function for interfacing with underlying stats software.
# Output should be a data frame with the following criteria:
# First column is method name
# After that currently nothing but this is stupid and will cause things to
# break once we start adding e.g. plots, so it needs standardised.

run_analysis <- function(args, method, effect.measure){

    
  # print(sprintf("Running %s with args",method))
  # print(args[setdiff])
  
  
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
      
      effect_raw <- c("winRatio"="Win_Ratio",
                      "winOdds"="Win_Odds" ,
                      "netBenefit"="Net_Benefit"
      )
      
      effect_label <- c("winRatio"="Win Ratio",
                        "winOdds"="Win Odds" ,
                        "netBenefit"="Net Benefit"
                       )
      
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
      
      out %>% 
        filter(outcome == effect_raw[effect.measure]) %>%
        mutate(outcome = effect_label[effect.measure]) -> out
      
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
    
    # NOTE: This shouldn't be called when using list-based methods as we convert
    # from list-based to heirarchical at the top level
    
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
    
    # NOTE: Win Odds is adjusted for covariates, but individual win/loss/tie proportions are not.
    # This needs flagged somewhere
    
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


# Power Analysis ----------------------------------------------------


# These sample sizes are in total. Divide by 2 to get per-group sample size
yu_power_generic <- function(p_win,p_loss,p_tie, k=0.5, alpha, power){
  
  logwr = log(p_win/p_loss)
  
  s2 = 4*(1+p_tie)/(3*k*(1-k)*(1-p_tie))
  
  N = s2 * (qnorm(1-alpha/2) + qnorm(power))^2 / (logwr^2)
  
  return(N)
}


#' Converts effect size and assumed proportion of ties
#' to win/loss/tie proportions
get_wlt <- function(effect,ES,ties){
  
  # Just uniroot() this.
  # It's cheap to run, handles error checking for us,
  # and extendable to other effect sizes as they're proposed/developed
  
  prop_wl <- 1-ties
  
  wins <- tryCatch({uniroot(function(wins){
    
    losses = prop_wl-wins
    
    this_ES <- switch (effect,
                       "winRatio" = wins/losses,
                       "winOdds" = (wins+0.5*ties)/(losses+0.5*ties),
                       "netBenefit" = wins-losses
    )
    
    this_ES-ES
  },
  interval = c(0,prop_wl),
  tol = .Machine$double.eps
  )$root
  },
  error = function(e){NA})
  
  c(wins=wins,losses=prop_wl-wins,ties=ties)
}

# Custom Plot functions---------------

## Percentile-Percentile plot function---------------

#' Visualise stochastic dominance using percentile_percentile plot
#' @param x0 Count of outcomes in control group (including zeros)
#' @param x1 Count of outcomes in intervention group (including zeros)
#' @param ties_method Should ties be dropped or split?
#' @param tie_display Should ties be split diagonally or horizontally?
#' @param show_proportions Show proportions in vertical/horizontal bar charts
#' @param show_dichot_ci Show show dichotomised 95% CI for odds ratios on plot
#' @param show_cOR Show show curve corresponding to common odds ratio
#' @param show_labels Show show labels, if provided
#' @returns A ggplot object
pp_plot <- function(x0,x1,
                    ranks,
                    labels=NULL,
                    ties_method,
                    tie_display="horizontal",
                    show_proportions = F,
                    show_dichot_ci = T,
                    show_cOR = T,
                    show_labels = T
) {
  
  p0 <- x0 / sum(x0)
  p1 <- x1 / sum(x1)
  
  if(is.null(labels)){
    labels <- rep("",length(x0))
  }
  
  oddsCurve <- function(x,r) r*x/((r-1)*x + 1)
  oddsCurve_x <- function(x,r) r/((r-1)*x+1)^2 # First derivative with respect to x
  
  posx <- data.frame(rank0 = ranks, xmin=cumsum(p0)-p0,xmax=cumsum(p0))
  posy <- data.frame(rank1 = ranks, ymin=cumsum(p1)-p1,ymax=cumsum(p1))
  
  posx <- lapply(1:nrow(posx),function(i){posx[i,]})
  posy <- lapply(1:nrow(posy),function(i){posy[i,]})
  
  posGrid <- expand.grid(posx,posy)
  
  as.data.frame(cbind(
    do.call("rbind",posGrid[[1]]),
    do.call("rbind",posGrid[[2]])
  )) -> posGrid
  
  xlabel <- data.frame(labels=ranks, xpos = unique(apply(posGrid[,c("xmin","xmax")],1,mean)))
  ylabel <- data.frame(labels=ranks, ypos = unique(apply(posGrid[,c("ymin","ymax")],1,mean)))
  
  if(ties_method=="split"){
    splitProp <- 0.5
  } else if (ties_method =="drop"){
    splitProp <- outer(p0,p1)
    splitProp <- sum(splitProp[lower.tri(splitProp)])/(sum(splitProp[lower.tri(splitProp)])+sum(splitProp[upper.tri(splitProp)]))
  }
  
  tieGrid <- posGrid[which(posGrid$rank0==posGrid$rank1),]
  
  splitTieDf <- do.call("rbind",lapply(1:nrow(tieGrid),function(i){
    
    dx <- tieGrid[i,"xmax"]-tieGrid[i,"xmin"]
    dy <- tieGrid[i,"ymax"]-tieGrid[i,"ymin"]
    
    if(tie_display =="diagonal"){
      #   Split ties diagonally
      if(splitProp >= 0.5){
        
        tmp <- rbind(
          c(tieGrid[i,"xmin"],tieGrid[i,"ymax"]-dy*sqrt(2*(1-splitProp))),
          c(tieGrid[i,"xmin"]+dx*sqrt(2*(1-splitProp)),tieGrid[i,"ymax"])
        )
        
      } else {
        
        tmp <- rbind(
          c(tieGrid[i,"xmax"]-dx*sqrt(2*(splitProp)),tieGrid[i,"ymin"]),
          c(tieGrid[i,"xmax"],tieGrid[i,"ymin"]+dy*sqrt(2*(splitProp)))
        )
      }
    } else if(tie_display =="horizontal"){
      # Split ties horizontally
      
      tmp <- rbind(
        c(tieGrid[i,"xmin"],tieGrid[i,"ymin"]+splitProp*(tieGrid[i,"ymax"]-tieGrid[i,"ymin"])),
        c(tieGrid[i,"xmax"],tieGrid[i,"ymin"]+splitProp*(tieGrid[i,"ymax"]-tieGrid[i,"ymin"]))
      )
    }
    
    
    colnames(tmp) <- c("x","y")
    cbind(rank=tieGrid[i,"rank0"]+c(0.2,0.3),tmp)
    
  }))
  
  
  posGrid <- do.call("rbind",lapply(1:nrow(tieGrid),function(i){
    cbind(rank=tieGrid[i,"rank0"]+c(0.1,0.4),
          rbind(
            c(x=tieGrid[i,"xmin"],y=tieGrid[i,"ymin"]),
            c(x=tieGrid[i,"xmax"],y=tieGrid[i,"ymax"])
          )
    )
  }))
  
  allPoints <- cbind(as.data.frame(rbind(posGrid,splitTieDf)))
  
  allPoints <- allPoints[order(allPoints$rank),]
  
  allPoints_area <- rbind(allPoints,
                          c(rank=max(allPoints$rank)+1,
                            x=1,
                            y=0))
  
  # Estimate confidence intervals without adjustments
  
  tieGrid$estimate <- tieGrid$ymax*(1-tieGrid$xmax)/(tieGrid$xmax*(1-tieGrid$ymax))
  
  # Get halfwidth, etc. based on wald normal approximation
  tieGrid$halfWidth <- qnorm(1-0.05/2) * sqrt(1/(sum(x0)*tieGrid$xmax)+
                                                1/(sum(x1)*tieGrid$ymax)+
                                                1/(sum(x0)*(1-tieGrid$xmax))+
                                                1/(sum(x1)*(1-tieGrid$ymax)))
  
  tieGrid$lower <- exp(log(tieGrid$estimate) - tieGrid$halfWidth)
  tieGrid$upper <- exp(log(tieGrid$estimate) + tieGrid$halfWidth)
  
  do.call("rbind",lapply(1:(nrow(tieGrid)-1),function(i){
    
    x_mid <- tieGrid[i,"xmax"]
    y_mid <- oddsCurve(x_mid,tieGrid[i,"estimate"])
    
    # For each point, find intersection between lower/upper odds curves
    # and a straight line that's normal to the curve at that point
    
    # We can just rootfind this
    
    x_lower <- uniroot(function(x_lower){
      
      m <- -1/oddsCurve_x(x_mid,tieGrid[i,"estimate"])
      
      normalLineVal <- oddsCurve(x_mid,tieGrid[i,"estimate"]) + m*(x_lower-x_mid)
      
      CurveVal <- oddsCurve(x_lower,tieGrid[i,"lower"])
      
      CurveVal - normalLineVal
      
    },interval = c(0,1))$root
    
    y_lower <- oddsCurve(x_lower,tieGrid[i,"lower"])
    
    x_upper <- uniroot(function(x_upper){
      
      m <- -1/oddsCurve_x(x_mid,tieGrid[i,"estimate"])
      
      normalLineVal <- oddsCurve(x_mid,tieGrid[i,"estimate"]) + m*(x_upper-x_mid)
      
      CurveVal <- oddsCurve(x_upper,tieGrid[i,"upper"])
      
      CurveVal - normalLineVal
      
    },interval = c(1e-8,1-1e-8))$root
    
    y_upper <- oddsCurve(x_upper,tieGrid[i,"upper"])
    
    c(i=i,
      odds_lower=unname(tieGrid[i,"lower"]),
      x_lower=unname(x_lower),
      y_lower=unname(y_lower),
      odds_mid=unname(tieGrid[i,"estimate"]),
      x_mid=unname(x_mid),
      y_mid=unname(y_mid),
      odds_upper=unname(tieGrid[i,"upper"]),
      x_upper=unname(x_upper),
      y_upper=unname(y_upper)
    )
    
  }) ) -> odds
  
  odds <- as.data.frame(odds)
  
  
  rbind(
    tieGrid %>%
      mutate(weight=xmax-xmin, group=1) %>%
      dplyr::select(rank=rank0,group,weight=weight),
    tieGrid %>%
      mutate(weight=ymax-ymin, group=0) %>%
      dplyr::select(rank=rank0,group,weight=weight)
  ) %>% 
    mutate(weight=weight*ifelse(group==1,sum(x1),sum(x0)),
               rank=factor(rank)) -> propodds_df
  
  
  suppressWarnings({polr(rank~group,weights = propodds_df$weight,data=propodds_df,Hess = T)}) %>%
    (function(x){
      out <- c(coef(x),confint(profile(x)))
      names(out) <- c("estimate","lower","upper")
      out
    }) %>% exp() -> commonOdds
  
  commonOdds_df <- data.frame(qc = seq(0,1,length.out=201))
  commonOdds_df$estimate <- oddsCurve(commonOdds_df$qc,commonOdds["estimate"])
  commonOdds_df$lower <- oddsCurve(commonOdds_df$qc,commonOdds["lower"])
  commonOdds_df$upper <- oddsCurve(commonOdds_df$qc,commonOdds["upper"])
  
  ggp <- ggplot()+
    scale_x_continuous(labels=scales::percent_format())+
    scale_y_continuous(labels=scales::percent_format())+
    labs(y="Intervention",
         x="Control",
         fill="Rank"
    )+
    theme_bw()+
    theme(
      panel.grid = element_blank(),
      aspect.ratio = 1
    )+
    geom_polygon(data=allPoints_area,
                 fill="blue",alpha=0.2,
                 aes(x=x,y=y))+
    geom_path(data=allPoints,aes(x=x,y=y))+
    geom_rect(data=tieGrid,
              fill="gray",
              alpha=0.1,
              color="#999999",linetype="dashed",
              aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,
                  group=paste(rank0,rank1)
              )
    )+
    geom_abline(slope=1,intercept=0, color="dark red",size=1,linetype="dashed")+
    geom_point(data=odds,
               aes(x=x_mid,y=y_mid))+
    geom_rect(data=tieGrid,
              ymin=-0.1,ymax=0,color="black",
              aes(xmin=xmin,xmax=xmax,fill=factor(rank0)))+
    geom_rect(data=tieGrid,
              xmin=-0.1,xmax=0,color="black",
              aes(ymin=ymin,ymax=ymax,fill=factor(rank1)))  
  
  if(show_proportions){
    ggp <- ggp + 
      geom_text(data=tieGrid,
                aes(x=(xmin+xmax)/2,y=-0.05,label=sprintf("%0.2f",xmax-xmin)))+
      geom_text(data=tieGrid,
                aes(y=(ymin+ymax)/2,x=-0.05,label=sprintf("%0.2f",ymax-ymin)))+
      # TODO: Build custom scale function 
      scale_fill_brewer(palette="RdYlGn",direction=-1)
  }
  
  if(show_cOR){
    ggp <- ggp+
      geom_ribbon(data=commonOdds_df,aes(x=qc,ymin=lower,ymax=upper),fill="dark blue",alpha=0.2)+
      geom_line(data=commonOdds_df,aes(x=qc,y=estimate),color="blue")
  }
  
  
  if(show_dichot_ci){
    ggp <- ggp+geom_segment(data=odds,
                            aes(x=x_lower,y=y_lower,
                                xend=x_upper,yend=y_upper,
                                group=i))
  }
  
  
  if(show_labels & !is.null(labels)){
    
    # Drop the final value's label, we don't show
    # things at the start or end
    
    labeldf <- cbind(odds,labels=labels[1:nrow(odds)])
    labeldf <- labeldf[grepl("[:alnum:]+",labeldf$labels),]
    
    if(nrow(labeldf)){
      ggp <- ggp + 
        geom_label(data=labeldf,
                   aes(x=x_lower+0.005,y=y_lower,
                       label=labels),
                   hjust=0, size=3)
    }
    
  }
  
  ggp
  
}


## Win ratio plot function ---------------

# Function for helping build win ratio plot

analysis_results_to_wr_df <- function(df,df_overall){
  
  
  df %>% arrange(level) -> df
  
  df$win_inherit <- c(0,df$win_cumulative[1:(nrow(df)-1)])
  df$loss_inherit <- c(0,df$loss_cumulative[1:(nrow(df)-1)])
  
  df$this_win_contribution <- df$win_cumulative-df$win_inherit
  df$this_loss_contribution <- df$loss_cumulative-df$loss_inherit
  
  # Force data frame into correct structure
  df %>%
    dplyr::select(
      level,
      level_names=level_var,
      win_inherit,
      win=this_win_contribution,
      tie=tie_cumulative,
      loss=this_loss_contribution,
      loss_inherit=loss_inherit,
      
      win_ratio_individual=estimate,
      ci_lower_individual=lower,
      ci_upper_individual=upper,
      
      win_ratio_cumulative=estimate_cumulative,
      ci_lower_cumulative=lower_cumulative,
      ci_upper_cumulative=upper_cumulative
    ) -> df
  
  # Need to get wins/losses/etc into sheet for the plot function
  df_overall %>%
    mutate(win_inherit=0,
           loss_inherit=0,
           level=max(df$level)+1,
           level_var="Overall"
    ) %>%
    dplyr::select(
      level,
      level_names=level_var,
      win_inherit,
      win=win,
      tie=tie,
      loss=loss,
      loss_inherit,
      
      win_ratio_individual=estimate,
      ci_lower_individual=lower,
      ci_upper_individual=upper,
      
      win_ratio_cumulative=estimate,
      ci_lower_cumulative=lower,
      ci_upper_cumulative=upper 
    ) -> df_overall
  
  rbind(df,df_overall)
  
}

winRatioPlot <- function(df,tie_handling=NA,neutral_point=1,estimate_name="",
                         level="level", level_names="level_names", win_inherit="win_inherit", win="win", tie="tie",
                         loss="loss", loss_inherit="loss_inherit",
                         win_ratio_individual="win_ratio_individual", ci_lower_individual="ci_lower_individual", ci_upper_individual="ci_upper_individual",
                         win_ratio_cumulative="win_ratio_cumulative", ci_lower_cumulative="ci_lower_cumulative", ci_upper_cumulative="ci_upper_cumulative")
{
  
  # Include some checks to ensure that the argument inserted is a dataframe and that the necessary variables
  # are included in the dataframe and stop and report an error message if they are missing
  target_names <- c(level="level", level_names="level_names", win_inherit="win_inherit", win="win", tie="tie",
                    loss="loss", loss_inherit="loss_inherit",
                    win_ratio="win_ratio_individual", ci_lower="ci_lower_individual", ci_upper="ci_upper_individual",
                    win_ratio_cum="win_ratio_cumulative", ci_lower_cum="ci_lower_cumulative", ci_upper_cum="ci_upper_cumulative")
  names(target_names) <- c(level, level_names, win_inherit, win, tie, loss, loss_inherit, win_ratio_individual,
                           ci_lower_individual, ci_upper_individual, win_ratio_cumulative, ci_lower_cumulative, ci_upper_cumulative)
  colnames(df) <- ifelse(is.na(target_names[colnames(df)]),colnames(df),target_names[colnames(df)])
  
  expected_names <- c("level", "level_names", "win_inherit", "win", "tie",
                      "loss", "loss_inherit", "win_ratio_individual", "ci_lower_individual", "ci_upper_individual",
                      "win_ratio_cumulative", "ci_lower_cumulative", "ci_upper_cumulative")
  
  if(length(setdiff(expected_names, colnames(df)) >0)) {
    stop(sprintf("Missing variables for: %s. Check input dataframe contains these variables.",
                 paste(setdiff(expected_names, colnames(df)),collapse=", ")))
  }
  
  # Set parameters for plot
  outcome_box_width <- 1
  overall_box_width <- 0.6
  forest_plot_polygon_height <- 0.36
  forest_plot_line_offset <- 0.1
  
  # Sort data in ascending level in case dataframe is out of level order
  df <- df[order(df$level),]
  
  # Create a list of level names to use later in plot (reverse order given the plot is reversed)
  level_label_names_list <- rev(as.list(df$level_names))
  
  
  # Put dataframe in long form
  df_long <- tidyr::pivot_longer(df,
                                 c(loss_inherit,loss,tie,win,win_inherit),
                                 names_to = "pairType",values_to = "n")
  
  df_long$pairType <- factor(df_long$pairType,
                             levels = c("loss_inherit", "loss", "tie", "win", "win_inherit"),
                             labels = c("Inherited losses", "Losses", "Ties", "Wins", "Inherited wins"))
  
  # Create a proportion variable for each level
  df_long$p <- df_long$n
  for(i in unique(df_long$level)){
    df_long[df_long$level == i,"p"] <- df_long[df_long$level == i,"p"]/sum(df_long[df_long$level == i,"p"])
  }
  
  df_long <- do.call("rbind",by(df_long,df_long$level,function(df_rb){
    df_rb$p_prev <- ((cumsum(df_rb$p)-df_rb$p))
    df_rb
  }))
  
  
  # Set box width variable
  df_long$width <- ifelse(df_long$level==max(df_long$level), overall_box_width, outcome_box_width)
  
  # Win ratio plot with separate overall line
  wr_box_overall2 <- ggplot2::ggplot(df_long) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin=level-width/2,
        xmax=level+width/2,
        ymin=p_prev,
        ymax=p_prev+p,
        fill=pairType
      ),
      color="black"
    ) +
    ggplot2::scale_x_reverse(
      breaks=c(max(df_long$level):1),
      labels=level_label_names_list,
      minor_break=NULL,
      limits=c(
        max(df_long$level)+overall_box_width/2,
        outcome_box_width/2
      )
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "tan1",
        "red2",
        "khaki1",
        "forestgreen",
        "#97d42c"
      )
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        title = "Treatment comparison",
        title.theme = ggplot2::element_text(face = "bold")
      )
    ) +
    ggplot2::coord_flip() +
    ggplot2::ylab(
      expression(bold("Proportion (intervention vs control)"))
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank()
    )
  
  if(tie_handling=="drop"){
    # drop: ties_start + losses/(wins+losses)*ties
    dash_ties_start <- unname(unlist(
      df_long[
        df_long$level==max(df_long$level) & df_long$pairType=="Ties",
        "p_prev"
      ]
    ))
    
    dash_wins <- unname(unlist(
      df_long[
        df_long$level==max(df_long$level) & df_long$pairType=="Wins",
        "p"
      ]
    ))
    
    dash_losses <- unname(unlist(
      df_long[
        df_long$level==max(df_long$level) & df_long$pairType=="Losses",
        "p"
      ]
    ))
    
    dash_ties <- unname(unlist(
      df_long[
        df_long$level==max(df_long$level) & df_long$pairType=="Ties",
        "p"
      ]
    ))
    
    dash_x <- max(df_long$level)-overall_box_width/2
    dash_xend <- max(df_long$level)+overall_box_width/2
    y_dash <- dash_ties_start + dash_losses/(dash_losses+dash_wins)*dash_ties
    
    wr_box_overall2 <- wr_box_overall2 +
      ggplot2::annotate(
        "segment",
        y=y_dash,
        yend=y_dash,
        x=dash_x,
        xend=dash_xend,
        linetype="dashed"
      ) +
      ggplot2::xlab(NULL)
  }
  
  if(tie_handling=="split"){
    
    # split: ties_start + 0.5*ties
    dash_ties_start <- unname(unlist(
      df_long[
        df_long$level==max(df_long$level) & df_long$pairType=="Ties",
        "p_prev"
      ]
    ))
    
    dash_ties <- unname(unlist(
      df_long[
        df_long$level==max(df_long$level) & df_long$pairType=="Ties",
        "p"
      ]
    ))
    
    dash_x <- max(df_long$level)-overall_box_width/2
    dash_xend <- max(df_long$level)+overall_box_width/2
    y_dash <- dash_ties_start + 0.5*dash_ties
    
    wr_box_overall2 <- wr_box_overall2 +
      ggplot2::annotate(
        "segment",
        y=y_dash,
        yend=y_dash,
        x=dash_x,
        xend=dash_xend,
        linetype="dashed"
      ) +
      ggplot2::xlab(NULL)
  }
  
  # Create dataframe with info to make the green and red background panels for forest plot
  background_colourpanel <- data.frame(favours_int=c(0,1),
                                       xmin=c(-Inf,-Inf),
                                       xmax=c(Inf,Inf),
                                       ymin=c(-Inf,neutral_point),
                                       ymax=c(neutral_point,Inf))
  
  background_colourpanel$favours_int <- factor(background_colourpanel$favours_int,
                                               levels = c(0,1),
                                               labels = c("Favours control", "Favours intervention"))
  
  
  # Create a long format of the dataframe with just the data for the forest plot
  drop <- c("win_inherit", "win", "tie", "loss", "loss_inherit")
  df_forest_wide <- df[ , !(names(df) %in% drop)]
  
  df_forest <- tidyr::pivot_longer(df_forest_wide,
                                   cols = 3:8,
                                   names_pattern = "(.*)(_individual|_cumulative)$",
                                   names_to = c(".value", "outcome_type")
  )
  
  df_forest$level_offset <- ifelse(df_forest$outcome_type == "_individual",
                                   df_forest$level-forest_plot_line_offset,
                                   df_forest$level+forest_plot_line_offset)
  df_forest$outcome_type <- factor(df_forest$outcome_type,
                                   levels = c("_individual", "_cumulative"),
                                   labels = paste(c("Outcome","Cumulative"),
                                                    estimate_name)
                                  )
  
  
  # Win ratio forest plot with overall level as separate diamond and cumulative win ratio
  wr_forest_overall_diamond_cum2 <- ggplot2::ggplot(df_forest) +
    ggplot2::geom_rect(data=background_colourpanel,
                       ggplot2::aes(
                         xmin=xmin,
                         xmax=xmax,
                         ymin=ymin,
                         ymax=ymax,
                         fill=favours_int
                       ),
                       alpha=0.15) +
    ggplot2::scale_fill_manual(
      values=c("#D10707",
               "#45E928")
    ) +
    ggplot2::geom_hline(yintercept=neutral_point,color="#1A2678") +
    ggplot2::geom_line(data=df_forest[df_forest$level<max(df$level) & df_forest$outcome_type == paste("Cumulative",estimate_name), ],
                       ggplot2::aes(
                         x=level_offset,
                         y=win_ratio
                       ),
                       color="black",
                       linetype="dashed"
    ) +
    ggplot2::geom_errorbar(data = df_forest[df_forest$level<max(df_forest$level), ],
                           ggplot2::aes(ymin=ci_lower, ymax=ci_upper, x=level_offset, color=outcome_type), width=0
    ) +
    ggplot2::geom_point(data = df_forest[df_forest$level<max(df_forest$level), ],
                        ggplot2::aes(y=win_ratio, shape=outcome_type, x=level_offset, color=outcome_type), size=3
    ) +
    ggplot2::geom_polygon(
      data = data.frame(
        x_coord=c(max(df_forest$level)-forest_plot_polygon_height/2, max(df_forest$level),
                  max(df_forest$level)+forest_plot_polygon_height/2, max(df_forest$level)
        ),
        y_coord=c(unname(unlist(df_forest[df_forest$level==max(df_forest$level) & df_forest$outcome_type==  paste("Outcome",estimate_name), "win_ratio"])),
                  unname(unlist(df_forest[df_forest$level==max(df_forest$level) & df_forest$outcome_type==  paste("Outcome",estimate_name), "ci_upper"])),
                  unname(unlist(df_forest[df_forest$level==max(df_forest$level) & df_forest$outcome_type==  paste("Outcome",estimate_name), "win_ratio"])),
                  unname(unlist(df_forest[df_forest$level==max(df_forest$level) & df_forest$outcome_type==  paste("Outcome",estimate_name), "ci_lower"]))
        )
      ),
      ggplot2::aes(x=x_coord, y=y_coord)
    ) +
    ggplot2::scale_x_reverse(
      breaks=NULL,
      limits=c(
        max(df_forest$level)+overall_box_width/2,
        outcome_box_width/2
      )
    ) +
    ggplot2::scale_y_continuous(limits=c(0,NA)) +
    labs(x=NULL, y=estimate_name)+
    ggplot2::guides(
      color = ggplot2::guide_legend(reverse = FALSE, title = NULL),
      shape = ggplot2::guide_legend(title = NULL),
      fill = ggplot2::guide_legend(title = NULL)
    ) +
    ggplot2::scale_color_manual(values=c("grey40", "black")) +
    ggplot2::scale_shape_manual(values=c(4,16)) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw()+
    ggplot2::theme(axis.title.x = element_text(face="bold"))
  
  list(
    overall = wr_box_overall2,
    forestplot = wr_forest_overall_diamond_cum2,
    combined=  patchwork::wrap_plots(wr_box_overall2,
                                     wr_forest_overall_diamond_cum2) +
                          patchwork::plot_layout(guides = 'collect')
  )
  

  
}




