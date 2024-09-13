
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






# Win ratio plot function
winRatioPlot <- function(df,tie_handling=NA,
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
  
  df <- df[,expected_names]
  
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
                                       xmin=c((max(df$level)+overall_box_width/2),(max(df$level)+overall_box_width/2)),
                                       xmax=c(outcome_box_width/2,outcome_box_width/2),
                                       ymin=c(0,1),
                                       ymax=c(1,max(df$ci_upper_individual)))
  
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
                                   labels = c("Outcome win ratio", "Cumulative win ratio"))
  
  
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
    ggplot2::geom_hline(yintercept=1,color="#1A2678") +
    ggplot2::geom_line(data=df_forest[df_forest$level<max(df$level) & df_forest$outcome_type == "Cumulative win ratio", ],
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
        y_coord=c(unname(unlist(df_forest[df_forest$level==max(df_forest$level) & df_forest$outcome_type=="Outcome win ratio", "win_ratio"])),
                  unname(unlist(df_forest[df_forest$level==max(df_forest$level) & df_forest$outcome_type=="Outcome win ratio", "ci_upper"])),
                  unname(unlist(df_forest[df_forest$level==max(df_forest$level) & df_forest$outcome_type=="Outcome win ratio", "win_ratio"])),
                  unname(unlist(df_forest[df_forest$level==max(df_forest$level) & df_forest$outcome_type=="Outcome win ratio", "ci_lower"]))
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
    ggplot2::xlab(NULL) +
    ggplot2::ylab(expression(bold("Win ratio"))) +
    ggplot2::guides(
      color = ggplot2::guide_legend(reverse = FALSE, title = NULL),
      shape = ggplot2::guide_legend(title = NULL),
      fill = ggplot2::guide_legend(title = NULL)
    ) +
    ggplot2::scale_color_manual(values=c("grey40", "black")) +
    ggplot2::scale_shape_manual(values=c(4,16)) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw()
  
  patchwork::wrap_plots(wr_box_overall2, wr_forest_overall_diamond_cum2) + patchwork::plot_layout(guides = 'collect')
  
}




