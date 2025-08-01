



getDOORList <- function(df,direction){

  if(!all(colnames(df)==names(direction))) stop("Direction names must match column names")

  classes <- sapply(1:ncol(df),function(i){class(df[,i])})

  whichClasses <- list(
    Surv_good = which(classes=="Surv" & direction == "<" ), # Earlier time good
    Surv_bad = which(classes=="Surv" & direction == ">" ), # Earlier time bad
    logical_good = which(classes=="logical"  & direction == ">" ), # Event good
    logical_bad = which(classes=="logical" & direction == "<" ), # Event bad
    numeric_good = which(classes %in% c("integer","numeric") & direction == ">" ), # Higher value good
    numeric_bad = which(classes %in% c("integer","numeric") & direction == "<" ) # Higher value bad
  )

  whichClasses <- whichClasses[sapply(whichClasses,function(x){length(x)>0})]

  classCombn <- lapply(whichClasses, function(theseCols){


    if(length(theseCols)==0) return(NULL)

    recursivePart <- function(partition,
                              unselected = 1:length(partition),
                              block=1,
                              allocation=rep(NA,length(partition)
                              )){

      # There's a bug in this for the case where partition = c(1,1,1,1,...)
      # Adding as a special case is easier than reworking to avoid.
      if(all(partition==1)) return(list(list(tree=lapply(1:length(partition),function(x){x}),
                                             allocation=1:length(partition)
      )))
      # print("--------------")
      # print(list(
      #   partition=partition,
      #   unselected=unselected,
      #   block=block
      # ))
      apply(combn(unselected, partition[block]),2,function(selected){

        new_unselected <- unselected[!(unselected %in% selected)]

        new_allocation <- allocation
        new_allocation[selected] <- block
        #
        #     print(list(selected=selected,
        #                new_unselected=new_unselected
        #     ))

        if(length(new_unselected)>2){
          out <- list(
            tree=c(thisLevel=list(selected), recursivePart(partition,
                                                           unselected = new_unselected,
                                                           block=block+1,
                                                           allocation=new_allocation
            )$tree),
            allocation=new_allocation
          )
        } else {

          new_allocation[new_unselected] <- block+1

          # print(new_allocation)
          out <- list(
            tree=c(list(selected),list(new_unselected)),
            allocation=new_allocation
          )
        }

        out

      })

    }

    if(length(theseCols) == 1) return(matrix(1))

    groupings <- do.call("rbind",apply(partitions::parts(length(theseCols)),2,function(p){
      tmp <- recursivePart(p)
      do.call("rbind",lapply(tmp,function(x){x$allocation}))
    }))

    groupings
  })



  # We are not allowing for combining continuous variables.
  # Force all numerics to be in their own group
  if(length(classCombn$numeric_good)>0){
    classCombn$numeric_good <- matrix(1:ncol(classCombn$numeric_good), nrow=1)
  }

  if(length(classCombn$numeric_bad)>0){
    classCombn$numeric_bad <- matrix(1:ncol(classCombn$numeric_bad), nrow=1)
  }


  # Get all possible ways of combining columns
  classCombnGrid <- do.call("expand.grid",
                            lapply(classCombn,function(x){1:nrow(x)})
  )

  colnames(classCombnGrid) <- names(classCombn) <- names(whichClasses)

  # Loop over each of these combinations and get list of permutations within it

  #https://stackoverflow.com/a/20199902/11302715
  permutations <- function(n){
    if(n==1){
      return(matrix(1))
    } else {
      sp <- permutations(n-1)
      p <- nrow(sp)
      A <- matrix(nrow=n*p,ncol=n)
      for(i in 1:n){
        A[(i-1)*p+1:p,] <- cbind(i,sp+(sp>=i))
      }
      return(A)
    }
  }

  var_combn_list <- lapply(1:nrow(classCombnGrid),function(i){NA})

  for(i_combn in 1:nrow(classCombnGrid)){

    print(i_combn)

    # For this combination, get variable group ID

    thisCombnSet <- classCombnGrid[i_combn,]

    groupMember <- rep(NA,ncol(df))
    names(groupMember) <- colnames(df)

    for(i_class in names(classCombn)){

      groupMember[whichClasses[[i_class]]] <- paste(i_class,
                                                    classCombn[[i_class]][thisCombnSet[1,i_class],],
                                                    sep="_")

    }

    # If the number of groups to rank is above some threshold number, stop here
    # and move to the next way of combining things
    uniqueGroups <- unique(groupMember)
    print(length(uniqueGroups))

    if(length(uniqueGroups)>10) next



    # Then run perm(max(id)) to get all possible ways of ordering these
    thesePerms <- permutations(length(uniqueGroups))
    colnames(thesePerms) <- uniqueGroups

    # Finally, if we're combining binaries, there's more than one possible way to do it
    combineMethod <- lapply(unique(groupMember[
      do.call("c",unname(whichClasses[grepl("logical",names(whichClasses))]))
    ]),
    function(i){

      if(sum(groupMember==i) > 1){
        return(c("any","sum"))
      } else {
        return(c("any"))
      }
    })

    names(combineMethod) <- unique(groupMember[
      do.call("c",unname(whichClasses[grepl("logical",names(whichClasses))]))
    ])

    combineMethod <- c(i_perm=list(1:nrow(thesePerms)),combineMethod)

    combineMethod <- do.call("expand.grid",combineMethod)


    var_combn_list[[i_combn]] <- pbapply::pblapply(1:nrow(combineMethod),function(i){
      list(
        combine = groupMember,
        ordering = thesePerms[combineMethod$i_perm[i],],
        combineMethod=combineMethod[i,setdiff(colnames(combineMethod),"i_perm")]
      )
    })


  }

  var_combn_list <- do.call("c",var_combn_list)



  lapply(var_combn_list,function(x){

    if(length(x$combineMethod)==1){
      names(x$combineMethod) <- names(x$ordering)[grepl("logical",names(x$ordering))]
    } else {
      x$combineMethod <- unlist(x$combineMethod)
    }

    x$direction <-
      tapply(
        direction[names(x$combine)],
        x$combine,
        unique
      )

    x

  })

}


build_transformed_df <- function(DOOR,df){

  rankedGroups <- sort(DOOR$ordering)


  transformed_df <- lapply(names(rankedGroups), function(i){

    out <- do.call("rbind",lapply(1:nrow(df), function(j){


      theseVars <- df[j,names(DOOR$combine[DOOR$combine==i]),drop=F]


      if(grepl("Surv",i)){

        firstTime <- Inf
        firstEvent <- 0

        for(k in 1:ncol(theseVars)){

          if(theseVars[,k][,"status"] == 1){

            if(firstEvent == 0){
              # Currently no event recorded. Use new info by default

              firstEvent <- 1
              firstTime <- theseVars[,k][,"time"]
            } else {

              # Only update if new event happened earlier
              if(firstTime > theseVars[,k][,"time"]){
                firstTime <- theseVars[,k][,"time"]
              }

            }

          } else {

            if(firstEvent == 0){

              # No event observed. Use earliest censoring time
              if(firstTime > theseVars[,k][,"time"]){
                firstTime <- theseVars[,k][,"time"]
              }

            } else {
              # If we have an event already, do nothing
            }
          }


        }

        return(  data.frame(firstTime=firstTime,firstEvent=firstEvent) )


      } else if(grepl("logical",i)){

        if(DOOR$combineMethod[i]=="any"){
          return(apply(theseVars,1,any))
        } else {
          return(apply(theseVars,1,sum))
        }

      } else {

        if(ncol(theseVars)>1) stop(sprintf("Can't combine %s",i))

        return(theseVars[,1])
      }

    }))

    if(grepl("Surv",i)){
      out <- survival::Surv(out$firstTime,out$firstEvent)
    } else {
      out <- c(out)
    }

    out

  })

  names(transformed_df) <- names(rankedGroups)
  transformed_df <- do.call("data.frame",transformed_df)


  transformed_df

}


construct_K <- function(DOOR, df){


  # Binary and continuous can be compared using < and > operators
  # Define extention for survival data
  `<.Surv` <- function(x,y){

    if( all(c(x[,"status"], y[,"status"]) == 1)) {

      if(x[,"time"] < y[,"time"]) return(1)
      if(x[,"time"] > y[,"time"]) return(-1)
      if(x[,"time"] == y[,"time"]) return(0)

    } else if( x[,"status"] == 1){

      if(x[,"time"] < y[,"time"]){
        return(1)
      } else{
        return(0)
      }


    } else if( y[,"status"] == 1){

      if(x[,"time"] > y[,"time"]){
        return(-1)
      } else{
        return(0)
      }

    } else {

      return(0)

    }
  }



  K <- matrix(0,nrow=nrow(df), ncol=nrow(df))

  for(i in colnames(df)){


    # Build K_new based on column i
    K_new  <- matrix(NA,nrow=nrow(df), ncol=nrow(df))

    for( patient_i in 1:nrow(df)){
      for( patient_j in patient_i:nrow(df)){

        this_k <- df[patient_i,i]<df[patient_j,i]

        if(DOOR$direction[i] == ">") this_k <- -1 * this_k
        K_new[patient_i,patient_j] <- this_k
        K_new[patient_j,patient_i] <- -1*this_k

      }
    }

    # Insert K_new into K

    K[K==0] <- K_new[K==0]

  }

  K

}




