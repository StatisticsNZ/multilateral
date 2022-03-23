#' GEKS
#'
#' The GEKS method is the product of many bilateral comparisons that maximises
#' the number of paired price points while being free of chain drift. The GEKS
#' has the following variations 'GEKS-F', 'GEKS-T','GEKS-J', 'GEKS-IT'
#' @keywords internal
GEKS <- function(input_data,
                 index_method,
                 matched = NULL,
                 feature_names = NULL){
  
  if(is.null(matched)){
    #Recommend not requiring matched with GEKS-IT while true for all others 
    matched <- ifelse(index_method=="GEKS-IT",FALSE,TRUE)
  }
  
  #Rebase period
  input_data[,"period_index":=fmatch(period_index, unique(period_index))]
  
  #Must sort as bilateral assumes p0 and p1 vectors correlate to same product
  sortcol <-c("period_index","id")
  setorderv(input_data, sortcol)
  
  tempGEK <- GEKS_w(input_data,
                    index_method,
                    matched = matched,
                    feature_names = feature_names)
  
  # initiate a vector of warnings for NAs
  if(length(tempGEK$na_pairs) > 0){
    na_warn <- paste0("In window: ",tempGEK$na_pairs,"\n")
  }else{
    na_warn <- character()
  }
  
  # if there were periods where there were no overlapping products then
  # print a warning
  if(length(na_warn) > 0){
    warning(paste0("The following windows contained bilateral comparisons where no overlapping products were found: \n",
                   "Window: Pairs \n",na_warn))
  }
  
  return(tempGEK$pgeo)
}

#' GEKS
#'
#' Sub GEKS function
#' @keywords internal
#' @noRd
GEKS_w <- function(input_data,
                   index_method,
                   matched,
                   feature_names = NULL){
  
  
  # get the window length
  n <- max(input_data$period_index,na.rm = TRUE)
  
  # initialise some matrices
  # matrix of all price indices using each month as the base period
  pindices <- matrix(0, nrow = n, ncol = n)
  
  # get the vector of period indices that are inside the window
  pi <- unique(input_data$period_index)
  
  # initialise a vector for storing NA pair information
  na_pairs <- character()
  
  # for every period in the window...
  for(j in 1:n){
    
    # for every period in the window...
    for(k in 1:n){
      # if j=k then the index is 1
      if(j==k){
        pindices[j,k] <- 1
      }
      # if we're below the diagonal, then use symmetry to
      # save computation time
      else if(j>k){
        pindices[j,k] <- 1/pindices[k,j]
      }
      else {
        # set the period pi(j) = base period
        input_data_t0 <- input_data[input_data$period_index == pi[j]]
        # set the period pi(k) = period '1'
        input_data_t1 <- input_data[input_data$period_index  == pi[k]]
        
        # if user asked for matching, get matched samples
        if(matched){
          input_data_t0 <- input_data_t0[id %fin% unique(input_data_t1$id)]
          input_data_t1 <- input_data_t1[id %fin% unique(input_data_t0$id)]
        }
        
        # set the price index element to NA if there are no
        # matches
        if(nrow(input_data_t1)==0){
          pindices[j,k] <- NA
          na_pairs <- paste0(na_pairs, paste0("(",j,",",k,")"), collapse = ",")
        }
        else{
          # set the price and quantity vectors
          p0 <- input_data_t0$price
          p1 <- input_data_t1$price
          q0 <- input_data_t0$quantity
          q1 <- input_data_t1$quantity
          
          if(index_method=="GEKS-IT"){
            
            f0 <- input_data_t0[,..feature_names]
            f1 <- input_data_t1[,..feature_names]
            id0 <- input_data_t0$id
            id1 <- input_data_t1$id
            
          }
          # calculate the price index for 'base' period j and 'next' period k
          switch(index_method,
                 'GEKS-F' = {pindices[j,k] <- fisher_t(p0,p1,q0,q1)},
                 'GEKS-T' = {pindices[j,k] <- tornqvist_t(p0,p1,q0,q1)},
                 'GEKS-IT' = {pindices[j,k] <- IT_t(p0,p1,q0,q1,f0,f1,id0,id1)},
                 'GEKS-J' = {pindices[j,k] <- jevons_t(p0,p1)})
        }
        
      }
    }
  }
  # compute the geometric mean of each column of the price indices matrix
  pgeo <- apply(pindices, 2, gm_mean, na.rm = TRUE)
  
  # normalise to the first period
  pgeo <- pgeo/pgeo[1]
  
  return(list(pgeo=pgeo, na_pairs=na_pairs))
}

#' Time Product Dummy (TPD), also known as Fixed Effects Window Splice (FEWS)
#'
#'The TPD approach models price against time and a unique ID for each product,
#'where the product id can be seen as representing the 'bundles' of all the
#'price-determining characteristics (features) of that product, to produce a
#'fully quality-adjusted price index from the parameters estimated for time. A
#'new product must have at least two observations before it is non-trivially
#'incorporated into the calculation, which means that a splice method other than
#'the (latest) movement splice must be used, to ensure that the index isn't
#'biased away from the impact of new products. For example, the window splice
#'which is the original formulation of the method presented as the FEWS index
#'
#' @keywords internal
TPD <- function(input_data){
  
  # A cryptic error is thrown if there is a time period which has no id's whcih
  # are present in other periods (in window). Doing a test for this, and giving
  # a more sensible error. To solve this either remove the offending time period
  # or impute
  ids_more_than_1 <- input_data$id[duplicated(input_data$id)]
  id_counts <- input_data[,.(unq=mean(id%fin%ids_more_than_1)), by = "period"]
  
  if (any(id_counts$unq == 0)){
    bad_period <- id_counts[unq==0,period]
    stop("One or more time periods have only got ids which occur at no other ",
         "time periods. The offending time periods are: \n",
         paste(bad_period, collapse = ", "))
  }
  
  
  
  # Refactor the dates here. Otherwise columns are created in the regression
  # matrix with all zeros, corresponding to dates not in the current window
  input_data[,c("logprice",
                "timefact",
                "idfact"):=list(log(price),
                                factor(period_index),
                                factor(id))]
  
  if(!is.null(input_data$quantity)){
    input_data[,"weight":=price*quantity]
  }
  
  # glm uses the alphabetically first id as the reference. However, if this
  # value doesn't appear in the then all other values are being compared
  # to a number that is essentially zero. Hence you can get crazily high numbers
  # like indexes of 10^50 from normal looking data
  input_data <- within(input_data,
                       idfact <- relevel(idfact,
                                         ref = as.character(input_data$idfact[1])))
  
  
  # Regression doesn't work if there is only 1 item in the time window.
  if (nlevels(input_data$idfact) == 1) {
    glm_formula <- input_data$logprice ~ input_data$timefact
  } else {
    glm_formula <- input_data$logprice ~ input_data$timefact + input_data$idfact
  }
  
  # Run the regression
  all_coefs <-  coef(glm4(glm_formula,
                          weights = input_data$weight,
                          sparse = TRUE))
  
  # There are coefficients returned for each time period, and each product.
  # we are only interested in change of price wrt time - so only keep theses
  # coefficients. Theses rownames start with timefact
  rows_keep <- grepl(".*timefact.*", names(all_coefs))
  names(all_coefs) <- NULL
  
  # subest to just timefact values, and return in a list
  c(1,exp(all_coefs[rows_keep]))
}


#' Time Dummy Hedonic (TDH)
#'
#' The TDH approach models price against time and product characteristics
#' (features), to produce a fully quality-adjusted price index from the
#' parameters estimated for time.
#'
#' @keywords internal
TDH <- function(input_data){
  # Regress the dates and id's as factors against logprice
  # Arguments
  #   input_data - a input_data frame with logprices, weights and id's
  # Returns
  #   modelOutput - the output of the linear model
  
  # Refactor the dates here. Otherwise columns are created in the regression
  # matrix with all zeros, corresponding to dates not in the current window
  input_data[,c("logprice",
                "timefact"):=list(log(price),
                                  factor(period_index))]
  
  
  if(!is.null(input_data$quantity)){
    input_data[,"weight":=price*quantity]
  }
  
  #We do not want ID in the model
  if("id"%in%names(input_data)){
  input_data[,"id":=NULL]
  }

  input_data <- droplevels(input_data)
  
  #Even though the data as a whole may have all characteristics with more than one level
  #In a given window this criteria may fail. To catch this it checks the number of levels
  #for each characteristic, if it is equal to one it will be removed.
  cols_to_drop <- which(sapply(input_data,nlevels)<=1&sapply(input_data,is.factor))
  if(length(cols_to_drop)!=0){
  input_data[,cols_to_drop] <- NULL #Final check of factors with 1 or 0 level(s)
  }
  # Run the regression
  all_coefs <-  coef(lm(logprice ~ timefact + . - price - period_index - period - weight,
                        weights = weight,
                        data = input_data))
  
  # There are coefficients returned for each time period, and each product.
  # we are only interested in change of price due to time - so only keep theses
  # coefficients. These rownames start with timefact
  rows_keep <- grepl(".*timefact.*", names(all_coefs))
  names(all_coefs) <- NULL
  
  # subest to just timefact values, and return in a list
  c(1,exp(all_coefs[rows_keep]))
}
