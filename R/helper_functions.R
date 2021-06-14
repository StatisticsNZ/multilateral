#' Get window start period index
#'
#' Calculate a sequence of period indexes corresponding to the starts of each window
#'
#' @param period_index period index vector passed from input_data
#' @param window_length  window length
#' @return A numeric sequence corresponding to the start date of each window
#' @keywords internal
get_window_st_period <- function (period_index, window_length) {

  num_windows <- length(unique(period_index)) - window_length + 1

  seq.int(from = min(period_index),
          by = 1,
          length.out = num_windows)
}

#' Get whole windows period index
#'
#' Calculate a sequence of period indexes for given window
#'
#' @param st_period Start date
#' @param window_length  window length
#' @return A period index sequence corresponding to each date in the window
#' @keywords internal
get_window_periods <- function(st_period, window_length){

  seq.int(st_period, by = 1, length.out = window_length)
}

#' Geometric mean
#'
#' Calculate the geometric mean of a vector of numbers
#'
#' @param x an R numerical object
#' @param na.rm  a logical value indicating whether NA values should be
#' stripped before the computation proceeds.
#' @return If all values in x are numeric class, a single numeric
#' class value is returned.
#' @examples
#' x <- c(0:10, 50)
#' gm_mean(x)
#'
#' @export
gm_mean <- function(x, na.rm = TRUE){
  # Implementation from here:
  # https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in

  # This is a safer implementation than using PRODUCT () as floating point
  # errors are very likely when using PRODUCT () for many large or small numbers

  if(any(x[!is.na(x)] <= 0)){
    warning("Non-positive values being ignored from supplied vector")
  }
  if(any(is.na(x)) && na.rm){
    warning("NA values being ignored from supplied vector")
  }

  #Also note this excludes any observation less than or equal to
  #zero in the calculation
  exp(mean(log(x[x > 0]), na.rm = na.rm))

}



#' All checks
#' @keywords internal
check_inputs <- function(period,
                         price,
                         index_method,
                         ...){

  params <- list(...)

  #Check if accidentally provided two of same param
  if(any(duplicated(names(params)))){
    stop(sprintf("You have provided the same parameter twice, please only provide %s once",
                 paste(names(params)[which(duplicated(names(params)))], collapse = " and ")))
  }

  #Could re do this by method
  index_method_config <- read_yaml(system.file("config","index_method_config.yaml", package = "multilateral"))
  splice_method_config <- read_yaml(system.file("config","splice_method_config.yaml", package = "multilateral"))

  all_index_methods <- names(index_method_config)
  all_splice_methods <- splice_method_config$splice_method

  requires_id <- index_method_config[index_method][[1]]$requires_id
  requires_features <- index_method_config[index_method][[1]]$requires_features
  requires_quantity <- index_method_config[index_method][[1]]$requires_quantity
  requires_weight <- index_method_config[index_method][[1]]$requires_weight
  can_restrict_to_matched_sample <- index_method_config[index_method][[1]]$can_restrict_to_matched_sample


  #ASSERTIONS

  #--------
  if(!(index_method%fin%all_index_methods)){
    stop(index_method,
         " is not a valid index method! It must be one of\n* ",
         paste(all_index_methods, collapse = "\n* "))
  }

  if(requires_quantity&is.null(params$quantity)){
    stop(index_method, " requires quantities")
  }

  if(requires_weight&is.null(params$quantity)&is.null(params$weight)){
    stop(index_method, " requires quantities or at least weights")
  }

  if(requires_features&is.null(params$features)){
    stop("You must provide a data frame of features\n Requirements of ",
         yaml::as.yaml(index_method_config[index_method]))
  }

  if(requires_id&is.null(params$id)){
    stop("You must provide a vector of ids\n Requirements of ",
         yaml::as.yaml(index_method_config[index_method]))
  }


  if(!is.null(params$splice_method)&&!(params$splice_method%fin%all_splice_methods)){
    stop(params$splice_method,
         " is not a valid splice method! It must be one of\n* ",
         paste(all_splice_methods, collapse = "\n* "))
  }

  #window_length is allowed to be NULL
  if(!is.null(params$window_length)){
    assert_is_numeric(params$window_length)
  }

  #--------Data
  if(!(class(period)%fin%c("Date","numeric", "integer", "double"))){
    stop("period must be class Date or numeric")
  }

  assert_is_numeric(price)


  if(requires_weight&is.null(params$weight)){
    assert_is_numeric(params$quantity)
  }else if(requires_weight&is.null(params$quantity)){
    assert_is_numeric(params$weight)
  }

  if(!is.null(params$quantity)&!is.null(params$weight)){
    warning("Ignoring weight as quantity is provided")
  }


  if(!is.null(params$id)&&!(class(params$id)%fin%c("character","factor", "numeric", "integer"))){
    stop("id must be class character, factor, numeric or integer")
  }



  vec_lengths <- sapply(list(period,
                             price,
                             params$id,
                             params$quantity,
                             params$weight),
                        length)

  if(length(unique(vec_lengths[vec_lengths!=0]))!=1){
    stop("period, price, id, quantity, weight must be same length or null")
  }


  #check if matched sample is applicable
  if(!is.null(params$matched)){

    assert_is_logical(params$matched)

    if(params$matched&!can_restrict_to_matched_sample){
      stop("Matched sample is not applicable for this method, people remove it")
    }


  }


  if(!is.null(params$features)&&nrow(params$features)!=length(period)){
    stop("features data.frame must have the same number",
         " of rows as the length of other variables e.g period")
  }

  #check for aggregation problem
  if(requires_id){
    agg_check <- data.table(period = period,id = params$id)
    agg_check <- agg_check[,.(not_aggregated = any(duplicated(id))),period]
    if(any(agg_check$not_aggregated)){
      stop("Multiple of the same id are being observed in a single period,",
           " aggregate before continuing")
    }
  }

  #check for aggregation problem on no id related input_data
  if(!requires_id&requires_features){
    agg_check <- cbind(period,params$features)
    if(any(duplicated(agg_check))){
      stop("Multiple of the same features are being observed in a single period,",
           " aggregate before continuing")
    }
  }


  #Only GK can use expanding or fixed base splice
  if(is.null(params$splice_method)&index_method=="GK"){
    stop("GK method requires a splice method.")
  }

  #Only GK can use expanding or fixed base splice
  if(!is.null(params$splice_method)&&index_method!="GK"&&params$splice_method %fin% c("fbew","fbmw")){
    stop("Only the GK method can use the fixed base expanding/moving window, please use an alternate")
  }

  if(!is.null(params$num_cores)){
    assert_is_numeric(params$num_cores)
  }


  return(NULL)
}





#' fill missing data points
#'
#' fill in missing observations
#' @param input_data the input_data to be imputed
#' @param p_replace what to replace missing prices with
#' @param q_replace what to replace missing quantities with
#' @keywords internal
fill_missing <- function(input_data, p_replace = 0, q_replace = 0){


  if(!is.data.table(input_data)){
    input_data <- as.data.table(input_data)
  }

  # list of time periods
  pers <- sort(unique(input_data$period_index))
  # list of products
  prods <- sort(unique(input_data$id))

  # fill out the gaps from missing/new products with replacement values.
  available <- table(input_data[,.(id, period_index)])
  if(sum(!(available == 0)) > 0){

    # which products are not available
    to_add <- as.data.frame(which(available == 0, arr.ind = TRUE))

    # generate the new observation row for price, quantity, time and product id
    new_obs <- data.table(price = rep(p_replace, nrow(to_add)),
                          quantity = rep(q_replace,nrow(to_add)),
                          id = prods[to_add$id],
                          period_index = pers[to_add$period_index])

    # add the new observations onto the dataset
    input_data <- base::merge(input_data, new_obs, all = TRUE)

    # ensure dataset still sorted by time period and product ID
    setorder(input_data, period_index, id)
  }

  return(input_data)

}
