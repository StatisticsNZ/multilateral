#' Multilateral price index calculation
#'
#' A flexible implementation of multilateral price index calculation for scanner
#' data. This function can be applied on any dataset where key attributes
#' exist (depending on method). Those are in general terms a period, ID, price,
#' and quantity. It will allow for extension of the method by the use of
#' calculation over a window of time and splicing them together.
#'
#' @param period vector of the periods corresponding to price observations. \cr
#' NOTE: \code{period} must be of class Date or numeric.
#' @param price vector of prices
#' @param index_method The index method of choice
#' @param check_inputs_ind logical, whether to check inputs or not
#' @param verbose print additional information to console
#' @param ... All other possible arguments, see details
#'
#' @return A list object of length 3 containing; 
#' 
#' \code{index}, a data.frame of the final spliced price index based on the method specified \cr
#' \code{index_windows}, a data.frame containing each individual windows index before splicing \cr 
#' \code{splice_detail}, a data.frame containing the breakdown of splice information\cr
#'
#' @details
#' The function takes vectors for each of the inputs. It is important to note
#' that the \code{period} argument must be of numeric or Date class. This is
#' because the order of the dates matters. \cr \cr The function also has the
#' capability to run in parallel, using the \code{num_cores} argument. Note that
#' for smaller datasets using non-parallel code is often faster than using
#' parallelisation due to the overhead associated with dividing the job across
#' multiple cores.
#'
#' \code{...} represents all other possible arguments the user can provide, they
#' include: id, quantity, weight, features, splice_method, window_length, matched, chain_method,
#' num_cores
#'
#' \code{index_method} can be one of 'TPD', 'TDH',
#' 'GEKS-J','GEKS-F','GEKS-T', or 'GEKS-IT' you can view the configuration
#' file found under inst/config/index_method_config for more information
#'
#' \code{splice_method} can be one of 'half','window','movement','geomean', or
#' 'geomean_short' you can view the configuration file found under
#' inst/config/splice_method_config for more information
#'
#' @examples
#' tpd_index <- multilateral(period = turvey$month,
#'                           id = turvey$commodity,
#'                           price = turvey$price,
#'                           quantity = turvey$quantity,
#'                           splice_method = "geomean",
#'                           window_length = 13,
#'                           index_method = "TPD")
#' @export
#' @import data.table
#' @import fastmatch
#' @import assertive
#' @import parallel
#' @importFrom MatrixModels glm4 coef
#' @importFrom yaml read_yaml as.yaml
#' @importFrom stats relevel lm
#' @importFrom utils head setTxtProgressBar tail txtProgressBar
multilateral <-  function(period,
                          price,
                          index_method,
                          check_inputs_ind = TRUE,
                          verbose = FALSE,
                          ...) {
  
  params <- list(...)
  
  if(check_inputs_ind){
    
    check_inputs(period = period,
                 price = price,
                 index_method = index_method,
                 ...)
    
  }
  
  # make a data table from all of the inputs
  input_data <- data.table(period = period,
                           id = params$id,
                           price = price,
                           quantity = params$quantity,
                           weight = params$weight)
  
  #include features if available (else will bind NULL)
  input_data <- cbind(input_data,params$features)
  
  # It is essential that the data frame is sorted by period
  # use an if because sorting is slow, but testing is fast
  if (is.unsorted(input_data$period)){
    input_data <- setkey(input_data,period)
  }
  
  # Add column which will be used as the time index variable.
  # this is to allow the period input to be either date or numeric
  input_data[,"period_index":=fmatch(period, unique(period))]
  
  if(is.null(params$window_length)){
    params$window_length <- max(input_data$period_index)
  }
  
  # Get the indexes of the start of each window
  window_st_period <- get_window_st_period(period_index = input_data$period_index,
                                           window_length = params$window_length)
  
  if(verbose){cat("Number of windows:", length(window_st_period), "\n")}

  num_windows <- length(window_st_period)
  
  if(verbose){pb <- txtProgressBar(min = 0, max = length(window_st_period), style = 3)}
  
  if (!is.null(params$num_cores)) {
    
    if(verbose){cat(sprintf("\nInitialising %s cores for parallelisation\n",params$num_cores))}
    
    # Starting Parallel =======================================================
    clust <- makeCluster(params$num_cores)
    
    if(verbose){cat("Calculating across cores...\n")}
    
    indexes <- parLapply(clust, 1:length(window_st_period), function(i){
      index_model(st_period = window_st_period[i],
                  input_data = input_data,
                  feature_names = colnames(params$features),
                  window_length = params$window_length,
                  matched = params$matched,
                  index_method = index_method,
                  splice_method = params$splice_method)
    })
    
    indexes <-  lapply(indexes, "[[", 1)
    
    stopCluster(clust)
    
  }else{
    
    indexes <- lapply(1:length(window_st_period),
                      function(i){
                        if(verbose){setTxtProgressBar(pb, i)}
                        
                        index_model(st_period = window_st_period[i],
                                    input_data = input_data,
                                    feature_names = colnames(params$features),
                                    window_length = params$window_length,
                                    matched = params$matched,
                                    index_method = index_method,
                                    splice_method = params$splice_method)
                      })
    
  }
  
  if(verbose){close(pb)}
  
  # Convert indexes from a list of data.frames to a wide dataframe
  indexes <- as.data.frame(indexes)
  
  if(verbose){cat("\nwindow calculations complete. Splicing results together\n")}
  
  # index_list is a list of each window's fixed effects index
  index_list <- get_index_list (indexes = indexes,
                                input_data = input_data,
                                window_st_period = window_st_period,
                                window_length = params$window_length,
                                index_method = index_method)
  
  # Make the index from the index_list
  if(!is.null(params$chain_method)){
    index_df <- get_chain_index_df (index_list = index_list,
                              window_length = params$window_length,
                              chain_method = params$chain_method)
    
  }else{
  index_df <- get_index_df (index_list = index_list,
                            window_length = params$window_length,
                            splice_method = params$splice_method)
  }
  
  # Wrap the output in a list and return
  index_all <- list(index = index_df$index,
                    index_windows = rbindlist(index_list),
                    splice_detail = index_df$splice_detail)
  
  class(index_all) <- c(class(index_all),"multilateral")
  base::attr(index_all,"params") <- list(index_method = index_method,
                                         window_length = params$window_length,
                                         splice_method = params$splice_method,
                                         chain_method = params$chain_method,
                                         check_inputs_ind = check_inputs_ind,
                                         matched =  params$matched)
  
  
  return(index_all)
}


#' @rdname multilateral
#' @param x multilateral class object
#' @export
print.multilateral <- function(x, ...){
  print(x$index)
  cat("\n------------------------#\n\n")
  invisible(lapply(seq_along(attributes(x)$params),function(i){
    cat(names(attributes(x)$params[i]),":",attributes(x)$params[i][[1]],'\n')
  }))
}