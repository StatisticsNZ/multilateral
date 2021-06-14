#' multilateral
#'
#' A flexible implementation of multilateral price index calculation for scanner
#' data. This function can be applied on any dataset where a key attributes
#' exist (depending on method). Those are in general terms a period, ID, price,
#' and quantity. It will allow for extension of the method by the use of
#' calculation over a window of time and splicing them together.
#'
#' @param period vector of the periods corresponding to price observations. \cr
#' NOTE: \code{period} must be of class Date or numeric.
#' @param price vector of prices
#' @param index_method The index method of choice
#' @param check_inputs_ind logical, whether to check inputs or not
#' @param ... All other possible arguments, see details
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
#' include: id, quantity, weight, features, splice_method, window_length,
#' num_cores
#'
#' \code{index_method} can be one of 'TPD', 'TDH',
#' 'GEKS-J','GEKS-F','GEKS-T','GEKS-IT', or 'GK' you can view the configuration
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
#' @import snow
#' @importFrom MatrixModels glm4 coef
#' @importFrom yaml read_yaml as.yaml
#' @importFrom stats relevel lm
#' @importFrom utils head setTxtProgressBar tail txtProgressBar
multilateral <-  function(period,
                          price,
                          index_method,
                          check_inputs_ind = TRUE,
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
  window_st_period <- get_window_st_period(period = input_data$period_index,
                                       window_length = params$window_length)

  cat("Number of windows:", length(window_st_period), "\n")

  #Edge case
  if(index_method == "GK"){window_st_period <- 1}

  num_windows <- length(window_st_period)

  pb <- txtProgressBar(min = 0, max = length(window_st_period), style = 3)

  if (!is.null(params$num_cores)) {

    cat(sprintf("\nInitialising %s cores for parallelisation\n",params$num_cores))

    # Starting Parallel =======================================================
    clust <- makeCluster(params$num_cores)

    cat("Calculating across cores...\n")

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
                        setTxtProgressBar(pb, i)

                        index_model(st_period = window_st_period[i],
                                    input_data = input_data,
                                    feature_names = colnames(params$features),
                                    window_length = params$window_length,
                                    matched = params$matched,
                                    index_method = index_method,
                                    splice_method = params$splice_method)
                      })

  }

  close(pb)


  # Convert indexes from a list of data.frames to a wide dataframe
  indexes <- as.data.frame(indexes)

  cat("\nwindow calculations complete. Splicing results together\n")

  # index_list is a list of each window's fixed effects index
  index_list <- get_index_list (indexes = indexes,
                                input_data = input_data,
                                window_st_period = window_st_period,
                                window_length = params$window_length,
                                index_method = index_method)

  # Make the GEKS from the fe_list
  index_df <- get_index_df (index_list = index_list,
                            window_length = params$window_length,
                            splice_method = params$splice_method)


  # Wrap the output in a list and return
  list(index = index_df$index,
       index_windows = rbindlist(index_list),
       splice_detail = index_df$splice_detail)
}
