#' Index model
#'
#' Parent function to all variations of multilateral calculations that will
#' be applied on each individual window
#'
#' @param st_period st period number
#' @param input_data  input data
#' @param feature_names feature names if features included
#' @param window_length window length
#' @param index_method index method
#' @param splice_method splice method
#' @param matched Whether to use matched sample or not
#' @return index numbers for given window
#' @keywords internal
index_model <- function(st_period,
                        input_data,
                        feature_names = NULL,
                        window_length,
                        index_method,
                        splice_method,
                        matched = FALSE) {
  
  # Get the periods for this window
  win_dates <- get_window_periods(st_period, window_length)
  
  # Subset input_data by the dates in this window
  input_data_win <- input_data[period_index %fin%  win_dates]
  
  #Location to add new methods
  index_coefs <- switch(index_method,
                        "GEKS-J" = GEKS(input_data_win,
                                        index_method = "GEKS-J",
                                        matched = matched),
                        
                        "GEKS-F" = GEKS(input_data_win,
                                        index_method = "GEKS-F",
                                        matched = matched),
                        
                        "GEKS-L" = GEKS(input_data_win,
                                        index_method = "GEKS-L",
                                        matched = matched),
                        
                        "GEKS-P" = GEKS(input_data_win,
                                        index_method = "GEKS-P",
                                        matched = matched),
                        
                        "GEKS-T" = GEKS(input_data_win,
                                        index_method = "GEKS-T",
                                        matched = matched),
                        
                        "GEKS-IT" = GEKS(input_data_win,
                                         index_method = "GEKS-IT",
                                         feature_names = feature_names,
                                         matched = matched),
                        
                        "TPD" = TPD(input_data_win),
                        
                        "TDH" = TDH(input_data_win)
  )
  
  
  list(data.table(index_coefs))
  
}

#' Get index list
#'
#' Take output of index_model and convert into list of each window index
#' with period information
#' @keywords internal
get_index_list <- function (indexes,
                            input_data,
                            window_st_period,
                            window_length,
                            index_method) {

  index_list <- vector(mode = "list", length = ncol(indexes))
  
  # Loop over each window and construct the DF
  for (i in 1:ncol(indexes)){
    # get the period_indexes for the current window
    period_temp <- get_window_periods(window_st_period[i],
                                      window_length = window_length)
    # Convert theses to period, and keep only unique values
    period_temp <- input_data$period[input_data$period_index %fin% period_temp]
    period_temp <- unique(period_temp)
    
    # Make the df for this list entry
    index_list[[i]] <- data.table(
      period = period_temp,
      index = as.vector(indexes[, i]),
      window_id = i)
    
    # row names are junk due to: period as factors, and rbinding rows
    rownames(index_list[[i]]) <- c()
  }
  
  index_list
}

#' Get spliced index df and splice information
#'
#' Take output of get_index_list splice the windows together to produce a
#' continuous time series of indexes
#' @keywords internal
get_index_df <- function (index_list, window_length, splice_method) {
  
  if(length(index_list)==1){
    return(list(index = index_list[[1]],
                splice_info = NULL))
  }
  
  index_update_list <- vector(mode = "list", length = length(index_list))
  splice_detail_list <- vector(mode = "list", length = length(index_list))
  
  index_update_list[[1]] <- index_list[[1]]
  
  
  # Loop over the windows, starting at the second window
  for (i in 2: length(index_list)){
    old_window <- index_list[[i - 1]]$index
    new_window <- index_list[[i]]$index
    
    # Get the previous index value
    latest_index <- tail(index_update_list[[i-1]]$index,1)
    
    update_factor <- splice_update (old_window,
                                    new_window,
                                    splice_method = splice_method)
    
    #update_factor = latest window movement * revision factor
    #Therefore revision factor = update_factor / latest window movement
    latest_window_movement <- (new_window[length(new_window)]/new_window[length(new_window)-1])
    revision_factor <- update_factor/latest_window_movement
    
    # buid up the new row
    # period is the last date in the current window
    last_period <- index_list[[i]]$period[window_length]
    
    
    new_splice_detail_row <- data.table(period = last_period,
                                        latest_window_movement = latest_window_movement,
                                        revision_factor = revision_factor,
                                        update_factor = update_factor,
                                        window_id = i)
    
    splice_detail_list[[i]] <- new_splice_detail_row
    
    # Get the "new" index value
    new_index <- latest_index * update_factor
    
    new_index_row <- data.table(period = last_period,
                                index = new_index,
                                window_id = i)
    
    index_update_list[[i]] <- new_index_row
  }
  
  return(list(index = rbindlist(index_update_list),
              splice_detail = rbindlist(splice_detail_list)))
}


#' Get chained index df and splice information
#'
#' Take output of get_index_list chain the windows together to produce a
#' continuous time series of indexes
#' @keywords internal
get_chain_index_df <- function (index_list, window_length, chain_method) {
  
  if(length(index_list)==1){
    return(list(index = index_list[[1]],
                splice_info = NULL))
  }
  
  #Convert index_list into movements
  movement_list <- lapply(index_list,function(x){
    x$index <- x$index/data.table::shift(x$index,1)
    return(x)
  }
  )
  
  #Convert chain method name to chain position
  chain_position <- switch(chain_method,
                           "window" = 2, 
                           "movement" = window_length,
                           "half" = ceiling(window_length/2)+1)
  
  index_update_list <- vector(mode = "list", length = length(index_list))
  
  if(chain_method=="geomean"){
    
    movement_df <- rbindlist(movement_list)
    
    all_periods <- unique(movement_df$period)
    
    #Geomean across period
    update_factors <- sapply(all_periods[-1], function(x) {
      
      period_of_interest <- movement_df[period==x&!is.na(index)]
      
      gm_mean(period_of_interest$index)
      
    })
    
    index <- data.table(period = all_periods,
                        index = cumprod(c(1,update_factors)),
                        window_id = "geomean")
  }else{
    
    
    index_update_list[[1]] <- data.table(period = index_list[[1]]$period[chain_position-1],
                                         index = 1,
                                         window_id = 1)
    
    index_update_list[[2]] <- data.table(period = index_list[[1]]$period[chain_position],
                                         index = movement_list[[1]]$index[chain_position],
                                         window_id = 1)
    
    # Loop over the windows, starting at the second window
    for (i in 2: length(index_list)){
      
      latest_index <- index_update_list[[i]]$index
      
      update_factor <- movement_list[[i]]$index[chain_position]
      
      new_period <- index_list[[i]]$period[chain_position]
      
      new_index <- latest_index * update_factor
      
      new_index_row <- data.table(period = new_period,
                                  index = new_index,
                                  window_id = i)
      
      index_update_list[[i+1]] <- new_index_row
    }
    
    index <- rbindlist(index_update_list)
  }
  
  
  return(list(index = index,
              splice_detail = NULL))
}





