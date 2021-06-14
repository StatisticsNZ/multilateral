splice_update <- function (old_window, new_window, splice_method){

  update_factor <- switch(splice_method,
                          "half" = half_splice(old_window,new_window),
                          "window" = window_splice(old_window,new_window),
                          "movement" = movement_splice(old_window,new_window),
                          "geomean" = geomean_splice(old_window,new_window),
                          "geomean_short" = geomean_short_splice(old_window,new_window))

  return(update_factor)

}


half_splice <- function(old_window,new_window){

  stopifnot(length(old_window) == length(new_window))
  w <- length(old_window)

  new_window <- c(NaN, new_window)

  Pw_new <- new_window[w]
  Pw1_new <- new_window[w + 1]
  Pw_old  <- old_window[w]

  Pn_new <- new_window[ceiling(w/2)]
  Pn_old <- old_window[ceiling(w/2)]

  (Pw1_new / Pn_new) / (Pw_old / Pn_old )
}

window_splice <- function(old_window,new_window){

  stopifnot(length(old_window) == length(new_window))
  w <- length(old_window)

  new_window <- c(NaN, new_window)

  Pw_new <- new_window[w]
  Pw1_new <- new_window[w + 1]
  Pw_old  <- old_window[w]

  Pn_new <- new_window[2]
  Pn_old <- old_window[2]

  (Pw1_new / Pn_new) / (Pw_old / Pn_old )
}

geomean_short_splice <- function(old_window,new_window){


  stopifnot(length(old_window) == length(new_window))
  w <- length(old_window)

  new_window <- c(NaN, new_window)

  Pw_new <- new_window[w]
  Pw1_new <- new_window[w + 1]
  Pw_old  <- old_window[w]


  t_accum = c()
  for (t in seq(from = 1, to = w - 2, by = 1)) {
    Pt1_new <- new_window[t+1]
    Pt1_old <- old_window[t+1]
    t_accum <- c(t_accum, ((Pw1_new / Pt1_new) / (Pw_old / Pt1_old)))
  }

  gm_mean(t_accum)
}

geomean_splice <- function(old_window,new_window){

  stopifnot(length(old_window) == length(new_window))
  w <- length(old_window)

  new_window <- c(NaN, new_window)

  Pw_new <- new_window[w]
  Pw1_new <- new_window[w + 1]
  Pw_old  <- old_window[w]


  t_accum = c()
  for (t in seq(from = 1, to = w - 1, by = 1)) {
    Pt1_new <- new_window[t+1]
    Pt1_old <- old_window[t+1]
    t_accum <- c(t_accum, ((Pw1_new / Pt1_new) / (Pw_old / Pt1_old)))
  }

  gm_mean(t_accum)
}

movement_splice <- function(old_window,new_window){

  stopifnot(length(old_window) == length(new_window))
  w <- length(old_window)

  new_window <- c(NaN, new_window)

  Pw_new <- new_window[w]
  Pw1_new <- new_window[w + 1]

  (Pw1_new/Pw_new)
}


