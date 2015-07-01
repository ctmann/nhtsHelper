#' Check if a time falls within a window
#'
#' @param time the time to check against
#' @param start the start of the window
#' @param end the end of the window
#'
#' @return boolean, whether the time falls inside the window.
check_between <- function(time, start, end){
  start <- get_mins_from_4am(start)
  end <- get_mins_from_4am(end)
  end <- ifelse(end == 0, 1440, end)
  time <- get_mins_from_4am(time)

  at_time <- time >= start & time <= end
}


#' Calculate duration of activity or trip in minutes
#'
#' @param start a vector of start times in military time.
#' @param end a vector of end times in military time.
#'
#' @return the elapsed minutes between the times
get_duration <- function(start, end){
  # difference of times converted from military
  get_mins_from_4am(end) - get_mins_from_4am(start)
}

#' Turn military time into numerical units
#'
#' The NHTS codes its data in character strings representing military time. For
#' instance, \code{723} is 7:23 AM. This is useful, except that the day in the
#' NHTS begins at \code{0400} hours. This function converts these times into
#' a number of discrete minutes after the beginning point. This allows
#' arithmetic operations on the time.
#'
#' @param x a character string in military time, e.g. `1523` is 3:23 pm.
#'
#' @return the number of minutes after 4 am.
get_mins_from_4am <- function(x){
  hours   <- as.numeric(substr(x, 1, 2))
  minutes <- as.numeric(substr(x, 3, 4))

  # Make the military clock 0400 to 2800.
  hours[hours < 4] <- hours[hours < 4] + 24

  # Shift 4 hours back to make 4am = 0.
  hours * 60 + minutes - (4*60)
}

#' Turn number of minutes after 4 AM into military time.
#'
#' This is precisely the inverse of \code{get_mins_from_4am}.
#' @param x a numeric vector representing the number of minutes elapsed since
#'   4 am.
#' @return a character vector expressing the number of elapsed minutes in
#' military time.
get_military_time <- function(x){
  x <- x %% 1440

  hours <- floor(x/60) + 4
  minutes <- x %% 60

  # Reverse the military clock to 0000 to 2400.
  hours[hours >= 24] <- hours[hours >= 24] - 24

  paste(sprintf("%02d", hours), sprintf("%02d", minutes), sep = "")
}
