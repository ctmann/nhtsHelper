#' Get the trip purposes
#'
#' This function requires `home_codes`, `work_codes`, and `school_codes`
#' objects be included in the project environment.

#' @param o_purp a vector of origin purposes coded as in the NHTS \code{WHYTO}
#'   variable
#' @param d_purp a vector of destination purposes coded as in the NHTS
#'   \code{WHYFROM} variable
#'
#' @return a vector of trip purposes on the following classification:
#'   \code{hbw, hbo, hbschool, nhbo, nhbw}
#'   
#' @export
get_trip_purpose <- function(o_purp, d_purp){

  purpose <- ifelse(

    # is there a home end?
    o_purp %in% home_codes | d_purp %in% home_codes,

    # home-based something
    ifelse(
      # is there a work end?
      o_purp %in% work_codes | d_purp %in% work_codes,
      "hbw",

      ifelse(
        # is there a school end?
        o_purp %in% school_codes | d_purp %in% school_codes,
        "hbschool",
        
        ifelse(
          # is there a shopping end?
          o_purp %in% shop_codes | d_purp %in% shop_codes,
          "hbshop",
          
          # else home-based other
          "hbo"
        ) 
      )
    ),

    # non-home based
    ifelse(
      # is there a work end?
      o_purp %in% work_codes | d_purp %in% work_codes,
      "nhbw",
      # else, non-home-based other
      "nhbo"
    )
  )

  purpose
}

#' Swap P and A
#'
#' This function determines if O and D need to be swapped to create a P and A
#' record, and swaps them if necessary.
#' This function requires a `home_codes`
#' object be included in the project environment.
#' @param df a trips record data_frame
#'
#' @return a data_frame with the p and a block and taz appended
#' @import dplyr
#' 
#' @export
swap_pa <- function(df){

  df %>%
    mutate(
      # if destination is at home, then p is d.
      p_block = ifelse(to %in% home_codes, d_block, o_block),
      a_block = ifelse(to %in% home_codes, o_block, d_block),
      p_taz = ifelse(to %in% home_codes, d_taz, o_taz),
      a_taz = ifelse(to %in% home_codes, o_taz, d_taz)
    ) %>%
    # drop od
    select(-o_block, -d_block, -o_taz, -d_taz)
}

#' Get activity types
#'
#' This function requires a `home_codes`
#' object be included in the project environment.

#' @param codes a vector of activity codes as found in the NHTS \code{WHYTO} or
#'   \code{WHYFROM} variables.
#'
#' @return a vector of activity codes using the following types:
#'   \code{home, work, school, pudo, other}
#'
#' @import dplyr
#' 
#' @export
get_activity_type <- function(codes){

  purpose_codes <- data_frame(
    codes = c(home_codes, work_codes, school_codes, pudo_codes, shop_codes),
    purpose = c(
      rep("home", length(home_codes)),
      rep("work", length(work_codes)),
      rep("school", length(school_codes)),
      rep("pudo", length(pudo_codes)),
      rep("shop", length(shop_codes))
    )
  )

  codes <- data_frame(codes) %>%
    left_join(purpose_codes, by = "codes") %>%
    mutate(purpose = ifelse(is.na(purpose), "other", purpose))

  codes$purpose
}

#' Determine whether to link out the activity.
#'
#' Work trip models can be thwarted unless we remove incidental stops from them.
#' This function flags activities to drop based on the duration of an activity
#' and the activity purposes surrounding it.
#'
#' @param purpose
#' @param start
#' @param end
#'
#' @return boolean vector, TRUE if the record should be dropped.
#' 
#' @export
determine_linkout <- function(purpose, start, end){

  linkout <- ifelse(
    # cannot already be home or work purpose
    purpose %in% c(work_codes, home_codes),
    FALSE,
    ifelse(
      # must be shorter than ten minutes
      get_duration(start, end) > 10,
      FALSE,
      ifelse(
        # previous activity must be home and subsequent must be work
        lag(purpose) %in% home_codes & lead(purpose) %in% work_codes,
        TRUE,
        ifelse(
          # or vice-versa
          lag(purpose) %in% work_codes & lead(purpose) %in% home_codes,
          TRUE,
          FALSE
        )
      )
    )
  )

  return(linkout)
}
