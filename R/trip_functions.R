#' Get the trip purposes
#'
#' This function requires `purposes$home_codes`, `purposes$work_codes`, and
#' `purposes$school_codes` objects be included in the project environment.
#' 
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
    o_purp %in% purposes$home_codes | d_purp %in% purposes$home_codes,

    # home-based something
    ifelse(
      # is there a work end?
      o_purp %in% purposes$work_codes | d_purp %in% purposes$work_codes,
      "hbw",

      ifelse(
        # is there a school end?
        o_purp %in% purposes$school_codes | d_purp %in% purposes$school_codes,
        "hbschool",
        
        ifelse(
          # is there a shopping end?
          o_purp %in% purposes$shop_codes | d_purp %in% purposes$shop_codes,
          "hbshop",
          
          # else home-based other
          "hbo"
        ) 
      )
    ),

    # non-home based
    ifelse(
      # is there a work end?
      o_purp %in% purposes$work_codes | d_purp %in% purposes$work_codes,
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
#' This function requires a `purposes$home_codes`
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
      p_block = ifelse(to %in% purposes$home_codes, d_block, o_block),
      a_block = ifelse(to %in% purposes$home_codes, o_block, d_block),
      p_taz = ifelse(to %in% purposes$home_codes, d_taz, o_taz),
      a_taz = ifelse(to %in% purposes$home_codes, o_taz, d_taz)
    ) %>%
    # drop od
    select(-o_block, -d_block, -o_taz, -d_taz)
}

#' Get activity types
#'
#' This function requires a `purposes$home_codes`
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
    codes = c(purposes$home_codes, purposes$work_codes, 
              purposes$school_codes, purposes$pudo_codes, purposes$shop_codes),
    purpose = c(
      rep("home", length(purposes$home_codes)),
      rep("work", length(purposes$work_codes)),
      rep("school", length(purposes$school_codes)),
      rep("pudo", length(purposes$pudo_codes)),
      rep("shop", length(purposes$shop_codes))
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
#' @param purpose Trip purpose vector
#' @param start Trip start time
#' @param end Trip end time
#' @param duration Length of time in minutes for minimum linkout time.
#'
#' @return boolean vector, TRUE if the record should be dropped.
#' 
#' @export
determine_linkout <- function(purpose, start, end, duration){

  linkout <- ifelse(
    # cannot already be home or work purpose
    purpose %in% c(purposes$work_codes, purposes$home_codes),
    FALSE,
    ifelse(
      # must be shorter than ten minutes
      get_duration(start, end) > duration,
      FALSE,
      ifelse(
        # previous activity must be home and subsequent must be work
        lag(purpose) %in% purposes$home_codes & lead(purpose) %in% 
          purposes$work_codes,
        TRUE,
        ifelse(
          # or vice-versa
          lag(purpose) %in% purposes$work_codes & lead(purpose) %in% 
            purposes$home_codes,
          TRUE,
          FALSE
        )
      )
    )
  )

  return(linkout)
}

#' Read a trips data frame
#' 
#' 
#' 
#'
#' @param file the path to the data file
#' @param clean If TRUE, will reduce to regularly used columns and process mode 
#'   codes into named modes.
#' @param linkout If given, will link out activities of less than this duration 
#'   that are part of a home-to-work tour portion.
#' 
#' @return A data frame with 
#' 
#' @importFrom readr read_csv
#' @import dplyr
#' 
#' @export
read_trips <- function(file, clean = TRUE, linkout = NULL){
  trips <- readr::read_csv(file)
  
  names(trips) <- tolower(names(trips)) # save the pinkies
  
  if(clean){
    trips <- trips %>%
      transmute(
        houseid = as.character(houseid), 
        tdcaseid = as.character(tdcaseid),
        personid = substr(tdcaseid, 1, 10),
        tdtrpnum, 
        from = as.character(whyfrom),
        to   = as.character(whyto),
        start = as.character(strttime),
        end   = as.character(endtime),
        mode = sprintf("%02d", trptrans),
        num_on_trip = trpaccmp
      ) %>%
      mutate(
        mode = ifelse(mode %in% c("09", "10", "12", "13", "14", "16", "17", "18",
                                  "24"),
                      "transit", mode),
        mode = ifelse(mode %in% c("22", "23"), "non-motorized", mode),
        mode = ifelse(mode %in% c("11"), "schoolbus", mode),
        mode = ifelse(mode %in% c("01", "02", "03", "04", "05", "06", "07", 
                                  "08", "19"),
                      "auto", mode),
        mode = ifelse(mode == "auto" & num_on_trip >= 1, "shared", mode),
        mode = ifelse(
          mode %in% c("auto", "shared", "non-motorized", "schoolbus", "transit"), 
          mode, "other")
      ) 
  }
  
  
  if(!is.null(linkout)){
    # We need to turn the trips table into an activity table.
    activities <- trips %>%
      group_by(personid) %>%
      arrange(tdtrpnum) %>%
      mutate(
        activity = to,
        t_start = start,
        act_start = end,
        # if last trip, end activity at 3:59 AM
        act_end = ifelse(is.na(lead(start)), "0359", lead(start)),
        end = act_end,
        start = act_start
      ) %>%
      
      # drop trip-specific variables
      select(houseid, personid, tdcaseid, activity, start, end, from, t_start)
    
    # make a record for the first activity
    first <- activities %>%
      group_by(personid) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(
        tdcaseid = paste(personid, "00", sep = ""),
        activity = from,
        # Begins at 4:00 a.m.
        end = t_start,
        start = "0400"
      )
    
    # bind and cleanup
    activities <- rbind(first, activities)  %>%
      group_by(personid) %>%
      select(-from, -t_start) %>%
      arrange(as.numeric(tdcaseid)) 
    
    activities <- activities %>%
      ungroup() %>%
      mutate(purpose = get_activity_type(activity))
    
    activities <- activities %>%
      mutate(
        linkout = determine_linkout(activity, start, end, linkout)
      ) %>%
      filter(!linkout) %>%
      select(-linkout)
    
    trips <- activities %>%
      group_by(personid) %>%
      mutate(
        from = activity,
        to = lead(activity),
        start_t = end,
        end_t = lead(start),
        end = end_t,
        start = start_t,
        tdcaseid = lead(tdcaseid),
        purpose = get_trip_purpose(from, to)
      ) %>% 
      select(-activity, -start_t, -end_t) %>%
      filter(!is.na(tdcaseid)) %>%
      left_join(trips %>% select(tdcaseid, mode, num_on_trip), by = "tdcaseid")
    
    return(trips)
  } else {
    trips <- trips %>%
      mutate(purpose = get_trip_purpose(from, to))
  
    return(trips)
  }

}


