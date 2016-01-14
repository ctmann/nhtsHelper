# Build a default purpose codes list
purposes <- list(
  work_codes   = c("10", "11", "12"),
  home_codes   = c("1"),
  school_codes = c("21"),
  pudo_codes   = c("70", "71", "72", "73"),
  shop_codes   = c("40", "41", "42", "43", "80", "81", "82", "83")
)

devtools::use_data(purposes, overwrite = TRUE)
