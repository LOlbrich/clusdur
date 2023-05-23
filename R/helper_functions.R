#' @title wide_to_long
#'
#' @description Provides an overview table for the time and scope conditions of
#'     a data set
#'
#' @param dat A data set object
#' @param id Scope (e.g., country codes or individual IDs)
#' @param time Time (e.g., time periods are given by years, months, ...)
#'
#' @return A data frame object that contains a summary of a sample that
#'     can later be converted to a TeX output using \code{overview_print}
#' @examples
#' data(toydata)
#' output_table <- overview_tab(dat = toydata, id = ccode, time = year)
#' @export
#' @import data.table

# generate long dataset
wide_to_long <- function(data, id, cols){

  # set data to data.table
  df <- setDT(data)

  # reshape data to long format
  data_long <- data.table::melt(df, id.vars = c(id), measure = cols, value.name = "duration", variable.name = "item")

  # replace zeroes with NA
  data_long[, duration := fifelse(duration <= 0, NA_real_, duration)]

  # count number of valid latencies by item
  data_long[, item_count := sum(!is.na(duration)), item]

  # generate order of items
  data_long[, item_order := 1:.N, id]

  # calculate relative duration
  data_long[, duration_rel := frank(duration, na.last = "keep") / item_count, item]

  # replace missings with median relative rank
  data_long[, duration_rel_median := median(duration_rel, na.rm = TRUE), id]
  data_long[, duration_rel := ifelse(is.na(duration_rel), duration_rel_median, duration_rel)]

  # calculate deciles
  data_long[, duration_decile := as.character(cut(duration_rel, breaks = seq(0, 1, 0.1))), item]
  data_long[, duration_decile := ifelse(is.na(duration), "missing", duration_decile), item]

  data_long
}

#' @title long_to_wide
#'
#' @description Provides an overview table for the time and scope conditions of
#'     a data set
#'
#' @param dat A data set object
#' @param id Scope (e.g., country codes or individual IDs)
#' @param time Time (e.g., time periods are given by years, months, ...)
#'
#' @return A data frame object that contains a summary of a sample that
#'     can later be converted to a TeX output using \code{overview_print}
#' @examples
#' data(toydata)
#' output_table <- overview_tab(dat = toydata, id = ccode, time = year)
#' @export
#' @import data.table

# calculate relative frequencies
long_to_wide <- function(data, id){
  # generate wide dataset
  dcast(data, as.formula(paste0(as.character(id), "~ item")), value.var = "duration_rel")

}


