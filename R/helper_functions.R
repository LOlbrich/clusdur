#' @title wide_to_long
#'
#' @description Reshapes duration data from wide to long and generate relative ranks
#'
#' @param data The timestamp data in wide format
#' @param id Respondent identifier
#' @param cols The columns holding the duration data
#'
#' @return A data.table object containing the columns id, item, duration, duration_rel, duration_decile
#' @examples
#' data(toydata)
#' output_table <- overview_tab(dat = toydata, id = ccode, time = year)
#' @export
#' @import data.table
#' @importFrom stats median

# generate long dataset
wide_to_long <- function(data, id, cols){

  # set data to data.table
  df <- setDT(data)

  # reshape data to long format
  data_long <- data.table::melt(df, id.vars = c(id), measure.vars = cols, value.name = "duration", variable.name = "item")

  # count number of valid latencies by item
  data_long[, item_count := sum(!is.na(duration)), item]

  # generate order of items
  data_long[, item_order := 1:.N, id]

  # calculate relative duration
  data_long[, duration_rel := frank(duration, na.last = "keep") / item_count, item]

  # replace missings with median relative rank
  data_long[, duration_rel_median := stats::median(duration_rel, na.rm = TRUE), id]
  data_long[, duration_rel := ifelse(is.na(duration_rel), duration_rel_median, duration_rel)]
  data_long[, duration_rel_median := NULL]

  # calculate deciles
  data_long[, duration_decile := as.character(cut(duration_rel, breaks = seq(0, 1, 0.1))), item]
  data_long[, duration_decile := ifelse(is.na(duration), "missing", duration_decile), item]

  # return long data
  data_long
}

#' @title long_to_wide
#'
#' @description Reshapes duration data from long to wide for cluster analysis
#'
#' @param data The timestamp data in long format
#' @param id Respondent identifier
#'
#' @return A data.table object holding the relative ranks of durations in wide format
#' @examples
#' data(toydata)
#' output_table <- overview_tab(dat = toydata, id = ccode, time = year)
#' @export
#' @import data.table
#' @importFrom stats as.formula

# calculate relative frequencies
long_to_wide <- function(data, id){
  # generate wide dataset
  data.table::dcast(data, stats::as.formula(paste0(as.character(id), "~ item")), value.var = "duration_rel")

}


