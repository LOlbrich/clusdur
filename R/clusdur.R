#' @title clusdur
#'
#' @description Clusters respondents based on their timestamp data. The relative rank of the duration for each screen is used as input to the cluster analysis.
#'
#' @param data A data set in wide format containing the timestamp data.
#' @param id The respondent id.
#' @param cols The columns containing the timestamps.
#' @param method The linkage method (see [fastcluster::hclust()]). Default is "ward.D2".
#' @param k Number of clusters. Default is 2.
#'
#' @return clusdur returns an object of class "clusdur". The function [clusdur::summary()] is used to obtain and print a summary of the cluster analysis. The function [clusdur::plot()] is used to obtain a plot of the composition of the clusters with regard to duration deciles for each screen. The function [clusdur::shiny_clusdur()] is used to obtain a simple shiny app that shows the duration distributions for each screen.
#' @return `raw_data` The used data
#' @return `long_data` The data in long format with relative durations and duration deciles
#' @return `cluster_id` A dataframe holding the id, cluster id, and the cluster id with the cluster size
#' @return `k` The number of clusters
#' @return `method` The clustering method
#' @return `tree` The fitted tree
#' @examples
#' # load simulated data
#' data(dur_sim)
#'
#' # fit cluster analysis
#' fit <- clusdur(data = dur_sim, id = "id", cols = 2:51, method = "ward.D2", k = 2)
#'
#' # plot distribution across deciles
#' plot(fit)
#'
#' # print summary of cluster analysis
#' summary(fit)
#'
#' # run shiny app showing duration distributions by cluster
#' shiny_clusdur(fit)
#'
#' @export
#' @import data.table
#' @import fastcluster
#' @importFrom stats cutree dist

clusdur <- function(data, id, cols, method = "ward.D2", k = 2){

  # check for errors to breakoff as early as possible

  # check if data is a data.frame
  if(!(is.data.frame(data))){
    stop("data is not a data.frame")
  }

  # check correct method is selected
  if(!(method %in% c("single", "complete", "average", "mcquitty", "ward.D", "ward.D2", "centroid", "median"))){
    stop("Please select one of single, complete, average, mcquitty, ward.D, ward.D2, centroid or median as method.")
  }

  # check if id is in dataframe
  if(!(id %in% colnames(data))){
    stop("Provided id is not a column name in data.")
  }

  # check is k is an integer
  if(!is.numeric(k) | abs(k) - round(k) > .Machine$double.eps^0.5){
    stop("k is not a whole number.")
  }

  # reshape data from wide to long
  df_long <- wide_to_long(data, id, cols)

  # check for negative durations
  if(min(df_long$duration < 0)){
    stop("Dataframe contains negative durations.")
  }

  # reshape from long to wide
  df_wide <- long_to_wide(df_long, id)

  # run cluster analysis
  cluster_solution <- fastcluster::hclust(dist(df_wide[, 2:ncol(df_wide)]), method = method)

  # store id, cluster, and cluster label in data.table
  df_clust <- df_wide[, cluster := stats::cutree(cluster_solution, k = k)][, c(id, "cluster"), with = FALSE]
  df_clust[, cluster_label := paste0("Cluster ", cluster, " (N = ", .N, ")"), cluster]

  # merge df_clost to df_long
  df_long <- df_clust[df_long, on = id]

  # store results in list
  r <- list("raw_data" = data,
              "long_data" = df_long,
              "cluster_id" = df_clust,
              "k" = k,
              "method" = method,
              "tree" = cluster_solution)

  # save list as cludur class
  class(r) <- c("list", "clusdur")

  return(r)
}


#' @title plot.clusdur
#'
#' @description Provides an overview table for the time and scope conditions of
#'     a data set
#'
#' @param fit A clusdur object
#'
#' @return A ggplot object`
#' @examples
#' # load simulated data
#' data(dur_sim)
#'
#' # fit cluster analysis
#' fit <- clusdur(data = dur_sim, id = "id", cols = 2:51, method = "ward.D2", k = 2)
#'
#' # plot distribution across deciles
#' plot(fit)
#' @export
#' @import data.table
#' @import ggplot2
#' @importFrom scales pretty_breaks
#' @importFrom grDevices grey.colors


plot.clusdur <- function(fit){

  # check if fit is a clusdur object
  if(!is(fit, "clusdur")){
    stop("fit is not a clusdur object")
  }

  ggplot2::ggplot(data = fit$long_data,
       aes(fill = duration_decile, x = item_order)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("black", grDevices::grey.colors(9), "white")) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  labs(y = "Proportion", x = "Order of screens", fill = "Decile") +
  facet_wrap(~cluster_label, scales = "free_y", nrow = 4) +
  theme(legend.position = "right")
}



#' @title summary.clusdur
#'
#' @description Provides an overview table for the time and scope conditions of
#'     a data set
#'
#' @param dat A data set object
#' @param id Scope (e.g., country codes or individual IDs)
#' @param time Time (e.g., time periods are given by years, months, ...)
#'
#' @return A data frame object that contains a summary of a sample that
#'     can later be converted to a TeX output using `overview_print`
#' @examples
#' # load simulated data
#' data(dur_sim)
#'
#' # fit cluster analysis
#' fit <- clusdur(data = dur_sim, id = "id", cols = 2:51, method = "ward.D2", k = 2)
#'
#' # print summary of cluster analysis
#' summary(fit)
#' @export
#' @import data.table


summary.clusdur <- function(fit){

  # check if fit is a clusdur object
  if(!is(fit, "clusdur")){
    stop("fit is not a clusdur object")
  }

  n_obs <- nrow(fit$cluster_id)
  n_vars <- length(unique(fit$long_data$item))
  k <- fit$k
  method <- fit$method
  cluster_size <- fit$cluster_id[, .N, cluster]
  mean_rank <- fit$long_data[, mean(duration_rel), cluster]
  setnames(mean_rank, 2, "Average rank")

  out <- list("n_obs" = n_obs,
              "n_vars" = n_vars,
              "k" = k,
              "method" = method,
              "cluster_size" = cluster_size,
              "mean_rank" = mean_rank)

  class(out) <- "summary.clusdur"

  return(out)
}


#' @title print.summary.clusdur
#'
#' @description Provides an overview table for the time and scope conditions of
#'     a data set
#'
#' @param dat A data set object
#' @param id Scope (e.g., country codes or individual IDs)
#' @param time Time (e.g., time periods are given by years, months, ...)
#'
#' @return A data frame object that contains a summary of a sample that
#'     can later be converted to a TeX output using `overview_print`
#' @examples
#' # load simulated data
#' data(dur_sim)
#'
#' # fit cluster analysis
#' fit <- clusdur(data = dur_sim, id = "id", cols = 2:51, method = "ward.D2", k = 2)
#'
#' # print summary of cluster analysis
#' summary(fit)
#' @export
#' @import data.table


print.summary.clusdur <- function(summary.clusdur){

  cat("\n")
  cat("Cluster analysis of screen durations")
  cat("\n\n")
  cat("Number of observations: ", summary.clusdur$n_obs)
  cat("\n")
  cat("Number of items: ", summary.clusdur$n_vars)
  cat("\n")
  cat("Number of clusters: ", summary.clusdur$k)
  cat("\n\n")
  cat("Clustering method: ", summary.clusdur$method)
  cat("\n\n")
  cat("Cluster sizes")
  cat("\n")
  print(summary.clusdur$cluster_size, row.names = FALSE)
  cat("\n\n")
  cat("Average relative rank within clusters:")
  cat("\n")
  print(summary.clusdur$mean_rank, row.names = FALSE)
}
