#' @title clusdur
#'
#' @description Provides an overview table for the time and scope conditions of
#'     a data set
#'
#' @param data A data set object
#' @param id Scope (e.g., country codes or individual IDs)
#' @param cols Time (e.g., time periods are given by years, months, ...)
#' @param method The linkage method (one of "complete", "ward.D2", "average)
#' @param k Number of clusters
#'
#' @return A clusdur object
#' @examples
#' data(toydata)
#' output_table <- overview_tab(dat = toydata, id = ccode, time = year)
#' @export
#' @import data.table
#' @importFrom stats cutree


clusdur <- function(data, id, cols, method = "ward.D2", k = 2){

  df_long <- wide_to_long(data, id, cols)

  df_wide <- long_to_wide(df_long, id)

  cluster_solution <- fastcluster::hclust(dist(df_wide[, 2:ncol(df_wide)]), method = method)

  df_clust <- df_wide[, cluster := stats::cutree(cluster_solution, k = k)][, c(id, "cluster"), with = FALSE]

  df_clust[, cluster_label := paste0("Cluster ", cluster, " (N = ", .N, ")"), cluster]

  df_long <- df_clust[df_long, on = id]

  r <- list("raw_data" = data,
            "long_data" = df_long,
            "cluster_id" = df_clust,
            "k" = k,
            "method" = method)

  class(r) <- c("list", "clusdur")

  return(r)
}


#' @title plot.clusdur
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
#' @import ggplot2


plot.clusdur <- function(fit){

  ggplot2::ggplot(data = fit$long_data,
       aes(fill = duration_decile, x = item_order)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("black", grey.colors(9), "white")) +
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
#'     can later be converted to a TeX output using \code{overview_print}
#' @examples
#' data(toydata)
#' output_table <- overview_tab(dat = toydata, id = ccode, time = year)
#' @export
#' @import data.table


summary.clusdur <- function(fit){

  n_obs <- nrow(fit$df_clust)
  n_vars <- length(unique(fit$long_data$item))
  k <- fit$k
  method <- fit$method
  cluster_size <- fit$cluster_id[, .N, cluster]
  mean_rank <- fit$long_data[, mean(duration_rel), cluster]

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
#'     can later be converted to a TeX output using \code{overview_print}
#' @examples
#' data(toydata)
#' output_table <- overview_tab(dat = toydata, id = ccode, time = year)
#' @export
#' @import data.table


print.summary.clusdur <- function(x){

  cat("\n")
  cat("Sequence-cluster analysis of item latencies")
  cat("\n\n")
  cat("Number of observations: ", x$n_obs)
  cat("\n")
  cat("Number of items: ", x$n_vars)
  cat("\n")
  cat("Number of clusters: ", x$k)
  cat("\n\n")
  cat("Clustering method: ", x$method)
  cat("\n\n")
  cat("Cluster sizes")
  cat("\n")
  print(x$cluster_size, row.names = FALSE)
  cat("\n\n")
  cat("Average relative rank within clusters:")
  cat("\n")
  print(x$mean_rank, row.names = FALSE)
}
