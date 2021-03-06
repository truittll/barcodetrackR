% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/barcode_ggheatmap.R
\name{barcode_ggheatmap}
\alias{barcode_ggheatmap}
\title{barcode_ggheatmap (Barcode Heatmap using ggplot2)}
\usage{
barcode_ggheatmap(your_data, names = colnames(your_data), n_clones = 10,
  your_title = "", grid = TRUE, label_size = 1, dendro = FALSE,
  cellnote_size = 4, printtable = FALSE, table_option = "percents",
  log_transform = TRUE, log_choice = exp(1),
  distance_method = "Euclidean", minkowski_power = 1,
  cellnote_option = "stars", hclust_linkage = "complete",
  row_order = "hierarchical", clusters = 0)
}
\arguments{
\item{your_data}{A data frame. Usually individual barcodes in rows and samples in columns.}

\item{names}{Vector of x axis labels.}

\item{n_clones}{The top 'n' clones to plot.}

\item{your_title}{The title for the plot.}

\item{grid}{Logical. Include a grid or not in the heatmap.}

\item{label_size}{The size of the column labels.}

\item{dendro}{Logical. Whether or not to show row dendrogram for hierarchical clustering.}

\item{cellnote_size}{The numerical size of the cell note labels.}

\item{printtable}{Logical. Prints percent contribution as a table instead of plotting it.}

\item{table_option}{Character. One of "logs", "reads", or "percents" for printing.}

\item{log_transform}{Logical. Log transform data before clustering and plotting.}

\item{log_choice}{Data is log transformed with this log.}

\item{distance_method}{Character. Use summary(proxy::pr_DB) to see all options.}

\item{minkowski_power}{The power of the Minkowski distance (if minkowski is used).}

\item{cellnote_option}{Character. One of "stars", "reads", or "percents"}

\item{hclust_linkage}{Character. One of one of "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).}

\item{row_order}{Character. One of "hierarchical" or "emergence" to organize rows.}

\item{clusters}{How many clusters to cut hierarchical tree into for display when row_order is "hierarchical".}
}
\value{
Displays a heatmap in the current plot window.
}
\description{
Creates a heatmap using the top 'n' rows from each column, using ggplot2!
}
\examples{
BCheatmap(your_data = zh33, names = colnames(zh33), n_clones = 10,
      your_title = "First Time Point", grid = TRUE, columnLabels = 3)
BCheatmap(your_data = zh33, n_clones = 10, printtable = TRUE)
}

