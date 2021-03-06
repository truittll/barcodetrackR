% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/barcode_stacked.R
\name{barcode_stacked}
\alias{barcode_stacked}
\title{Barcode Stacked area plot}
\usage{
barcode_stacked(your_data, samples, time, n_clones, title = NULL,
  colors = FALSE, mode = "PROP", n = 9)
}
\arguments{
\item{your_data}{A data frame. Usually individual barcodes in rows and samples in columns.}

\item{samples}{A vector of the sample names you wish to include, in time order.}

\item{time}{The numeric time points associated with samples, in ascending order.}

\item{n_clones}{The number of clones to display.}

\item{title}{The title.}

\item{colors}{The color palette of the figure. Default is from the viridis package.}

\item{mode}{The type of stacked area plot. "PROP" is a proportioned stacked area plot, displaying the proportion of each barcode at the time points. "Plain" displays the pure counts of each barcode.}

\item{n}{n is only relevant if using a non default color palette and using more clones than the size of the palette. If n_clones is greater than the size of the palette, n should be the max size of the palette.}
}
\value{
Displays stacked area plot in the current plot window.
}
\description{
Creates a stacked area plot over time for the top clones in your data set.
}
\examples{
barcode_stacked(jd76,c("jd76_1m_grans.fastq","jd76_2m_grans.fastq","jd76_3m_grans.fastq"),c(1,2,3),20)
}
