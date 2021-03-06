% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ThreadNet_Graphics.R
\name{filter_network_edges}
\alias{filter_network_edges}
\title{filter_network_edges}
\usage{
filter_network_edges(n, threshold)
}
\arguments{
\item{n}{network list of nodeDF and edgeDF}

\item{threshold}{numeric threshold for filtering edges.}
}
\value{
n network list of nodeDF and edgeDF
}
\description{
Filters out network edges with weight below the threshold
}
