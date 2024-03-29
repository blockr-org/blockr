% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blocks.R
\name{new_head_block}
\alias{new_head_block}
\alias{head_block}
\title{Head block}
\usage{
new_head_block(data, n_rows = numeric(), n_rows_min = 1L, ...)

head_block(data, ...)
}
\arguments{
\item{data}{Tabular data in which to select some columns.}

\item{n_rows}{Number of rows to return.}

\item{n_rows_min}{Minimum number of rows.}

\item{...}{Further (metadata) attributes}
}
\description{
This allows to select the first n rows of the input dataframe.
}
