% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blocks.R
\name{new_summarize_block}
\alias{new_summarize_block}
\alias{summarize_block}
\title{Summarize block}
\usage{
new_summarize_block(
  data,
  func = c("mean", "se"),
  default_columns = character(),
  ...
)

summarize_block(data, ...)
}
\arguments{
\item{data}{Tabular data in which to select some columns.}

\item{func}{Summarize function to apply.}

\item{default_columns}{If you know in advance each function to apply,
you can also pass predefined selected column for each summary.
Therefore when not of length 0, columns should have the same length
as func.}

\item{...}{Further (metadata) attributes}
}
\description{
This block provides access to \link[dplyr]{summarize} verb and
returns a dataframe with the transformed columns.
}
