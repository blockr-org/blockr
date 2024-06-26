% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blocks.R
\name{new_join_block}
\alias{new_join_block}
\title{Join block}
\usage{
new_join_block(y = NULL, type = character(), by = character(), ...)
}
\arguments{
\item{y}{Second dataset for join.}

\item{type}{Join type.}

\item{by}{Join columns.}

\item{...}{Further (metadata) attributes}
}
\description{
This block provides access to the dplyr join verbs and
returns a dataframe. This blocks is made to work with multiple
stack as the \code{y} parameter expects a dataframe from another stack.
}
