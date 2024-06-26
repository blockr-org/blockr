% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/block-core.R
\name{evaluate_block}
\alias{evaluate_block}
\alias{evaluate_block.data_block}
\alias{evaluate_block.block}
\alias{evaluate_block.plot_layer_block}
\title{Evaluate a block generic}
\usage{
evaluate_block(x, ...)

\method{evaluate_block}{data_block}(x, ...)

\method{evaluate_block}{block}(x, data, ...)

\method{evaluate_block}{plot_layer_block}(x, data, ...)
}
\arguments{
\item{x}{An object inheriting from \code{"block"}}

\item{...}{For generic consistency.}

\item{data}{Result from previous block}
}
\description{
Calls \link{generate_code} and evaluate it
in the relevant environment.
}
