% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/block-core.R
\name{initialize_block}
\alias{initialize_block}
\alias{initialize_block.data_block}
\alias{initialize_block.block}
\title{Initialize block generic}
\usage{
initialize_block(x, ...)

\method{initialize_block}{data_block}(x, ...)

\method{initialize_block}{block}(x, data, ...)
}
\arguments{
\item{x}{An object inheriting from \code{"block"}}

\item{...}{For generic consistency.}

\item{data}{Result from previous block}
}
\value{
The block element.
}
\description{
Initializes all fields composing the block.
}
\seealso{
See \link{initialize_field}.
}
