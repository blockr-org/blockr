% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/server.R
\name{init}
\alias{init}
\alias{init.workspace}
\alias{init.stack}
\title{Generic for server initialisation}
\usage{
init(x, ...)

\method{init}{workspace}(x, vals, session, ...)

\method{init}{stack}(x, vals, ...)
}
\arguments{
\item{x}{Object for which to generate a \code{\link[=moduleServer]{moduleServer()}}}

\item{...}{Generic consistency}

\item{vals}{Internal reactive values.}

\item{session}{Shiny session object.}
}
\description{
Handle initialisation of workspace, stacks, ...
}
