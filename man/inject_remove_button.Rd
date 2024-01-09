% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ui.R
\name{inject_remove_button}
\alias{inject_remove_button}
\title{Inject remove button into block header}
\usage{
inject_remove_button(ns, b, .hidden = !getOption("BLOCKR_DEV", FALSE))
}
\arguments{
\item{ns}{Stack namespace.}

\item{b}{Current block.}

\item{.hidden}{Internal parameter. Default to FALSE}
}
\description{
This has to be called from the stack parent
namespace. This can also be called dynamically when
inserting a new block within a stack.
}
\keyword{internal}