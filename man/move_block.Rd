% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stack.R
\name{move_block}
\alias{move_block}
\title{Move blocks within a stack}
\usage{
move_block(stack, from, to)
}
\arguments{
\item{stack}{stack to update. See \link{new_stack}.}

\item{from}{Initial block position.}

\item{to}{New block position. The block at the new position
will take the old position.}
}
\value{
Invisibly returns the stack.
}
\description{
This is to be called oustide the stack by
other modules.
}
