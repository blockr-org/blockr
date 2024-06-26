% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fields.R
\name{new_hidden_field}
\alias{new_hidden_field}
\title{Hidden field constructor}
\usage{
new_hidden_field(value = expression(), ...)
}
\arguments{
\item{value}{Default text input value.}

\item{...}{Other parameters passed to \link{new_field} and may be needed
by \link{ui_input} to pass more options to the related shiny input.}
}
\description{
Hidden field is useful to host complex expression in
a field. See \link{new_filter_block} for a usecase.
}
