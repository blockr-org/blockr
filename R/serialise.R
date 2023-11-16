#' Serialise
#' Serialise stacks and blocks.
#' @param x Object to serialise.
#' @param ... Ignored.
#' @export
serialise <- function(x, ...) UseMethod("serialise")

#' @describeIn serialise Serialise a field
#' @method serialise field
#' @export
serialise.field <- function(x, ...) {
  class(x) <- "list"
  x
}

#' @describeIn serialise Serialise a block
#' @method serialise block
#' @export
serialise.block <- function(x, ...) {
  x <- lapply(x, serialise)
  class(x) <- "list"
  x
}

#' @describeIn serialise Serialise a block
#' @method serialise stack
#' @export
serialise.stack <- function(x, ...) {
  as_list(x) |>
    jsonlite::toJSON(auto_unbox = TRUE)
}

#' As list
#' Convert stacks, blocks, and fields to `list`.
#' @param x Object to convert.
#' @param ... Ignored.
#' @export
as_list <- function(x, ...) UseMethod("as_list")

#' @describeIn as_list Convert a field
#' @method as_list field
#' @export
as_list.field <- function(x, ...) {
  class(x) <- "list"
  x
}

#' @describeIn as_list Convert a block
#' @method as_list block
#' @export
as_list.block <- function(x, ...) {
  x <- lapply(x, as_list)
  class(x) <- "list"
  x
}

#' @describeIn as_list Convert a block
#' @method as_list stack
#' @export
as_list.stack <- as_list.block
