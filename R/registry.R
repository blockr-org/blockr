#' Block registry
#'
#' List available blocks.
#'
#' @export
available_blocks <- function() {
  lapply(set_names(nm = list_blocks()), get_block_descr)
}

block_descr_getter <- function(field) {

  field <- force(field)

  function(x) {

    stopifnot(inherits(x, "block_descr"))

    attr(x, field)
  }
}

block_descrs_getter <- function(descr_getter, ptype = character(1L)) {

  getter <- force(descr_getter)

  function(blocks = available_blocks()) {

    if (inherits(blocks, "block_descr")) {
      blocks <- list(blocks)
    }

    vapply(blocks, descr_getter, ptype)
  }
}

#' @param blocks Block description object(s)
#' @rdname available_blocks
#' @export
block_name <- block_descrs_getter(block_descr_getter("name"))

#' @rdname available_blocks
#' @export
block_descr <- block_descrs_getter(block_descr_getter("description"))

new_block_descr <- function(constructor, name, description, classes, input,
                            output, pkg) {

  stopifnot(
    is.function(constructor), is_string(name), is_string(description),
    is.character(classes), length(classes) >= 1L,
    is_string(input), is_string(output), is_string(pkg)
  )

  structure(
    constructor, name = name, description = description, classes = classes,
    input = input, output = output, package = pkg, class = "block_descr"
  )
}

block_registry <- new.env()

#' @param constructor Block constructor
#' @param name,description Metadata describing the block
#' @param classes Block classes
#' @param input,output Object types the block consumes and produces
#' @param package Package where block is defined
#'
#' @rdname available_blocks
#' @export
register_block <- function(constructor, name, description, classes, input,
                           output, package = NA_character_) {

  descr <- new_block_descr(constructor, name, description, classes, input,
                           output, package)

  id <- classes[1L]

  if (id %in% list_blocks()) {
    warning("block ", id, " already exists and will be overwritten.")
  }

  assign(id, descr, envir = block_registry)
}

#' @param ... Forwarded to `register_block()`
#' @rdname available_blocks
#' @export
register_blocks <- function(constructor, name, description, classes, input,
                            output, package = NA_character_) {

  if (length(constructor) == 1L && !is.list(classes)) {
    classes <- list(classes)
  }

  Map(
    register_block,
    constructor = constructor,
    name = name,
    description = description,
    classes = classes,
    input = input,
    output = output,
    package = package
  )
}

list_blocks <- function() {
  ls(envir = block_registry)
}

get_block_descr <- function(id) {
  res <- get(id, envir = block_registry, inherits = FALSE)
  stopifnot(inherits(res, "block_descr"))
  res
}

#' @param ids Character vector of block IDs (first entry in class attribute)
#' @rdname available_blocks
#' @export
unregister_blocks <- function(ids = NULL, package = NULL) {

  if (is.null(ids) && is.null(package)) {

    ids <- list_blocks()

  } else if (not_null(package)) {

    stopifnot(is_string(package))

    pkgs <- eapply(block_registry, `attr`, "package")

    if (not_null(ids)) {
      pkgs <- pkgs[ids]
    }

    ids <- names(pkgs)[lgl_ply(pkgs, identical, package)]
  }

  rm(list = ids, envir = block_registry, inherits = FALSE)
}

register_blockr_blocks <- function(pkg) {

  if (missing(pkg)) {
    pkg <- pkg_name()
  }

  register_blocks(
    constructor = c(
      data_block, filter_block, select_block, summarize_block
    ),
    name = c(
      "data block", "filter block", "select block", "summarize block"
    ),
    description = c(
      "choose a dataset",
      "filter rows in a table",
      "select columns in a table",
      "summarize data groups"
    ),
    classes = list(
      c("dataset_block", "data_block"),
      c("filter_block", "transform_block", "submit_block"),
      c("select_block", "transform_block"),
      c("summarize_block", "transform_block", "submit_block")
    ),
    input = c(NA_character_, "data.frame", "data.frame", "data.frame"),
    output = c("data.frame", "data.frame", "data.frame", "data.frame"),
    package = pkg
  )
}

#' @param block Block name or description object
#' @rdname available_blocks
#' @export
construct_block <- function(block, ...) {

  if (is_string(block)) {
    block <- get_block_descr(block)
  }

  stopifnot(inherits(block, "block_descr"))

  block(...)
}


# Define a function to parse block descriptions and create a table
# example:  parse_block_descriptions(available_blocks())
parse_block_descriptions <- function(block_descriptions) {
  # Create an empty data frame to hold the extracted information
  blocks_info <- data.frame(
    Name = character(),
    Description = character(),
    Classes = character(),
    Input = character(),
    Output = character(),
    Package = character(),
    stringsAsFactors = FALSE
  )
  
  # Iterate over each block description and extract information
  for (block in names(block_descriptions)) {
    block_info <- block_descriptions[[block]]
    new_row <- data.frame(
      Name = attr(block_info, "name"),
      Description = attr(block_info, "description"),
      Classes = paste(attr(block_info, "classes"), collapse = ", "),
      Input = attr(block_info, "input"),
      Output = attr(block_info, "output"),
      Package = attr(block_info, "package"),
      stringsAsFactors = FALSE
    )
    blocks_info <- rbind(blocks_info, new_row)
  }
  
  return(blocks_info)
}
