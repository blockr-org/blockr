

# reimplement mutate_block using keyvalue_field
admiral_expr <- function(by_vars = NULL, new_vars = NULL) {
  if (is.null(by_vars)) {
    return(quote(admiral::derive_vars_merged()))
  }
  stopifnot(inherits(by_vars, "character"))

  parse_one <- function(text) {
    expr <- try(parse(text = text))
    if (inherits(expr, "try-error")) {
      expr <- expression()
    }
    expr
  }

  exprs_by_vars <- do.call(c, lapply(by_vars, parse_one))
  exprs_new_vars <- do.call(c, lapply(new_vars, parse_one))

  bquote(
    admiral::derive_vars_merged(by_vars = exprs(..(exprs))),
    list(exprs = exprs),
    splice = TRUE
  )
}


new_admiral_block <- function(data, by_vars = NULL, new_vars = NULL,...) {
  fields <- list(
    by_vars = new_keyvalue_field(
      value = by_vars,
      title = "Grouping variables",
      descr = "The input dataset and the selected observations from the additional dataset are merged by the specified variables."
    ),
    new_vars = new_keyvalue_field(
      value = new_vars,
      title = "Variables to add",
      descr = "The specified variables from the additional dataset are added to the output dataset. "
    ),
    errors_toggle = new_switch_field(
      value = FALSE,
      title = "Some Boolean",
      descr = "The specified variables from the additional dataset are added to the output dataset. "
    ),
    expression = new_hidden_field(admiral_expr)
  )

  new_block(
    fields = fields,
    expr = quote(.(expression)),
    ...,
    class = c("admiral_block", "transform_block")
  )
}

#' @rdname new_block
#' @export
admiral_block <- function(data, ...) {
  initialize_block(new_admiral_block(data, ...), data)
}

