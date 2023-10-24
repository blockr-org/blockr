#' @rdname new_block
#' @export
new_cheat_block <- function(data, ...) {
  new_block(
    fields = list(
      dummy = new_string_field("dummy")
    ),
    expr = quote({
      dplyr::filter(data, LBTEST == "Hemoglobin") %>%
        dplyr::filter(!startsWith(VISIT, "UNSCHEDULED")) %>%
        dplyr::arrange(VISITNUM) %>%
        dplyr::mutate(VISIT = factor(
          VISIT,
          levels = unique(VISIT),
          ordered = TRUE
        )) %>%
        dplyr::group_by(VISIT, ACTARM) %>%
        dplyr::summarise(
          Mean = mean(LBSTRESN, na.rm = TRUE),
          SE = sd(LBSTRESN, na.rm = TRUE) / sqrt(dplyr::n()),
          .groups = "drop"
        ) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(ymin = Mean - SE, ymax = Mean + SE)
    }),
    ...,
    class = c("cheat_block", "transform_block")
  )
}

#' @rdname new_block
#' @export
cheat_block <- function(data, ...) {
  initialize_block(new_cheat_block(data, ...), data)
}

#' @rdname new_block
#' @param column Column to apply the operation on.
#' @export
new_as_factor_block <- function(data, column = "VISIT", ...) {

  all_cols <- function(data) colnames(data)

  mutate_expr <- function(data, column) {
    if (is.null(column)) return(NULL)
    if (!(column %in% colnames(data))) {
      return(NULL)
    }

    bquote(
      dplyr::mutate(
        VISIT = factor(
          .(column),
          levels = unique(.(column)),
          ordered = TRUE
        )
      ),
      list(column = as.name(column))
    )
  }

  fields <- list(
    column = new_select_field(column, column),
    expression = new_hidden_field(mutate_expr)
  )

  new_block(
    fields = fields,
    expr = quote(.(expression)),
    ...,
    class = c("dummy_block", "transform_block")
  )
}

#' @rdname new_block
#' @export
as_factor_block <- function(data, ...) {
  initialize_block(new_as_factor_block(data, ...), data)
}

#' @rdname new_block
#' @export
data_demo_block <- function(...) {
  initialize_block(
    new_data_block(
      ...,
      dat = as.environment("package:blockr.data"),
      selected = "lab"
    )
  )
}