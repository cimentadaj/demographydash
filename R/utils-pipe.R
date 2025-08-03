#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' Default value operator
#'
#' This operator returns the left-hand side if it is not NULL,
#' otherwise it returns the right-hand side.
#'
#' @name %||%
#' @rdname default-operator
#' @keywords internal
#' @usage lhs \%||\% rhs
#' @param lhs A value to test for NULL.
#' @param rhs A value to use if lhs is NULL.
#' @return lhs if it is not NULL, otherwise rhs.
`%||%` <- function(lhs, rhs) {
  if (is.null(lhs)) rhs else lhs
}
