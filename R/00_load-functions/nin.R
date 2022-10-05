`%nin%` <- function(x, table){
  #' Logical operation for not in
  #' @return logical outcome of not in operation.
  #' @param x vector or NULL: the values to be matched. Long vectors are supported.
  #' @param table vector or NULL: the values to be matched against. Long vectors are not supported.
  #' @examples
  #' c(1,3,11) %nin% 1:10
  is.na(match(x, table, nomatch=NA_integer_))
}