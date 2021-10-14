#' @title
#' Take 1
#'
#' @description
#' Generate a list of indices
#'
#' @param len a length value
#'
#' @return a vector of indices
#'
#' @export
#'
take1 <- function(len) {
  out <- c()
  i <- 0
  while (i < len){
    out <- c(out, i + sample(1:2))
    i <- i + 2
  }
  out <- out[1:len]
  return(out[seq(1, len, 2)])
}
