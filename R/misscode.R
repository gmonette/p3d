#' Recode missing data for p3d
#'
#' Recodes missing data to a value less than the minimum value -- or to a non-missing
#' code to indicate missingness for a factor -- to facilitate plotting in 3d
#'
#' @param x vector with possible missing values
#' @param offset (default .1) how far below the minimum value as a proportion of the range
#'
#' @export
misscode <- function(x,...) UseMethod('misscode')
#' @rdname misscode
#' @method misscode default
#' @export
misscode.default <- function(x,...,offset = .1) {
  rr <- range(x, na.rm = TRUE)
  vmiss <- min(x,na.rm = TRUE) - offset * diff(rr)
  nas <- is.na(x)
  x[nas] <- vmiss
  attr(x,'nas') <- nas
  x
}
#' @rdname misscode
#' @method misscode factor
#' @export
misscode.factor <- function(x, ...) {
  nas <- is.na(x)
  x <- addNA(x, ifany = TRUE)
  attr(x,'nas') <- nas
  x
}
#' @rdname misscode
#' @method misscode data.frame
#' @export
misscode.data.frame <- function(x,...) {
  x[] <- lapply(x[],misscode,...)
  isna <- lapply( x, function(x) attr(x,'nas'))
  #   disp(isna)
  isna <- do.call(cbind,isna)
  isna <- apply(isna, 1, sum)
  #   disp(isna)
  x$.nmiss <- isna
  x
}
