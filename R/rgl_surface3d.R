#' Graph a surface like rgl::rgl.surface
#'
#' Replacement for rgl::rgl.surface since it is now deprecated and soon to
#' be removed from the rgl package, but rgl's replacement [rgl::surface3d()]
#' handles the order of arguments differently.
#'
#' @export
rgl_surface <- function (x, z, y, coords = 1:3, ..., normal_x = NULL, normal_y = NULL,
          normal_z = NULL, texture_s = NULL, texture_t = NULL)
{
  # .Deprecated("rgl_surface")
  # loadNamespace('rgl')
  rgl.material0(...)
  flags <- rep(FALSE, 4)
  if (is.matrix(x)) {
    nx <- nrow(x)
    flags[1] <- TRUE
    if (!identical(dim(x), dim(y)))
      stop(gettextf("Bad dimension for %s", "rows"), domain = NA)
  }
  else nx <- length(x)
  if (is.matrix(z)) {
    nz <- ncol(z)
    flags[2] <- TRUE
  }
  else nz <- length(z)
  if (is.matrix(y)) {
    if (any(dim(y) != c(nx, nz)))
      stop(gettextf("Bad dimension for %s", "y"))
  }
  else if (is.matrix(x)) {
    if (length(y) != nx)
      stop(gettextf("Bad length for %s", "y"))
    y <- matrix(y, nx, nz)
  }
  else y <- matrix(y, nx, nz, byrow = TRUE)
  ny <- length(y)
  if (nx * nz != ny)
    stop("'y' length != 'x' rows * 'z' cols")
  if (nx < 2)
    stop("rows < 2")
  if (nz < 2)
    stop("cols < 2")
  if (length(coords) != 3 || !identical(all.equal(sort(coords),
                                                  1:3), TRUE))
    stop("'coords' must be a permutation of 1:3")
  nulls <- c(is.null(normal_x), is.null(normal_y), is.null(normal_z))
  if (!all(nulls)) {
    if (any(nulls))
      stop("All normals must be supplied")
    if (!identical(dim(y), dim(normal_x)) || !identical(dim(y),
                                                        dim(normal_y)) || !identical(dim(y), dim(normal_z)))
      stop(gettextf("Bad dimension for %s", "normals"),
           domain = NA)
    flags[3] <- TRUE
  }
  nulls <- c(is.null(texture_s), is.null(texture_t))
  if (!all(nulls)) {
    if (any(nulls))
      stop("Both texture coordinates must be supplied")
    if (!identical(dim(y), dim(texture_s)) || !identical(dim(y),
                                                         dim(texture_t)))
      stop(gettextf("Bad dimension for %s", "textures"),
           domain = NA)
    flags[4] <- TRUE
  }
  idata <- as.integer(c(nx, nz))
  xdecreasing <- diff(x[!is.na(x)][1:2]) < 0
  zdecreasing <- diff(z[!is.na(z)][1:2]) < 0
  parity <- (rgl:::perm_parity(coords) + xdecreasing + zdecreasing)%%2
  if (is.na(parity))
    parity <- 0
  ret <- .C(rgl_surface, success = as.integer(FALSE), idata,
            as.numeric(x), as.numeric(z), as.numeric(y), as.numeric(normal_x),
            as.numeric(normal_z), as.numeric(normal_y), as.numeric(texture_s),
            as.numeric(texture_t), as.integer(coords), as.integer(parity),
            as.integer(flags), NAOK = TRUE)
  if (!ret$success)
    stop("'rgl_surface' failed")
  lowlevel(ret$success)
}
environment(rgl_surface) <- asNamespace('rgl')

