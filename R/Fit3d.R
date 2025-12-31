##
## p3d:Fit3d.R
##
## Figure out what this does that's different
## 2018-10-11: Add Mod.vars.stanfit

#' @export
Fit3d <- function (fit, perm = c(1,3,2),
                   names.vars = pars$names,
                   other.vars = NULL,
                   grid.lines = 26,
                   col = "blue",
                   fill = TRUE,
                   grid = TRUE,
                   base.grid = FALSE,
                   col.grid = col,
                   col.res = col,
                   residuals = FALSE,
                   xlim = c(bbox[1],
                            bbox[2]),
                   zlim = c(bbox[5], bbox[6]),
                   verbose = 0,
                   type = "response",
                   alpha = 0.5,
                   lit = FALSE,
                   FUN = function(x)
                     x,
                   ...)
{
  # Fixed change in rgl_surface problem requiring different permutation of
  # x,y,z, i.e. vertical comes last.  GM --2023-09-04

  Mod.vars <- function(fit)
    UseMethod("Mod.vars")
  Mod.vars.function <- function(fit)
    names(formals(fit))
  Mod.vars.lme <-
    function(fit)
      rownames(attr(terms(fit), "factors"))
  Mod.vars.stanfit <- function(fit)
    names(fit)
  Mod.vars.default <- function(fit)
    names(model.frame(fit))
  mod.vars <- Mod.vars(fit)



  PP <- function(fit, ...)
    UseMethod("PP")
  PP.function <- function(fit, ...)
    Evalf(fit, ...)
  PP.lme <- function(fit, ...)
    predict(fit, ..., level = 0)
  PP.glm <- function(fit, ...) {
    if (type == "response")
      return(predict(fit, ..., type = "response"))
    if (type == "link")
      return(predict(fit, ..., type = "link"))
  }
  PP.default <- function(fit, ...)
    predict(fit, ...)
  Levels <- function(x)
    if (is.factor(x))
      levels(x)
  else
    unique(x)
  pars <- p3d:::Plot3d_par()
  if (missing(col))
    col <- pars$col
  if (verbose > 1)
    disp(col)
  if (verbose > 1)
    disp(mod.vars)
  use.groups <- names.vars["g"] %in% mod.vars
  has.groups <- !(is.na(names.vars["g"]))
  if (verbose > 1)
    disp(use.groups)
  if (verbose > 1)
    disp(has.groups)
  bbox <- par3d("bbox")
  xvals <- seq(xlim[1], xlim[2], length.out = grid.lines)
  zvals <- seq(zlim[1], zlim[2], length.out = grid.lines)
  if (use.groups) {
    g.levs <- Levels(pars$data[[pars$names["g"]]])
    col <- rep(col, length.out = length(g.levs))
    col.grid <- rep(col.grid, length.out = length(g.levs))
    pred <- expand.grid(x = xvals, z = zvals, g = g.levs)
    names(pred) <- names.vars[c("x", "z", "g")]
    if (!is.null(other.vars)) {
      for (i in 1:length(other.vars)) {
        pred[[names(other.vars)[i]]] <- other.vars[[i]]
      }
    }
    yhats <- array(FUN(PP(fit, newdata = pred)), dim = c(grid.lines,
                                                         grid.lines, length(g.levs)))
  }
  else {
    pred <- expand.grid(x = xvals, z = zvals)
    names(pred) <- names.vars[c("x", "z")]
    if (!is.null(other.vars)) {
      for (i in 1:length(other.vars)) {
        pred[[names(other.vars)[i]]] <- other.vars[[i]]
      }
    }
    yhats <- array(FUN(PP(fit, newdata = pred)), dim = c(grid.lines,
                                                         grid.lines, 1))
  }
  for (gi in 1:(dim(yhats)[3])) {
    yhat <- yhats[, , gi]
    if (base.grid == TRUE)
      yhat[] <- bbox[3]
    xyz <- list(xvals,yhat,zvals)
    arg1 <- xyz[[perm[1]]]
    arg2 <- xyz[[perm[2]]]
    arg3 <- xyz[[perm[3]]]
    if (fill)
      p3d:::rgl_surface(
        arg1,
        arg2,
        arg3,
        # xvals,
        # yhat,                    # trying a change in the order
        # zvals,
        color = col[gi],
        alpha = alpha,
        lit = lit,
        ...
      )
    if (grid)
      p3d:::rgl_surface(
        arg1,
        arg2,
        arg3,
        # xvals,
        # yhat,                    # trying a change in the order
        # zvals,
        color = col.grid[gi],
        alpha = alpha,
        lit = lit,
        front = "lines",
        back = "lines",
        ...
      )
  }
  if (residuals) {
    mf <- pars$data
    fitted <- PP(fit, newdata = mf)
    yy <- mf[[names.vars["y"]]]
    xx <- mf[[names.vars["x"]]]
    zz <- mf[[names.vars["z"]]]
    if (has.groups) {
      gg <- mf[[names.vars["g"]]]
      if (is.null(gg))
        gg <- 1
    }
    if (base.grid == TRUE)
      fitted <- 0 * fitted + bbox[3]
    if (has.groups) {
      lines3d(
        as.vector(rbind(xx, xx, NA)),
        as.vector(rbind(yy,
                        fitted, NA)),
        as.vector(rbind(zz, zz, NA)),
        color = rep(col[as.factor(gg)],
                    each = 3),
        col = rep(col[as.factor(gg)], each = 3)
      )
    }
    else {
      lines3d(
        as.vector(rbind(xx, xx)),
        as.vector(rbind(yy,
                        fitted)),
        as.vector(rbind(zz, zz)),
        color = rep(col,
                    each = 2),
        col = rep(col, each = 2)
      )
    }
  }
  if (verbose > 0 && !is.function(fit))
    summary(fit)
  if (verbose > 0 && has.groups) {
    data.frame(xx, yy, zz, fitted, gg, col[as.factor(gg)])
  }
}

if(FALSE){
library(p3d)
  x <- 1:10
  z <- seq(10,100,10)

    dd <- expand.grid(x= x, z = z)
    y <- with(dd, 2*x +z)
    dd$y <- y
  y <- array(y, dim = c(length(x), length(z)))
  Plot3d(y ~ x + z, dd)
  fit <- lm(y ~ x + z, dd)
  Fit3d(fit, perm = c(2,3,1))
  Axes3d()
  dim(y)
  surface3d(x,y,z)

    dd$y <- with(dd, x*w + z/5 +rnorm(x))
  Init3d()
  fit <- lm(y ~ x*z*w, dd)
  Axes3d()
  Fit3d(fit, perm = c(1,2,3))
  Fit3d(fit, perm = c(1,3,2))
  Fit3d(fit, perm = c(3,1,2))

}
