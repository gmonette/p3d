##
## p3d:Points3d.R
## 2011-12-22
##

# merged Points.*.R (MF)


#' @export
Points3d <-
function(x,...) UseMethod("Points3d")

# Points3d.default <-
# 		function(x,...) scat3d( x,..., clear =FALSE)

#' @export
Points3d.formula <-
		function(x,...) Plot3d(x, ... , clear = FALSE)


#' @export
Points3d.default <-
  function( ... )  {
    a <- args3d(...)
    # disp(a)
    xyz <- a$x
    if (nrow(xyz) == 0) return( invisible(0))
    nas <- apply(xyz, 1, function(x) any(is.na(x)))
    inds <- 1:nrow(xyz)
    inds[nas] <- NA
    inds <-  cbind(inds[-length(inds)],inds[-1])
    inds <- na.omit(inds)
    inds <- c(t(inds))
    xyz <- xyz[inds,]
    a$x <- xyz
    # names(a) <- sub('^lwd$','size', names(a))
    # do.call("rgl.lines", a)
    do.call("points3d", a)
  }


#' @export
Points3d_ <-
  function(x,...) UseMethod("Points3d_")

#' @export
Points3d_.default <-
  function( ... )  {
    a <- args3d(...)
    # disp(a)
    xyz <- a$x
    if (nrow(xyz) == 0) return( invisible(0))
    nas <- apply(xyz, 1, function(x) any(is.na(x)))
    inds <- 1:nrow(xyz)
    inds[nas] <- NA
    inds <-  cbind(inds[-length(inds)],inds[-1])
    inds <- na.omit(inds)
    inds <- c(t(inds))
    xyz <- xyz[inds,]
    a$x <- xyz
    # names(a) <- sub('^lwd$','size', names(a))
    # do.call("rgl.lines", a)
    do.call("spheres3d", a)
  }
#' 3d added-variable plots
#'
#' @param model an \code{lm} model
#' @param coef1 a string that is the name of a numeric variable in the model (required)
#' @param coef2 another numeric variable in the model (required)
#' @param categorical an option categorical variable
#' @param data default: \code{model.frame(model)}
#' @param fit whether to show planar fit in added-variable plot (default: TRUE)
#' @param ... arguments passes to \code{\link{Plot3d}}
#' @examples
#' library(p3d)
#' Init3d(cex=1.2)
#' fit <- lm(mpg ~ cyl + gear + wt + hp, mtcars)
#' Avp3d(fit, 'gear','cyl')
#' Avp3d(fit, 'gear','cyl','gear', fit = FALSE)
#' Id3d()
#' @export
Avp3d <- function (model, coef1, coef2, categorical= NULL, data = model.frame(model), fit = TRUE, ...)
{
  # from code by John Fox in car::avPlot3d.lm
  # fit <- match.arg(fit, c("linear", "robust"), several.ok = TRUE)
  D <- data
  coefs <- names(coef(model))
  which1 <- which(coef1 == coefs)
  if (length(which1) == 0)
    stop(coef1, " is not in the model")
  nam1 <- paste0(coef1,'.others')
  which2 <- which(coef2 == coefs)
  if (length(which2) == 0)
    stop(coef2, " is not in the model")
  nam2 <- paste0(coef2,'.others')
  whichcat <- which(categorical == coefs)
  ynam <- insight::find_response(model)
  namy <- paste0(ynam,".others")
  y <- model.response(model.frame(model))
  X <- model.matrix(model)

  fmla3d <- paste0(namy,'~',nam1,"+",nam2)
  if(length(categorical)) fmla3d <- paste0(fmla3d, " | ", categorical)
  # print(fmla3d)
  fmla3d <- as.formula(fmla3d)

  D[namy] <- residuals(lm(y ~ X[, -c(which1,which2)] - 1))
  D[nam1] <-  residuals(lm(X[,which1] ~ X[,-c(which1, which2)] - 1))
  D[nam2] <-  residuals(lm(X[,which2] ~ X[,-c(which1, which2)] - 1))


  Plot3d(fmla3d, D, ...)

  if(fit) {
    fit.fmla <- paste0(namy,' ~ ', nam1, ' + ', nam2)
    fitlin <- lm(fit.fmla, D)
    Fit3d(fitlin)
  }

  invisible(D)
}
