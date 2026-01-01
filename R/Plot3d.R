##
## p3d:Plot3d.R
## 2011-12-22
## 2013-01-01 GM: added na.omit = na.include for global data frame
##            created in Plot3d.formula
##
#' Plot points in 3D
#'
#' Generic function to plot points in 3d
#'
#' @param object To dispatch.
#' @param ... other arguments.
#' @examples
#' library(p3d)
#' library(spida2)
#' library(latticeExtra)
#' data(Smoking)
#' head(Smoking)
#' rownames(Smoking) <- Smoking$Country
#'
#' dd <- na.omit(
#'   subset(Smoking,
#'          select = c(LE, CigCon, HealthExpPC,
#'                     Continent))
#'   )
#' Init3d(family = 'serif', cex = 2)
#' Plot3d( LE ~ CigCon + HealthExpPC | Continent, dd)
#' Axes3d()
#' Id3d(pad = 1)
#' Id3d(pad = 1, cex = 2, col = 'black')
#' fitlin <- lm(LE ~ CigCon, dd)
#' Fit3d(fitlin)
#' fitlin2 <- lm(LE ~ CigCon + HealthExpPC, dd)
#' Fit3d(fitlin2, col = 'green')
#' fit <- lm(LE ~ CigCon + log(HealthExpPC) +I(CigCon^2) +
#'     I(log(HealthExpPC)^2) + I(CigCon*log(HealthExpPC)),
#'     dd)
#' Fit3d(fit , col = 'red')
#' fitl <- lm(LE ~ CigCon + HealthExpPC, dd)
#' # HEpCap is highly 'skewed': dense on left, long tail on right
#' require(lattice)
#' densityplot(dd$HealthExpPC)
#'
#' # Useful to use a transformation to make spread more even
#' #  e.g. log
#' # First make sure all values are positive:
#'
#' sort(dd$HealthExpPC)
#'
#' # Do log transformation:
#'
#' dd$LogHE <- log(dd$HealthExpPC)    # create log HE
#'
#' densityplot( dd$LogHE )
#'
#' # Also useful to have categories:
#'
#' dd$HECat <- cut(dd$LogHE, 7)       # create categories
#' summary(Smoking)
#'
#' Plot3d(LE ~ CigCon + LogHE |HECat, dd)  # condition on level of HEpC
#' Axes3d()
#' Ell3d()
#' Id3d(pad=1)
#'
#' # Simple regression
#'
#' fit.lin <- lm( LE ~ CigCon, Smoking)
#' Fit3d( fit.lin )
#' fit.lin2 <- lm(LE ~ CigCon, subset(Smoking, Continent != "Africa"))
#' Fit3d( fit.lin2)
#' Pop3d(4)
#'
#' # Use multiple regression (advanced version with quadratic surface)
#'
#' fit = lm( LE ~ CigCon + I(LogHE^2) +I(CigCon^2) + I(LogHE^2) + I(CigCon*LogHE), Smoking)
#' Fit3d( fit, col = 'red' )
#' Pop3d(2)
#'
#' # refit omitting Africa:
#'
#' fit.na = lm( LE ~ CigCon + I(LogHE^2) +I(CigCon^2) + I(LogHE^2)
#'              + I(CigCon*LogHE), Smoking, subset = Continent != "Africa")
#' Fit3d( fit.na, col = 'red' )
#'
#' # Marginal relationship
#'
#' Pop3d(4)    # pop twice for each fit
#' fit.quad <- lm( LE ~ CigCon + I(CigCon^2) , Smoking)
#' Fit3d( fit.quad, col = 'green')
#'
#' #  A quadratic surface within each Continent (overfitting?!)
#'
#' fit2 = lm( LE ~ Continent*(CigCon + HealthExpPC +I(CigCon^2) +
#'                              I(HealthExpPC^2) + I(CigCon*HealthExpPC)), Smoking)
#' Fit3d( fit2 )
#'
#' #  For a 2D view of the y versus x
#'
#' view3d(0,0,0)   # note the first parameter, theta, rotates clockwise in the vertical axis
#' # from the initial position given by view3d(0,0,0); the second
#' # parameter, phi, tilts the graph around a horizontal screen axis
#' # towards the viewer.  The third parameter, fov, affects the
#' # amount of 'perspective'. fov = 0 yields an orthographic
#' # projection
#' @export
Plot3d <- function(object, ...)  UseMethod("Plot3d")

#' Maintain a list of parameters for Plot3d
#'
#' @param ... Parameters.
#' @param new Open new window if TRUE. Default: FALSE
#' @export
Plot3d_par <- function(..., new = FALSE){
   # A new version (2010-11-30) of Plot3d_par
   # that maintains separate lists for each rgl window allowing
   # the selection of different windows

  a <- list(...)

  #disp( environment())
  #if ( is.null(names(a))) disp(a)
  #else disp(names(a))

  pos <- rgl.cur()

  if ( !exists(".Plot3d_par",1) || pos ==0) assign(".Plot3d_par",list(),1)  # initialize
  if( pos == 0) pos <- 1
  if ( new ){
    p <- get(".Plot3d_par",1)
    p[[pos]] <- list()
    assign(".Plot3d_par",p,1)
  }
  if ( length( p <- get(".Plot3d_par",1)) < pos){
     p <- get(".Plot3d_par",1)
    p[[pos]] <- list()
    assign(".Plot3d_par",p,1)
  } #.Plot3d_par[[pos]] <<- list()
  if ( length(a) == 0) return(get(".Plot3d_par",1)[[pos]])
  if ( !is.null(names(a))) {
      p <- get(".Plot3d_par",1)
      p[[pos]][names(a)] <- a
      assign(".Plot3d_par",p,1)

      ret <- p[[pos]]
   }
   else {
  ret <- get(".Plot3d_par",1)[[pos]][[unlist(a)]]
   }
 ret
 }
#' Plot a fitted model
#'
#' @param fit A fitted object with a \code{model.frame} and
#'        a \code{predict} method whose predictors are
#'        functionally dependent on the variables plotted
#'        along the two axes of a 3D plot and an optional
#'        categorical variable.
#' @param ... Other arguments.
#' @rdname Plot3d
#' @export
Plot3d.lm <-
function( fit , ...) {
    Plot3d(formula(fit), model.frame(fit), ...)
    Fit3d( fit)
}

#' Plot3d S3 method for a formula
#'
#' @param formula A formula of the form \code{y ~ x + z} or
#'        \code{y ~ x + z | w} where \code{y}, \code{x}, \code{z}
#'        are numeric variables and
#'        \code{w} is a categorical variable.
#' @param data A data frame in which \code{formula} is evaluated.
#' @param groups The name of a variable to serve as an alternative
#'        to specifying a \code{w} variable in \code{formula}.
#' @param xlab, ylab, zlab Labels for the corresponding axes.
#' @param verbose Default: FALSE.
#' @param col Colors for levels of the \code{w} variable.
#' @param surface Whether to plot a surface. Default: FALSE.
#' @param fit Default: 'smooth'.
#' @param surface.col Color for surface.
#' @param lines.col Default: 'gray'.
#' @param lines.lwd Default: 1.
#' @param theta, phi, zoom, fov Arguments for \code{view3d}
#'        with defaults 0, 15, 1, 15, respectively.
#' @param keep.view Do not change current view. Default: FALSE.
#' @param ... Other arguments.
#' @rdname Plot3d
#' @export
Plot3d.formula <-
function( formula = attr(data, "formula"),
            data = sys.parent() ,
            groups = NULL,
            subset = NULL ,
            xlab = names(dd)[2],
            ylab = names(dd)[1], zlab = names(dd)[3],  verbose = 0,
            col = c("blue", "#008800", "orange",
                    "magenta", "darkcyan", "red",
                    "darkgray","darkblue",
                    "cyan", "darkorange",
                    "darkmagenta", "cyan", "darkred",
                    "yellowgreen", "purple","gray",
                    "#cc4466",
                    "slategray",
                    # "aliceblue",
                    # "antiquewhite",
                    "coral","purple",
                    # "azure",
                    "pink",
                    "#555555",
                    "whitesmoke"),
            surface = FALSE,
            fit = "smooth",
            surface.col = col,
            lines.col = 'gray',
            lines.lwd = 1,
            theta = 0,       # arguments for view3d
            phi = 15,
            zoom = 1,
            fov = 15,
#            keep.view = missing(phi) && missing(theta) && missing(zoom) && missing(fov),
            keep.view = FALSE,
            ...) {

# BUG: subset and groups might not work together

    par3d()  # seems more reliable if called first??
    p <- par3d()
    Levels <- function( x ) if (is.factor(x)) levels(x) else sort( unique(x))
    env <- environment(formula)
    subset <- eval( substitute(subset), data, env)
    groups <- eval( substitute(groups), data, env)
    if (verbose > 0) disp(subset)
    ff <- as.character( formula )
    if ( verbose > 0) disp( ff )
    if ( length(ff) != 3) stop("Formula should have form y ~ x + z [ |g ]")
    ff[3] <- sub("\\|","+",ff[3])
    if ( verbose > 0) disp( ff )
    fmla <- as.formula( paste( ff[2] ,"~", ff[3]))
#    dd <- model.frame( fmla, data, subset = subset )
    # GM 2013-01-01: added na.action = na.include so data frame
    #    in .Plot3d_par has rows matching original data set so
    # > Id3d(labels = dsm$Country)
    # will work correctly
    dd <- model.frame( fmla, data, subset = subset, na.action = na.include )

    if ( ncol(dd) < 3) stop( "Need at least three variables for Plot3d")
    if ( ncol(dd) > 4) stop( "More than 4 variables not yet implemented")
    if ( ncol(dd) == 3) {
        nams <- names(dd)
        names(nams) <- c('y','x','z')
        size_ <- Plot3d_par('size')
        Plot3d_par( data = dd, names=nams, has.groups = FALSE, col=col[1], new = TRUE)
        Plot3d_par(size = size_)
        # GM 2013-01-02: added '[1]' to col = col
        # to avoid meaningless rainbow
        if ( verbose > 1 ) disp( Plot3d_par() )
        scat3d( as.numeric(dd[[2]]), as.numeric(dd[[1]]), as.numeric(dd[[3]]),
            xlab , ylab, zlab,
            col = col[1],
            surface = surface,
            fit = fit,
            surface.col = surface.col[1],
                # GM 2013-01-02: ditto to above
            ...)

    }
    if ( ncol(dd)  == 4) {
        #if ( any ( isfac <- sapply( dd[,2:3], is.factor)) && !(is.factor( dd[,4]))) {
        #   consider putting categorical in last place
        #}
        if( verbose) disp(ncol(dd))
        nams <- names(dd)
        names(nams) <- c('y','x','z','g')
        # dd <- na.omit(dd)
        # dd[[4]] <- factor(dd[[4]])    #  (GM 2013-01-18)
        # Extend 'col' if necessary: (GM 2013-01-02)
        col <- rep(col, length.out = length(Levels(dd[[4]])))
        Plot3d_par( data = dd, names=nams, has.groups = TRUE, col=col, new = TRUE)
        if ( verbose > 1 ) disp( Plot3d_par() )
        scat3d( as.numeric(dd[[2]]), as.numeric(dd[[1]]), as.numeric(dd[[3]]),
            xlab, ylab, zlab, groups = dd[[4]],
            surface = surface,
            fit = fit,
            surface.col = col, ...)
        if ( verbose > -1 ) {
            pp <- Plot3d_par()
            cats <- data.frame(x=Levels(pp$data[[pp$names['g']]]))
            #disp(cats)
            #disp(col)
            cats$col <- col[1:length(cats$x)]
            names(cats)[1] <- pp$names['g']
            print(cats)
        }
    }
    if( !is.null(groups)) {
      lapply( split( 1:nrow(data), groups), function( ind ) {
              dz <- data[ ind,]
               fmla <- as.formula(paste( sub("\\|","+",as.character(fmla))[c(2,1,3)],collapse =""))
               Lines3d( yxz = model.frame( fmla, dz)[,1:3],col = lines.col,
                  lwd = lines.lwd)
         })
    }
    if ( verbose > 0 ) disp( 'Plot3d -1')
    if ( verbose > 0 )  disp( par3d('scale'))
    if (keep.view) {
      Acos <- function(x) 360*acos(x)/(2*pi)
      Asin <- function(x) 360*asin(x)/(2*pi)
      Atan2 <- function( s, c) atan2( s, c)*(180/pi)
      um <- p$userMatrix

      theta <- Atan2(-um[1,3], um[1,1])
      phi <-  Atan2 (um[3,2],um[2,2])
      view3d(theta = theta, phi = phi, fov = p$FOV, zoom = p$zoom,
        scale = p$scale)
    }   else {
    view3d( theta = theta, phi = phi, fov = fov, zoom = zoom )
    if ( verbose > 0 )   disp( 'Plot3d in else')
    if ( verbose > 0 ) disp( par3d('scale'))
    }
    if( verbose > -1) message("Use left mouse to rotate, middle mouse (or scroll) to zoom, right mouse to change perspective")
}
#' Default S3 method for \code{Plot3d}
#'
#' @param x x-axis (horizontal) variable
#' @param y y-axis (vertical) variable
#' @param x z-axis (depth) variable
#' @param xlab, ylab, zlab, groups Graphical parameters.
#' @param ... other arguments.
#' @rdname Plot3d
#' @export
Plot3d.default <-
function(x, y, z, xlab, ylab, zlab, groups = NULL, ...) {
   help = "
   Plot3d()   by GM Nov 9, 2005
     Modified Feb 7, 2007
   A wrapper for scat3d modified from scatter3d by John Fox
   "
   ####  NOTE: This function needs considerable work
   if ( ! is.null(groups)) stop("groups implemented only in Plot3d.formula")
   if ( is.matrix(x) || is.data.frame(x)) {
      if( ncol (x) < 3) x <- cbind( x , 0, 0)
      if ( is.null( colnames( x ))) {
          colnames(x) <- paste( deparse(substitute(x)), 1:ncol(x))
      }
      if(missing(xlab)) xlab = colnames(x)[1]
      if(missing(ylab)) ylab = colnames(x)[2]
      if(missing(zlab)) zlab = colnames(x)[3]
      dd <- x[,1:3]
      names(dd) <- c(xlab,ylab,zlab)
      data <- data.frame( x[,1], x[,2], x[,3])
      names( data ) <- c(xlab, ylab, zlab)
      # scat3d(x[,1],x[,2],x[,3],xlab=xlab, ylab=ylab, zlab=zlab,...)
   } else {
      data <- data.frame(x, y, z)
      names(data) <- c(deparse(substitute(x)),
        deparse(substitute(y)),
        deparse(substitute(z)))

      # scat3d(x,y,z, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), zlab = deparse(substitute(z)), ... )
   }
   fmla <- as.formula( paste( "`",names(data)[2],"` ~ `", names(data)[1],
        "` + `", names(data)[3], "`", sep = ''))
   Plot3d( fmla, data , ...)
}



