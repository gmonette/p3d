##
## p3d:Identify3d.R
## 2011-12-22
##


# BUG in Id3d: If you provide 'col' the color used is white


#' @export
Id3d <-
function (select,labels=row.names(data), pad = 1 , ...) {
  if( !missing(select)) {
    if(missing(labels)) {
      return(Flag3d(select, pad = pad, ...))
    } else {
      return(Flag3d(select, pad = pad, labels = labels, ...))
    }
  }
cat("Move display using left mouse button and middle button or scroll\n")
cat("Press right mouse button and drag a rectangle around points to be identified\n")
cat("Click right mouse button without selecting a point to terminate\n")
    pars <- Plot3d.par()

    data <- pars$data
    nams <- pars$names
    if( pad > 0 ) {
        padstring <- do.call(paste, c(as.list(rep(" ", pad)),sep =""))
        labels <- paste( padstring, labels, sep = "")
    }
    if ( pars$has.groups ) {
        ret <- p3d::Identify3d(
            data[[nams['x']]],  data[[nams['y']]],data[[nams['z']]],
            groups = data[[nams['g']]],labels=labels,...)
    } else {
        ret <- p3d::Identify3d(
            data[[nams['x']]],  data[[nams['y']]],data[[nams['z']]],
            labels=labels,...)
    }
    substring(ret,pad+1)
}


#' @export
tf <- function( x, y = 1) {
  if(missing(y)) cat("y is missing\n")
}

#' @export
Identify3d <-
function(x, ... ) {
'
    Use the right mouse button and drag a rectangle to selects points.
    Click and drag empty region to end.
'
    UseMethod("Identify3d")
}

#' @export
Identify3d.formula <-
function( formula = attr(data, "formula"),
            data = sys.parent()  , labels = NULL , adj = 0,  ...) {
    env <- environment(formula)
    labels <- eval(substitute(labels), data, env)
    if ( is.null(labels)) labels <- rownames(data)
    dd <- model.frame( formula, data )
    if ( ncol(dd) < 3) stop( "Need at least three variables for identify3d")
    Identify3d( dd[[2]], dd[[1]], dd[[3]], labels = labels, adj = adj, ...)
}

#' @export
Identify3d.default <-
function (
    x = pars$data[[pars$names['x']]],
    y = pars$data[[pars$names['y']]],
    z = pars$data[[pars$names['z']]],
    labels = rownames(Plot3d.par("data")), groups = NULL,
    col = Plot3d.par()$col ,   # c("blue", "green", "orange", "magenta", "cyan", "red", "yellow", "gray"),
    adj = 0, debug = FALSE, pad = 0,
    offset = ((100/length(x))^(1/3)) * 0.02,
	...) {
    pars <- Plot3d.par()
    stay <- !(is.null(pars$stay)||!pars$stay)
    ## added by GM Nov 9
       if ( ( is.matrix(x) && (ncol(x) > 1) ) || is.data.frame(x) ) {
            dat <- x
            x <- dat[,1]
            y <- dat[,2]
            z <- dat[,3]
            # GM 2012-01-02:
            # labels <- rownames(dat)
            if( missing(labels) ) labels <- rownames(dat)

       }
    select3d <-function (...) {
    # adapted from select3d and rgl.selecte3d in the rgl package, but passes
    #  through arguments to rgl.select
        rgl:::.check3d()
        rect <- rgl:::rgl.select(...)
        llx <- rect[1]
        lly <- rect[2]
        urx <- rect[3]
        ury <- rect[4]
        if (llx > urx) {
            temp <- llx
            llx <- urx
            urx <- temp
            }
        if (lly > ury) {
            temp <- lly
            lly <- ury
            ury <- temp
            }
        proj <- rgl:::rgl.projection()
        function(x, y, z) {
            pixel <- rgl.user2window(x, y, z, proj = proj)
            apply(pixel, 1, function(p) (llx <= p[1]) && (p[1] <=
                urx) && (lly <= p[2]) && (p[2] <= ury) && (0 <= p[3]) &&
                (p[3] <= 1))
            }
        }
    valid <- if (is.null(groups)) complete.cases(x, y, z)
    else complete.cases(x, y, z, groups)
    x <- x[valid]
    y <- y[valid]
    z <- z[valid]
    labels <- labels[valid]
    pad <- paste( rep(" ", pad), collapse = "")
    labels.pad <- paste(pad,labels)
    # x <- (x - min(x))/(max(x) - min(x))
    # y <- (y - min(y))/(max(y) - min(y))
    # z <- (z - min(z))/(max(z) - min(z))
    if (.Platform$OS.type == "windows") rgl.bringtotop(stay=stay)
    identified <- character(0)
    groups <- if (!is.null(groups)) as.numeric(groups[valid])
    if (debug) disp(groups)
    else rep(1, length(x))
    repeat {
        f <- select3d(button="right")
        if ( debug ) disp(x)
        which <- f(x, y, z)
        if (debug) disp(which)
        if (!any(which)) break
        if (!is.null(groups)) cols <- col[groups][which] else cols <- col[1]
        if (debug) disp( col[groups][which] )
        if (debug) disp( col[groups])
        if (debug) disp( labels.pad[which] )
        if (debug) disp( cols )
        text3d(x[which], y[which] + offset, z[which], labels.pad[which],
            color = cols, adj = adj,...)
        identified <- c(identified, labels[which])
        }
    identified
}

# rgl.texts(c(1,2),c(1,3),c(1,2),text=c("AAA","BBB"), color=c('blue','red'))




#' @export
Flag3d <-
  function ( select,
  # adds a label to a Plot3d display to selected points
  # GM 2013-01-02
    x = pars$data[[pars$names['x']]],
    y = pars$data[[pars$names['y']]],
    z = pars$data[[pars$names['z']]],
    labels = rownames(Plot3d.par("data")), groups = NULL,
    col = Plot3d.par()$col ,   # c("blue", "green", "orange", "magenta", "cyan", "red", "yellow", "gray"),
    adj = 0, debug = FALSE, pad = 0,
    offset = ((100/length(x))^(1/3)) * 0.02,
    ...) {
    pars <- Plot3d.par()

    ## added by GM Nov 9
    if ( ( is.matrix(x) && (ncol(x) > 1) ) || is.data.frame(x) ) {
      dat <- x
      x <- dat[,1]
      y <- dat[,2]
      z <- dat[,3]
      if( missing(labels) ) labels <- rownames(dat)
    }
    if ( pars$has.groups && missing(groups)) {
        groups <- pars$data[[pars$names['g']]]
    }
    valid <- if (is.null(groups)) complete.cases(x, y, z)
    else complete.cases(x, y, z, groups)
    x <- x[valid]
    y <- y[valid]
    z <- z[valid]
    labels <- labels[valid]
    pad <- paste( rep(" ", pad), collapse = "")
    labels.pad <- paste(pad,labels)
    # x <- (x - min(x))/(max(x) - min(x))
    # y <- (y - min(y))/(max(y) - min(y))
    # z <- (z - min(z))/(max(z) - min(z))
    rgl.bringtotop()
    groups <- if (!is.null(groups)) as.numeric(groups[valid])

      if ( is.numeric(select) || is.logical(select)) select <- labels[which]
      which <- labels %in% select
      if (!is.null(groups)) cols <- col[groups][which] else cols <- col[1]
#     disp(col)
#     disp(col[groups])
#     disp(cols)
#     disp(labels.pad[which])
      text3d(x[which], y[which] + offset, z[which], labels.pad[which],
                color = cols, adj = adj,...)
    labels[which]
  }

