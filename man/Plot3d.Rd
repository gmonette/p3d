\name{Plot3d}
\alias{Plot3d}
\alias{Plot3d.default}
\alias{Plot3d.lm}
\alias{Plot3d.formula}
\title{
Plot3d generates a 3-dimensional plot using the package \code{rgl}.
%%   ~~ data name/kind ... ~~
}
\description{
Plot3d generates a 3-dimensional plot of points using the package \code{rgl}.
A grid of points for the x and z variables and an optional factor variable
is kept as an object in order to
allowing plotting fitted surfaces that are functions of the x and z variables and the
optional factor.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{

## S3 method for class 'formula'

Plot3d(formula = attr(data, "formula"),
    data = sys.parent(),
    groups = NULL,
    subset = NULL,
    xlab = names(dd)[2],
    ylab = names(dd)[1],
    zlab = names(dd)[3],
    verbose = 0,
    col = c("blue", "green", "orange", "magenta", "cyan", "red", "yellow", "gray"),
    surface = FALSE,
    fit = "smooth",
    surface.col = col,
    lines.col = "gray",
    lines.lwd = 1,
    theta = 0,
    phi = 15,
    fov = 15,
    zoom = 1,
    keep.view = FALSE,
    ...)

## S3 method for class 'lm'

  Plot3d(fit, ...)

## S3 default method

  Plot3d(x, y, z, xlab, ylab, zlab, groups = NULL, ...)
}
\arguments{
  \item{formula}{ a two sided formula of the form \code{y ~ x + z | [g] } where
  \code{y} is the variable plotted on the vertical axis, \code{x} is plotted on
  the horizontal axis and \code{z} is plotted along the axis coming out of the
  screen. \code{g} is interpreted as a factor and the index of its levels is
  used to select the value of \code{col} used.
%%     ~~Describe \code{formula} here~~
}
  \item{data}{
  a data frame in which \code{formula} is evaluated.
%%     ~~Describe \code{data} here~~
}
  \item{groups}{ a variable used
   to join points belonging to the same group by lines
%%     ~~Describe \code{groups} here~~
}
  \item{subset}{  is a condition to select a subset of
  \code{data}.
%%     ~~Describe \code{subset} here~~
}
  \item{xlab, ylab, zlab}{
  labels for respective axes
%%     ~~Describe \code{xlab} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{surface}{
  if TRUE, a non-parametric regression surface(s) are drawn. 
%%     ~~Describe \code{surface} here~~
}
  \item{fit}{
%%     ~~Describe \code{fit} here~~
}
  \item{surface.col}{
%%     ~~Describe \code{surface.col} here~~
}
  \item{lines.col}{
%%     ~~Describe \code{lines.col} here~~
}
  \item{lines.lwd}{
%%     ~~Describe \code{lines.lwd} here~~
}
  \item{theta, phi, fov, zoom}{
    are arguments passed to \code{view3d} for the initial view. 
    \code{theta} is the amount of rotation of the object aroud a vertical axis in degrees clockwise as seen from above, 
    \code{phi} is the elevation of the eye in degrees above a horizontal plane. By default, rotation maintains the verticality of the vertical axis to respect the way in which regression is normally visualized. \code{fov = 0} produces an orthogonal projection with no perspective distortion. However, the impression of seeing data in 3D may be difficult to evoke. Thus a frontal orthographic projection is achieved with \code{phi = 0, fov = 0, theta = 0}.%%     ~~Describe \code{lines.lwd} here~~
}
  \item{keep.view}{
    if TRUE, the previous point of view is maintained. 
%%     ~~Describe \code{lines.lwd} here~~
}

  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
%\source{
%%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
%\references{
%%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
\dontrun{
    library(p3d)
    data(Smoking)
    head(Smoking)
    rownames(Smoking) = Smoking$Country

    Init3d(family = 'serif', cex = 1.5)
    Plot3d( LE ~ CigCon + HealthExpPC | Continent, Smoking)
    Axes3d()
    Identify3d(pad=1)


    fit = lm( LE ~ CigCon + log(HealthExpPC) +I(CigCon^2) + I(log(HealthExpPC)^2) + I(CigCon*log(HealthExpPC)), Smoking)
    Fit3d( fit )
    fitl <- lm( LE ~ CigCon + HealthExpPC, Smoking)
    Fit3d( fitl, col = 'pink')
# HEpCap is highly 'skewed': dense on left, long tail on right
    require( lattice )
    densityplot( Smoking$HealthExpPC )

# Useful to use a transformation to make spread more even
#  e.g. log
# First make sure all values are positive:

    sort( Smoking$HealthExpPC)

# Do log transformation:

    Smoking$LogHE <- log(Smoking$HealthExpPC)    # create log HE

    densityplot( Smoking$LogHE )

# Also usefult to have categories:

    Smoking$HECat <- cut(Smoking$LogHE, 7)       # create categories
    summary(Smoking)

    Plot3d( LE ~ CigCon + LogHE |HECat, Smoking )  # condition on level of HEpC
    Axes3d()
    Ell3d()
    Identify3d(pad=1)

# Simple regression

    fit.lin <- lm( LE ~ CigCon, Smoking)
    Fit3d( fit.lin )
    fit.lin2 <- lm(LE ~ CigCon, subset(Smoking, Continent != "Africa"))
    Fit3d( fit.lin2)
    Pop3d(4)

# Use multiple regression (advanced version with quadratic surface)

    fit = lm( LE ~ CigCon + I(LogHE^2) +I(CigCon^2) + I(LogHE^2) + I(CigCon*LogHE), Smoking)
    Fit3d( fit, col = 'red' )
    Pop3d(2)

# refit omitting Africa:

    fit.na = lm( LE ~ CigCon + I(LogHE^2) +I(CigCon^2) + I(LogHE^2)
               + I(CigCon*LogHE), Smoking, subset = Continent != "Africa")
    Fit3d( fit.na, col = 'red' )

# Marginal relationship

    Pop3d(4)    # pop twice for each fit
    fit.quad <- lm( LE ~ CigCon + I(CigCon^2) , Smoking)
    Fit3d( fit.quad, col = 'green')

#  A quadratic surface within each Continent (overfitting?!)

    fit2 = lm( LE ~ Continent*(CigCon + HealthExpPC +I(CigCon^2) +
            I(HealthExpPC^2) + I(CigCon*HealthExpPC)), Smoking)
    Fit3d( fit2 )

#  For a 2D view of the y versus x

    view3d(0,0,0)   # note the first parameter, theta, rotates clockwise in the vertical axis
                    # from the initial position given by view3d(0,0,0); the second
                    # parameter, phi, tilts the graph around a horizontal screen axis
                    # towards the viewer.  The third parameter, fov, affects the
                    # amount of 'perspective'. fov = 0 yields an orthographic
                    # projection
}
}
\keyword{datasets}
