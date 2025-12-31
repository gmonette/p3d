\name{p3d-package}
\Rdversion{1.1}
\alias{p3d-package}
\alias{p3d}
\docType{package}
\title{
Visualization of models in 3D
}
\description{
\code{p3d} is a very rough collection of functions to help visualize models
that are expressed as functions of two independent variables with the
addition of a possible categorical variable. It is based on an older version
of \code{scatter3d} in the \code{car} package by John Fox which, in turn,
uses the \code{rgl} package.

The essential idea is provided by the \code{\link{Plot3d}} function, 
which allows plotting a data frame via a formula of the form
\code{y ~ x + z [| g]}, where \code{y} is the response,
\code{x, z} are independent variables and \code{g} is an optional 
grouping factor.  In addition to generating the plot, the function 
saves information about the current data frame and graphics parameters
in a globally accessible structure.

Following a call to \code{\link{Plot3d}}, the following can be done using simple calls:

points can be interactively labeled using \code{\link{Identify3d}};

the fitted surfaces for other related
models using the same data can be added using \code{\link{Fit3d}}

graphical annotations that can be represented as a collection of line segments in 3D space
can be added using \code{\link{Lines3d}}

3D data ellipsoids can be added using \code{\link{Ell3d}}

In addition, the package contains a variety of utility functions for working with 2D and 3D
models, graphical enhancements and 3D geometry.
}
\details{
\tabular{ll}{
Package: \tab p3d\cr
Type: \tab Package\cr
Version: \tab 0.02-3\cr
Date: \tab 2011-10-28\cr
License: \tab GPL version 2 or newer\cr
LazyLoad: \tab yes\cr
}
A simple example below illustrates the major features of \code{p3d}.


}
\author{
Georges Monette

Maintainer: Georges Monette <georges@yorku.ca>
}
\references{
Monette, G. (1990), Geometry of Multiple Regression and Interactive 3-D Graphics, In Fox, J. & Long, S. (ed.), 
\emph{Modern Methods of Data Analysis}, Sage Publications, 209-256. 
}
%~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory ~~
\keyword{ package }
\keyword{ multivariate }
\seealso{
%~~ Optional links to other man pages, e.g. ~~
\code{\link[rgl:rgl-package]{rgl}}
}
\examples{
\dontrun{
  library(p3d)
  library(car)

  data(Prestige)

  fit <- lm( women ~ education * prestige * type, Prestige)

  Init3d()
  Plot3d( women ~ education + prestige | type, Prestige)
  Fit3d( fit )
  fit2 <- lm( women ~ education + prestige, Prestige)
  Fit3d( fit2, col = 'red')
  Ell3d()
}
}
\section{Changes}{
Lines3d.default: 2009-05-09: disabled line changing 'lwd' to 'size' since 'segments3d' uses 'lwd'.
}

