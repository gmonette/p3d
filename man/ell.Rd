\name{ell}
\Rdversion{1.1}
\alias{ell}
%\alias{dell}
\alias{ellplus}
\alias{elltan}
\alias{elltanc}
\alias{ellpt}
\alias{ellptc}
\alias{ellbox}
\alias{ellpts}
\alias{ellptsc}
%\alias{cell}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Ellipses in 2D
}
\description{
Tools to generate 2D data, concentration and confidence ellipses given a center and a 'variance' matrix. Also tools
to generate points on a ellipse in a given direction or conjugate to that direction, axes
along a vector or conjugate to it, tangent lines at a point or parallel to a vector.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{

ell( center = c(0,0), shape = diag(2), radius = 1, n = 100)

%dell( x, y, radius = 1, n = 100)

ellpt( ell, dir = c(0,1) , radius = 1 )

ellptc( ell, dir = c(0,1) , radius = 1 )

elltan( ell, dir = c(0,1) , radius = 1 , len = 1, v= c(-1,1))

elltanc( ell, dir = c(0,1) , radius = 1 , len = 1, v= c(-1,1))

ellbox( ell, dir = c(0,1) , radius = 1 )

ellpts( ell, dir = c(0,1) , radius = 1 )

ellptsc( ell, dir = c(0,1) , radius = 1 )

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{center}{  the center of the ellipse
}
  \item{shape}{   a 2 x 2 positive semi-definite. It is the variance matrix of a multivariate normal for which the ellipse is a concentration ellipse.
}
  \item{radius}{ of the ellipse or point relative to the unit ellipse. For all functions except those generating tangents \code{radius} can be a vector.
  For example, \code{ ellpt( ell, c(0,1), radius = c(-1,1))} will generate an axis of the unit ellipse.
}
  \item{n}{  number of points to generate for the ellipse
%%     ~~Describe \code{n} here~~
}
  \item{ell}{  is an object of class \code{ell} created by \code{ell}, \code{dell} or \code{cell}
}
  \item{dir}{  vector giving the direction from the center of the ellipse to find a point on the ellipse or a tangent to it, or a direction conjugate to the direction in which ...
}
  \item{len}{  half 'length' of a tangent vector
}
  \item{v}{  tangent vector
%%     ~~Describe \code{n} here~~
}
}
\details{
\code{ell} returns a matrix of points on the ellipse, suitable for plotting
with \code{lines}.

\code{ellpt} returns a point on an ellipse particular direction specified by \code{dir}

\code{ellptc} returns a point on an ellipse in a conjugate direction specified by \code{dir}

\code{ellpts} returs the nine points of the enclosing parallelogram (+ the centre) with the parallelogram whose
with a side parallel to \code{dir}. \code{ellptsc} returns the same points but in a different order

}
\value{
The functions \code{ell} and \code{dell} return an object of class \code{ell} consisting of
matrix whose rows are points on the ellipse and, thus, can be plotted with \code{plot} or \code{lines}.
The other functions return a n x 2 matrix of points to plotted.
}
%\references{
%%% ~put references to the literature/web site here ~
%}
\author{
Georges Monette
}
%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{cell}}, \code{\link{dell}}, \code{\link{ell.conj}}, ~~~
}
\examples{
\dontrun{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

  plot( e1 <- ell(c(1,1), matrix(c(1,.6,.6,1), ncol = 2)) )
  ellptc ( e1, radius = c(-1,1))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
