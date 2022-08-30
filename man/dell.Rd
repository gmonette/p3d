\name{dell}
\Rdversion{1.1}
\alias{dell}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Data ellipse in 2D
}
\description{
Calculates points on a 2D data ellipse
}
\usage{
dell(x, y, radius = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A numeric vector, or a two-column matrix, or a data.frame
}
  \item{y}{
A numeric vector
%%     ~~Describe \code{y} here~~
}
  \item{radius}{
Radius of the ellipse
}
  \item{\dots}{
Arguments passed to \code{ell}, including \code{n}, the number of points
on the ellipse.
}
}
%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}
\value{
\code{dell} returns an object of class \code{ell} consisting of a 
matrix whose rows are points on the ellipse and, thus, can be plotted with \code{plot} or \code{lines}.
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
\code{\link{ell}}, \code{\link{cell}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function( x, y, radius = 1, ...) {
        if ( (is.matrix(x) && (ncol(x) > 1))|| is.data.frame(x)) mat <- as.matrix(x[,1:2])
        else if (is.list(x)) mat <- cbind(x$x, x$y)
        else mat <- cbind( x,y)
        ell( apply(mat,2,mean), var(mat), radius = radius, ...)
    }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
