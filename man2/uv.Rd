\name{uv}
\Rdversion{1.1}
\alias{uv}
\alias{uv.ell}
\alias{uv.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Conjugate complement of a space
}
\description{
Conjugate complement of a space
}
\usage{
uv(object, ...)
\method{uv}{ell}(object, u, radius = 1, ...)
\method{uv}{default}(object, u, radius = 1, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
An \code{ell} object or the shape matrix for a multivariate sample
}
  \item{u}{
A direction vector
}
  \item{radius}{
Radius
}
  \item{\dots}{
Other arguments
}

}
\details{
None yet
}
\value{
A list with elements
%%  If it is a LIST, use
  \item{u}{A unit vector in the direction 'dir'}
  \item{v}{Conjugate direction}
%% ...
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Georges Monette
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function( shape, u , radius = 1) {
       # returns 'unit' u and conjugate v
       ConjComp <- function( X , Z = diag( nrow(X)) , ip = diag( nrow(X)), tol = 1e-07 ) {

                     # ConjComp returns a basis for the conjugate complement of the
                     # conjugate projection of X into span(Z) with respect to inner product with
                     # matrix ip.
                     # Note: Z is assumed to be of full column rank but not necessarily X.
                     xq <- qr(t(Z) \%*\% ip \%*\% X, tol = tol)
                     if ( xq$rank == 0 ) return( Z )
                     a <- qr.Q( xq, complete = T ) [ ,-(1:xq$rank)]
                     Z \%*\% a
            }

            u <- u / sqrt( sum( u*solve(shape,u)))   # 'unit' vector in direction of dir
            v <- c(ConjComp( u, diag(2) , solve(shape)))  # conjugate
            v <- v / sqrt( sum( v * solve( shape, v)))
       list(u = radius * u, v= radius * v)
    }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
