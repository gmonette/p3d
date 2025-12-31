\name{ell.conj}
\Rdversion{1.1}
\alias{ell.conj}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find conjugate axes or tangent lines to ellipse
}
\description{
Find conjugate axes or tangent lines to ellipse
}
\usage{
ell.conj(center, shape, dir, radius = 1, len = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{center}{
center of the ellipse
}
  \item{shape}{
shape matrix of the ellipse
}
  \item{dir}{
a vector specifying a direction along which to find conjugate axes or tangent lines
}
  \item{radius}{
%%     ~~Describe \code{radius} here~~
}
  \item{len}{
%%     ~~Describe \code{len} here~~
}
}
\details{
None yet
}
\value{
Returns a list with the following components
%%  If it is a LIST, use
  \item{u }{}
  \item{v }{}
  \item{tan1 }{}
  \item{tan2 }{}
  \item{tan3 }{}
  \item{tan4 }{}
  \item{center }{ellipse center}
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

\seealso{
\code{\link{uv}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function( center, shape, dir, radius = 1, len = 1) {
            # returns conjugate axes or tangent lines to ellipse
            vecs <- uv( shape, dir, radius)
            list( u = list( center-len*vecs$u, center+len*vecs$u),
                  v = list( center-len*vecs$v, center+len*vecs$v),
                  tan1 = list( center + vecs$u - len*vecs$v,center + vecs$u+ len*vecs$v),
                  tan2 = list( center - vecs$u - len*vecs$v,center - vecs$u+ len*vecs$v),
                  tan3 = list( center + vecs$v - len*vecs$u,center + vecs$v+ len*vecs$u),
                  tan4 = list( center - vecs$v - len*vecs$u,center - vecs$v+ len*vecs$u),
                  center = center)
    }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
