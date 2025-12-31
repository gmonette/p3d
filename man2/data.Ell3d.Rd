\name{data.Ell3d}
\Rdversion{1.1}
\alias{data.Ell3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Data ellipse in 3D
}
\description{
Plots a data ellipse in 3D for the current data
}
\usage{
data.Ell3d(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
None yet.
}
\value{
None
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
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
\code{\link{Ell3d}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(  ... ) {
    #
    # NEEDS WORK:
    # should work like 'ellipsoid = TRUE' when called with no arguments
    #
      a <- args3d(...)
      xyz <- a$x
      oargs <- a[-1]
      center <- apply( xyz, 2, mean, na.rm = T)
      shape <-  var( xyz, na.rm = T, use = "complete")
      do.call( Ell3d, c( center= center, shape = shape, oargs))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
