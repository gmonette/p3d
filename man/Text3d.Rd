\name{Text3d}
\Rdversion{1.1}
\alias{Text3d}
\alias{Text3d.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add text to a 3D plot 
%%  ~~function to do ... ~~
}
\description{
Add text to a 3D plot (in development)
}
\usage{
Text3d(obj, ..., col = "blue")

\method{Text3d}{default}(..., col = "blue")

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
%%     ~~Describe \code{obj} here~~
}
  \item{\dots}{
See details.
}
  \item{col}{
Text color.
}
}
\details{
    Text3d allows arguments to be vectors or matrices whose corresponding
    axes as specified with names of the form 'x', 'xy', 'yxz', etc.
    The text argument is called 'texts'. Other key arguments are adj and
    justify
}
\value{
None
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
\code{\link{Lines3d}}, \code{\link{args3d}}, 
\code{\link[rgl]{rgl.texts}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function( obj, ...) {
    # Note that this is the 'same' as Lines3d
    "
    Text3d allows arguments to be vectors or matrices whose corresponding
    axes as specified with names of the form 'x', 'xy', 'yxz', etc.
    The text argument is called 'texts'. Other key arguments are adj and
    justify
    "
    UseMethod("Text3d")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
