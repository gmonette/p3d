\name{pop}
\Rdversion{1.1}
\alias{pop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove last objects added to a 3D plot
}
\description{
Remove last \code{n} objects added to a 3D plot via \code{rgl.pop}
}
\usage{
pop(n = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
Number of items to remove
}
  \item{\dots}{
Passed to \code{rgl.pop}
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
None.
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
\code{\link{Pop3d}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(n=1,...) for ( i in 1:n) rgl.pop(...)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
