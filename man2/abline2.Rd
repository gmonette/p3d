\name{abline2}
\Rdversion{1.1}
\alias{abline2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate arguments to plot a line
}
\description{
Generates arguments to plot a line via \code{abline}
}
\usage{
abline2(p1, p2, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p1}{
Either a list of points or a single point
}
  \item{p2}{
Another point
}
  \item{\dots}{
Other points, or arguments to \code{abline}
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
None. Used for its side effect
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
\code{\link[graphics]{abline}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function( p1, p2, ...) {
            if ( is.list(p1)) abline2( p1[[1]], p1[[2]], ...)
            else do.call( abline, c(ab(p1,p2),...))
    }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
