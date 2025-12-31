\name{center}
\Rdversion{1.1}
\alias{center}
\alias{center.ell}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Center of object
}
\description{
Return the center of an object
}
\usage{
center(obj, ...)

\method{center}{ell}(obj, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
A \code{ell} object
}
  \item{\dots}{
Other arguments
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Returns the \code{center} attribute of an ellipse
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
\code{\link{ell}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function( obj, ... ) UseMethod("center")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
