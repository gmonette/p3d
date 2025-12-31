\name{xy}
\Rdversion{1.1}
\alias{xy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Points from list for plotting
}
\description{
Turn lists of points into a matrix for plotting
}
\usage{
xy(p1, p2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p1}{
A matrix of (x,y) coordinates, or a list of such matrices
}
  \item{p2}{
A matrix of (x,y) coordinates
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
The result of \code{rbind} on the arguments
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
\code{\link[base]{rbind}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function( p1, p2 ) {
          # turn list of points into matrix for plotting
          if( is.list( p1) ) do.call( rbind, plist)
          else rbind( p1, p2)
    }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
