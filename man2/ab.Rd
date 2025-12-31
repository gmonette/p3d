\name{ab}
\Rdversion{1.1}
\alias{ab}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate arguments to plot a line
}
\description{
Generate arguments to plot a line via \code{abline}.
}
\usage{
ab(p1, p2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p1}{
first point
}
  \item{p2}{
second point
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A list with suitable named arguments for \code{abline}
%%  If it is a LIST, use
  \item{v}{For a horizontal line}
  \item{a, b }{intercept and slope}
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
\code{\link[graphics]{abline}},
\code{\link{abline2}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function( p1, p2 ) {
       # prepare arguments to plot a line with
       # do.call('abline', ab( p1, p2))
       d <- p2 - p1
       if ( d[1] == 0) {
          ret <- list( v = p1[1])
       }
       else {
          b = (p2[2]-p1[2])/(p2[1]-p1[1])
          ret <- list( a = p2[2] - b*p2[1], b =b)
       }
       ret
    }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
