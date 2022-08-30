\name{Pop3d}
\Rdversion{1.1}
\alias{Pop3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove last object added to 3D plot
}
\description{
Removes the last \code{n} objects added to 3D plot
}
\usage{
Pop3d(n = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
Number of objects to remove
}
  \item{\dots}{
Other arguments
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
None
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
\code{\link{pop}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(n=1,...) invisible(replicate( n, pop3d()))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
