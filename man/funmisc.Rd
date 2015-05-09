\name{funmisc}
\Rdversion{1.1}
\alias{fg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Miscellaneous functions for p3d
%%  ~~function to do ... ~~
}
\description{
A collection of functions to support the pacakge \code{p3d}
}
\usage{
fg(stay = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stay}{
if TRUE the rgl window covers other windows until killed or \code{fg(FALSE)} is given. Also, \code{Id3d} terminates the status of the win
}
}
\details{
\code{fg()} is an abbreviation for \code{rgl.brintotop(stay=TRUE)}  since the command is generally needed in presentations where brevity is a virtue. An alternative in presentations is to use the "Always on Top" program that works under Windows: Ctrl-Space keeps the active window on top. Issuing Ctrl-Space again while it is active allows it to revert to normal behaviour. 
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (stay = TRUE) 
rgl.bringtotop(stay = stay)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
