\name{Init3d}
\Rdversion{1.1}
\alias{Init3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Initialize rgl for Plot3d and related functions
}
\description{
Initialize rgl for Plot3d and related functions
}
\usage{
Init3d(par3d = list(), family = c("sans", "serif", "mono", "symbol"),  mouseMode = c("polar", "fov", "zoom"), cex = 0.8, font = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{par3d}{
%%     ~~Describe \code{par3d} here~~
}
  \item{family}{Font family to be used in Plot3d plots}
  \item{mouseMode}{Assignment to left, middle, right mouse buttons. default: \code{c("polar", "fov", "zoom")}}
  \item{cex}{
%%     ~~Describe \code{cex} here~~
}
  \item{font}{
%%     ~~Describe \code{font} here~~
}
  \item{\dots}{Other arguments passed down}
}
\details{
\code{Init3d} calls \code{\link[rgl]{par3d}} to set up a variety of defaults
for subsequent plots.
}
\value{
None.
%%  ~Describe the value returned
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
\code{\link[rgl]{par3d}}, 
\code{\link{Plot3d}}, ~~~
}
\examples{
# none
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
