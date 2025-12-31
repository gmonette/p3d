\name{S.theta}
\Rdversion{1.1}
\alias{S.theta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Rotate around vertical axis by theta degrees
}
\description{
Calculates the matrix to rotate around the vertical axis of a 3D plot by theta degrees
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
S.theta(theta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
Angle of rotation
}
}
\details{
The \code{userMatrix} for spinning has the form \code{E.phi(phi) \%*\% S.theta(theta) }

}
\value{
Returns the 4 x 4 rotation matrix
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
\code{\link{E.phi}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function( theta ) {
    # rotate through vertical axis through phi degrees
    theta = theta *2*pi/360
    cbind( c(cos(theta), 0,sin(theta),0), c(0,1,0,0), c(-sin(theta), 0, cos(theta),0), c(0,0,0,1))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
