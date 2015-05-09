\name{Id3d}
\Rdversion{1.1}
\alias{Id3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Identify points in the current Plot3d rgl window
}
\description{
Drag a rectangle around a point to identify it, with the point label
as specified in the \code{labels} argument.

Drag a rectangle around an empty region to exit from point identification
mode, or make the R console the active window and press \code{<ESC>}.

If \code{select} is provided, labels are displayed for points whose labels match elements of \code{select}. 
}
\usage{
Id3d(select, labels = row.names(data), pad = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{select}{providing a vector of strings will plot labels for points whose labels are equal to a string}
  \item{labels}{Observation labels}
  \item{pad}{amount to pad labels}
  \item{\dots}{
Other arguments, passed down to 'rgl.texts'
}
}
\details{
By default, observations are labeled using the \code{row.names} in the 
data frame for the current \code{Plot3d} window.
}
\value{
None}
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
\code{\link{Identify3d}}, ~~~
}
\examples{
# none
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
