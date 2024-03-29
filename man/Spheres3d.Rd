\name{Spheres3d}
\Rdversion{1.1}
\alias{Spheres3d}
\alias{Spheres3d.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Draw sphere in 3D space
}
\description{
A general function to draw spheres in 3D space.

\code{Spheres3d} allows arguments to be vectors or matrices whose corresponding
axes are specified with names of the form 'x', 'xy', 'yxz', etc.
}
\usage{
Spheres3d(obj, ...)

\method{Spheres3d}{default}(obj, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
Not presently used.
}
  \item{\dots}{
The \code{\dots}} argument is used to pass a collection
of vectors or matrices whose corresponding
axes are specified with names of the form 'x', 'xy', 'yxz'.
Special forms assigning \code{x}, \code{y}, \code{z} as \code{'min'} or \code{'max'}, such as
\code{x='min'} are treated appropriately.
Other arguments, such as \code{col}, \code{lwd} can also be passed.
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
\code{\link{Plot3d}}, ~~~
}
\examples{
require(car)
Plot3d( prestige ~ education + income, Prestige, col="blue")
data.Ell3d()
# marginal ellipses
# FIX THIS
Lines3d( y = 'min', xz = with( Prestige, dell( education, income)), lwd=2)
Lines3d( z = 'min', xy = with( Prestige, dell( education, prestige)), lwd=2)
Lines3d( x = 'min', yz = with( Prestige, dell( prestige, income)), lwd=2)
}
