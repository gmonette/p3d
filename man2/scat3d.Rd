\name{scat3d}
\Rdversion{1.1}
\alias{scat3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
3D scatterplot adapted from a function by J. Fox
}
\description{
Draws a 3D scatterplot with a variety of graphical enhancements, allowing multiple groups,
data ellipsoids and a variety of fitted response surfaces to be displayed.
}
\usage{
scat3d(x, y, z, 
	xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), zlab = deparse(substitute(z)), 
	xtend = 0.05, debug = TRUE, xlim = xrange(x, xtend), ylim = xrange(y, xtend), zlim = xrange(z, xtend), 
	revolutions = 0, 
	col = "yellow", bg.col = c("white", "black"), axis.col = if (bg.col == "white") "black" else "white", 
	surface.col = c("blue", "green", "orange", "magenta", "cyan", "red", "yellow", "gray"), 
	neg.res.col = "red", pos.res.col = "green", point.col = col, text.col = axis.col, 
	grid.col = if (bg.col == "white") "black" else "gray", fogtype = c("none", "exp2", "linear", "exp"), 
	residuals = (length(fit) == 1), surface = FALSE, fill = TRUE, grid = TRUE, grid.lines = 26, 
	df.smooth = NULL, df.additive = NULL, sphere.size = 1, threshold = 0.01, speed = 1, fov = 30, 
	fit = "linear", groups = NULL, parallel = TRUE, ellipsoid = FALSE, var = FALSE, level = 0.5, 
	model.summary = FALSE, verbose = 0, clear = TRUE, pow = -1, origin = FALSE, axes = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x,y,z}{Numeric vectors of the same length.  Only complete cases will be plotted and used in other calculations}
  \item{xlab, ylab, zlab}{Labels for the respective axes}
  \item{xtend}{
Amount to extend each axis beyond the axis limits
}
  \item{debug}{
%%     ~~Describe \code{debug} here~~
}
  \item{xlim,ylim,zlim}{Limits for the respective axes}
   \item{revolutions}{
%%     ~~Describe \code{revolutions} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{bg.col}{
%%     ~~Describe \code{bg.col} here~~
}
  \item{axis.col}{
%%     ~~Describe \code{axis.col} here~~
}
  \item{surface.col}{
%%     ~~Describe \code{surface.col} here~~
}
  \item{neg.res.col}{
%%     ~~Describe \code{neg.res.col} here~~
}
  \item{pos.res.col}{
%%     ~~Describe \code{pos.res.col} here~~
}
  \item{point.col}{
%%     ~~Describe \code{point.col} here~~
}
  \item{text.col}{
%%     ~~Describe \code{text.col} here~~
}
  \item{grid.col}{
%%     ~~Describe \code{grid.col} here~~
}
  \item{fogtype}{
%%     ~~Describe \code{fogtype} here~~
}
  \item{residuals}{
%%     ~~Describe \code{residuals} here~~
}
  \item{surface}{
Logical, indicating whether fitted surface(s) should be added to the plot, as given by \code{fit}.}
  \item{fill}{
%%     ~~Describe \code{fill} here~~
}
  \item{grid}{
%%     ~~Describe \code{grid} here~~
}
  \item{grid.lines}{
%%     ~~Describe \code{grid.lines} here~~
}
  \item{df.smooth}{
Degrees of freedom for \code{"smooth"} fit
}
  \item{df.additive}{
Degrees of freedom for \code{"additive"} fit
}
  \item{sphere.size}{
%%     ~~Describe \code{sphere.size} here~~
}
  \item{threshold}{
%%     ~~Describe \code{threshold} here~~
}
  \item{speed}{
%%     ~~Describe \code{speed} here~~
}
  \item{fov}{
%%     ~~Describe \code{fov} here~~
}
  \item{fit}{
 A character vector containing one or more of \code{c("linear", "quadratic", "smooth", 
                "additive")}
}
  \item{groups}{
%%     ~~Describe \code{groups} here~~
}
  \item{parallel}{
%%     ~~Describe \code{parallel} here~~
}
  \item{ellipsoid}{
%%     ~~Describe \code{ellipsoid} here~~
}
  \item{var}{
%%     ~~Describe \code{var} here~~
}
  \item{level}{
%%     ~~Describe \code{level} here~~
}
  \item{model.summary}{
Logical: display fitted model summaries at end?
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{clear}{
%%     ~~Describe \code{clear} here~~
}
  \item{pow}{
%%     ~~Describe \code{pow} here~~
}
  \item{origin}{
Logical: include the origin in the plot frame?
}
  \item{axes}{
%%     ~~Describe \code{axes} here~~
}
}
%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Returns model summaries if \code{model.summary=TRUE}, otherwise \code{NULL}
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
