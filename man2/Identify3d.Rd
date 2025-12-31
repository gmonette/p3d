\name{Identify3d}
\Rdversion{1.1}
\alias{Identify3d}
\alias{Identify3d.default}
\alias{Identify3d.formula}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Identify points in 3D
}
\description{
The function Identify3d allows you to label points interactively with the mouse: Press the right mouse button 
(on a two-button mouse) or the centre button (on a three-button mouse), drag a rectangle around the points 
to be identified, and release the button. 
Repeat this procedure for each point or set of "nearby" points to be identified. 

To exit from point-identification mode, click the right (or centre) button in an empty region of the plot.
}
\usage{
Identify3d(x, ...)

\method{Identify3d}{default}(
    x = pars$data[[pars$names['x']]],
    y = pars$data[[pars$names['y']]],
    z = pars$data[[pars$names['z']]],
    labels = rownames(.Plot3d.par$data), groups = NULL,
    col = Plot3d.par()$col, 
    adj = 0, debug = FALSE, pad = 0,
    offset = ((100/length(x))^(1/3)) * 0.02,
    ...
    )

\method{Identify3d}{formula}(formula = attr(data, "formula"), data = sys.parent(), labels = NULL, adj = 0, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{"model" formula, of the form \code{y ~ x + z}.
}
  \item{data}{data frame, for the formula method.
}
  \item{x}{variable for horizontal axis.
}
  \item{y}{variable for vertical axis (response).
}
  \item{z}{variable for out-of-screen axis.
}
  \item{labels}{
	text labels for the points, one for each point; in the default method defaults to the observation indices, in the formula method to the row names of the data
}
  \item{groups}{A factor or variable dividing the observations into groups.
}
  \item{col}{colors for the point labels.
}
  \item{adj}{
label \code{adj} value, passed to \code{\link[rgl]{rgl.texts}}
}
  \item{debug}{
if \code{TRUE}, print debugging output.
}
  \item{pad}{
padding between point and label, in number of \code{" "} characters.
}
  \item{offset}{
offset to \code{y} value, passed to \code{\link[rgl]{rgl.texts}}.
}
  \item{\dots}{
Other arguments passed down
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
\code{Identify3d} returns the labels of the identified points
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
\code{\link{Plot3d}}
}
\examples{
\dontrun{
    head(Smoking)
    rownames(Smoking) = Smoking$Country

    Init3d(family = 'serif', cex = 1.5)
    Plot3d( LE ~ CigCon + HealthExpPC | Continent, Smoking)
    Axes3d()
    Identify3d(pad=1)
 }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{iplot}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
