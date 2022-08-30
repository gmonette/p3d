\name{Fit3d}
\Rdversion{1.1}
\alias{Fit3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
3D plots of fitted response surfaces and functions
}
\description{
For the current plot, created with \code{\link{Plot3d}}, you can use \code{Fit3d} to superimpose
the fitted response surface for one or more models. For example, following

\code{Plot3d( y ~ x + z | g, data)}

the fitted surface for a model fit with \code{lm} can be displayed with

\code{Fit3d ( lm( y ~ x*z + I(x^2) + I(z^2) + g, data ))}

Other forms are possible, as described under Details.
 
}
\usage{
Fit3d(fit, names.vars = pars$names, other.vars = NULL, 
	grid.lines = 26, col = "blue", 
	fill = TRUE, grid = TRUE, base.grid = FALSE, col.grid = col, col.res = col, 
	residuals = FALSE, xlim = c(bbox[1], bbox[2]), zlim = c(bbox[5], bbox[6]), 
	verbose = 0, type = "response", alpha = 0.5, lit = FALSE, FUN = function(x) x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{
A fitted model object (e.g., of class \code{lm}, \code{glm} or \code{lme}  )
involving two quantitative covariates and possibly other variables,
or a function of two continuous variables and possibly other variables
}
  \item{names.vars}{
%%     ~~Describe \code{names.vars} here~~
}
  \item{other.vars}{list of values for variables used in model but not in display}
  \item{grid.lines}{
Number of grid lines for \code{x} and \code{z} variables
}
  \item{col}{
The color used for the grid of fitted values
and other annotations unless specified with other
\code{col.*} arguments.
}
  \item{fill}{
Draw fitted or function values as a shaded surface? Default: TRUE
}
  \item{grid}{
Draw fitted or function values as a grid with lines? Default: TRUE
}
  \item{base.grid}{draw grid in base horizontal plane?  Default: FALSE}

  \item{col.grid}{
Color used to draw the grid if \code{grid=TRUE}
}
  \item{col.res}{
Color used to show residuals if \code{residuals=TRUE}
}
  \item{residuals}{
Show residuals in the plot (i.e., lines from the surface to the points)? 
}
  \item{xlim}{
Limits for the \code{x} variable, to override those in the current plot
}
  \item{zlim}{
Limits for the \code{y} variable, to override those in the current plot
}
  \item{verbose}{
Tell me what you're doing}
  \item{type}{
For models fit with \code{glm}, fitted values can be displayed on
the \code{type="response"} scale or the \code{type="link"} scale.
}
  \item{alpha}{Transparency value for the fitted surface}

  \item{lit}{
rgl lighting. See ...
}
  \item{FUN}{
The response for a model can be a transformation of the scale of the
plot. The \code{FUN} argument is used to transform the fitted value
back to the scale of the graph, e.g.

\code{fit <- lm( log(y) ~ x * g, data); Fit3d( fit, FUN = exp)}
}
  \item{\dots}{
Other arguments passed down
}
}
\details{
Details: TBW
}
\value{
No useful value, unless \code{verbose=TRUE}, in which case \code{summary(fit)}
is returned.
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
 library(car)
 data(Prestige)
 Prestige$inc <- Prestige$income / 1000
 open3d()
 Plot3d ( inc ~ women + education|type , Prestige)
 
 Fit3d ( lm(inc ~ women + education,Prestige))
 Fit3d ( lm(inc ~ women * education,Prestige), col="pink")
}

data(coffee)
# fit marginal and joint models
fit.stress <- lm(Heart ~ Stress, data=coffee)
fit.coffee <- lm(Heart ~ Coffee, data=coffee)
fit.both   <- lm(Heart ~ Coffee + Stress, data=coffee)

Init3d(cex=1.2)
Plot3d( Heart ~ Coffee + Stress, coffee,
	surface=TRUE, fit="linear", surface.col="lightblue", 
	grid.lines=10, sphere.size=0.6)
box3d(color="gray")

data.Ell3d(col="lightblue")
# bivariate marginal ellipses
Lines3d( y = 'min', xz = with( coffee, dell( Coffee, Stress)), lwd=2, color="black")
Lines3d( z = 'min', xy = with( coffee, dell( Coffee, Heart)), lwd=2, color="blue")
Lines3d( x = 'min', yz = with( coffee, dell( Heart, Stress)), lwd=2, color="blue")
# fitted marginal regression planes
Fit3d(fit.stress, col="pink")
Fit3d(fit.coffee, col="green")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
\keyword{ hplot }% __ONLY ONE__ keyword per line
