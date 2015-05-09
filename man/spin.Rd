\name{spin}
\Rdversion{1.1}
\alias{spin}
\alias{spinto}
\alias{spins}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Spin or rotate a 3D plot
}
\description{
Like \code{rgl.viewpoint} but starts with defaults from \code{par3d},  except
for \code{theta} (rotation around vertical axis)
and \code{phi} (angular height above the horizontal plane) 

}
\usage{

spin(theta = 0, phi = 15, fov = par3d("FOV"), zoom = par3d("zoom"), scale = par3d("scale"), stay = FALSE)

spinto(theta = NULL, phi = NULL, fov = NULL, n = 100)

spins(inc.theta = 1/4, inc.phi = 0, theta = NULL, phi = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
rotation around vertical axis
}
  \item{phi}{
angular height above the horizontal plane
}
  \item{fov}{
amount of perspective, 0 for orthographic projection
}
  \item{zoom}{
Zoom factor, passed to \code{link[rgl]{rgl.viewpoint}}
}
  \item{scale}{
Scale factor, passed to \code{link[rgl]{rgl.viewpoint}}
}
  \item{inc.theta}{
  increment in theta for spinning
  }
  \item{inc.phi}{
  increment in phi for spinning
  }
  \item{n}{
  number of in-between steps when using \code{spinto} to rotate from current to target position
  }
  \item{stay}{
Not used in this implementation. 
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
None.
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
\code{link[rgl]{rgl.viewpoint}}
}
\examples{
require(p3d)
data(Smoking)
Plot3d( LE ~ CigCon + HealthExpPC | Continent, Smoking)
fit <- lm(LE ~ CigCon, Smoking)
fit2 <- lm( LE ~ CigCon + I(CigCon^2), Smoking)
fitc <- lm( LE ~ CigCon * Continent, Smoking)
Fit3d(fit)
Fit3d(fit2)
Fit3d(fitc)
# after manipulating the display, you can return to an orthographic 2D view of LE ~ CigCon with:
spinto()           # y vs x
spinto(0,-90,0)    # z vs x
spinto(-90,0,0)    # y vs z
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
