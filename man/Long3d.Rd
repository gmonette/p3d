\name{Long3d}
\Rdversion{1.1}
\alias{Long3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Represent hierarchical longitudinal data in 3D
}
\description{
Represent hierarchical longitudinal data in 3D
}
\usage{
Long3d(fmla, data, id, ..., col.lines = "grey", lwd.lines = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fmla}{
%%     ~~Describe \code{fmla} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{id}{
%%     ~~Describe \code{id} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{col.lines}{
%%     ~~Describe \code{col.lines} here~~
}
  \item{lwd.lines}{
%%     ~~Describe \code{lwd.lines} here~~
}
}
\details{
None yet
}
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
 \code{\link{Plot3d}}, ~~~
}
\examples{
\dontrun{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function( fmla, data, id, ..., col.lines = 'grey', lwd.lines = 1) {
      help <- "
      OBSOLETE: use Plot3d( ...., groups = id)
      
      Long3d( y ~ x + z | g, data, groups = id)
         will join points with the same 'groupid' with lines to
         show trajectories for longitudinal data.
         This function is still very rudimentary.
     
      "
         Plot3d( fmla, data, ...)
          lapply( split( 1:nrow(data), data[[id]]), function( ind ) {
              dz <- data[ ind,]
               fmla <- as.formula(paste( sub("\\|","+",as.character(fmla))[c(2,1,3)],collapse =""))
               Lines3d( yxz = model.frame( fmla, dz)[,1:3],col = col.lines,
                  lwd = lwd.lines)
         }
        )
        invisible(NULL)    
    }
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
