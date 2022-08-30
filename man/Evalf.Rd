\name{Evalf}
\Rdversion{1.1}
\alias{Evalf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Evaluates a function like predicts evaluates a model
}
\description{
Evaluates a function like \code{predict} evaluates a model
}
\usage{
Evalf(fun, data = newdata, newdata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fun}{
A function, taking arguments as in a given row of \code{data}
}
  \item{data}{
A data frame
}
  \item{newdata}{
Not presently used
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Retuns a list of the function evaluations for each row of \code{data}
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

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function( fun, data = newdata, newdata ) {
    # this is meant to evaluate a function much like predict 'evaluates' a model
    inds <- seq_along( rownames(data) )
    ret <- rep(NA, length(inds))
    for ( i in inds ) {
       ret[i] <- do.call(fun,data[i,])
    }
    ret
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
