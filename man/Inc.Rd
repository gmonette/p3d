\name{Inc}
\Rdversion{1.1}
\alias{Inc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Increment a number possibly represented in a character string or factor
}
\description{
Increment a number possibly represented in a character string or factor
}
\usage{
Inc(n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
A number, character string or factor
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
The incremented value
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
function( n ) {
        n <- as.character(n)
        len <- nchar(n)
        np1 <- as.numeric(n) + 1
        np1c <- as.character(np1)
        len <- max( len, nchar(np1c))
        formatC( np1, flag = '0', width = len)
    }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
