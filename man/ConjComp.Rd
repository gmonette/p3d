\name{ConjComp}
\alias{ConjComp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate a basis for the conjugate complement of a conjugate projection
}
\description{
ConjComp returns a basis for the conjugate complement of the conjugate projection 
of \code{X} into span(\code{Z}) with respect to inner product with matrix \code{ip}.   


}
\usage{
ConjComp(X, Z = diag(nrow(X)), ip = diag(nrow(X)), tol = 1e-07)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
A matrix
}
  \item{Z}{
A matrix giving the span for the projection, assumed to be of full column rank
}
  \item{ip}{
inner product matrix
}
  \item{tol}{
tolerance
}
}
\details{
none yet
}
\value{
A matrix of the same size as \code{X}
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
none yet
}
\examples{
X <- matrix(1:15, ncol=3)
X
R <- ConjComp(X)
round(t(R) \%*\% X)
round(t(X) \%*\% R)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
