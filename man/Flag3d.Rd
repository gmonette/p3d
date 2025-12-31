\name{Flag3d}
\alias{Flag3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Displays labels in 3d plot.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Flag3d(select, x = pars$data[[pars$names["x"]]], y = pars$data[[pars$names["y"]]], z = pars$data[[pars$names["z"]]], labels = rownames(Plot3d_par("data")), groups = NULL, col = Plot3d_par()$col, adj = 0, debug = FALSE, pad = 0, offset = ((100/length(x))^(1/3)) * 0.02, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{select}{
a vector of labels to display. Elements will be displayed if they match a label for a point in the 3d display.
}
  \item{x,y,z,labels,groups,col}{
generally obtained from the 3d global object returned with Plot3d_par()
}

  \item{adj}{
is passed to rgl.texts
%%     ~~Describe \code{adj} here~~
}
  \item{debug}{
%%     ~~Describe \code{debug} here~~
}
  \item{pad}{
number of character to pad labels on the left. %%     ~~Describe \code{pad} here~~
}
  \item{offset}{
%%     ~~Describe \code{offset} here~~
}
  \item{\dots}{
  passed to rgl.texts
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns the vector of elements in \code{select} that were matched in \code{labels}.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
G. Monette (borrowing heavily from J. Fox)
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (select, x = pars$data[[pars$names["x"]]], y = pars$data[[pars$names["y"]]], 
    z = pars$data[[pars$names["z"]]], labels = rownames(Plot3d_par("data")), 
    groups = NULL, col = Plot3d_par()$col, adj = 0, debug = FALSE, 
    pad = 0, offset = ((100/length(x))^(1/3)) * 0.02, ...) 
{
    pars <- Plot3d_par()
    if ((is.matrix(x) && (ncol(x) > 1)) || is.data.frame(x)) {
        dat <- x
        x <- dat[, 1]
        y <- dat[, 2]
        z <- dat[, 3]
        if (missing(labels)) 
            labels <- rownames(dat)
    }
    if (pars$has.groups && missing(groups)) {
        groups <- pars$data[[pars$names["g"]]]
    }
    valid <- if (is.null(groups)) 
        complete.cases(x, y, z)
    else complete.cases(x, y, z, groups)
    x <- x[valid]
    y <- y[valid]
    z <- z[valid]
    labels <- labels[valid]
    pad <- paste(rep(" ", pad), collapse = "")
    labels.pad <- paste(pad, labels)
    rgl.bringtotop()
    groups <- if (!is.null(groups)) 
        as.numeric(groups[valid])
    if (is.numeric(select) || is.logical(select)) 
        select <- labels[which]
    which <- labels \%in\% select
    if (!is.null(groups)) 
        cols <- col[groups][which]
    else cols <- col[1]
    rgl.texts(x[which], y[which] + offset, z[which], labels.pad[which], 
        color = cols, adj = adj, ...)
    labels[which]
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
