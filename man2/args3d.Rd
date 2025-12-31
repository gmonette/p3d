\name{args3d}
\Rdversion{1.1}
\alias{args3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Process 3D arguments
}
\description{
Process 3D arguments, also allowing \code{xy}, \code{xz}, \code{yz} to
indicate planes in 3D space, and allowing, e.g., \code{x='min'},
\code{x='max'}, \code{x='mid'} to specify the minimum, maximum, or
middle value for a given coordinate axis.
}
\usage{
args3d(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
A collection of arguments for 3D graphics functions including
\code{x}, \code{y}, \code{z}
}
}
\details{
For any coordinate axis or plane you can specify 'min', 'max', 'mid' as values 

}
\value{
Returns a list with a matrix, x with xyz values and with other arguments
specified in the call.
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
\note{
If  the first argument is positional then variables must be in order x,y,z
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Plot3d}}, \code{\link{Lines3d}}, 
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(...) {
help <- "

    - returns a list with a matrix, x with xyz values and with other args
    - can specify 'min', 'max', 'mid' as values not mixed with numerical
      values
    CAUTION: If first argument is positional then variables
             must be in order x,y,z
"
    na2f <- function( x ) {
         x[is.na(x)] <- FALSE
         x
    }
    args <- list(...)
    
    getdim <- function(nam, args){
        pos <- regexpr(nam, names(args)) 
        arg.ind <- pos > 0
        if( length(pos <- pos [ pos > 0 ])==0) return(0)
        cbind( args[arg.ind][[1]])[,pos]
    }
    
    if( is.null(names(args))) names(args) <- rep("",length(args) )
    nn <- names(args)
		#disp(nn)
        if ( (length(nn) >2) && all( nn[1:3] =="")) names(args) [1:3] <- c('x','y','z')
        nn <- names(args)
        if ( (length(nn) > 0) && (nn[1] == "") ){
						 names(args)[1] <- 'xyz'
						 args[[1]] = rbind( args[[1]])	
				}
        nn <- names(args)
        #disp(nn)
    nxyz <- sapply( nn, regexpr, "xyzyxzxzyyzxzxyzyz")
    #disp(nxyz)

    xyzs <- args[nxyz > 0]
    #disp(xyzs)
    oargs <- args[nxyz < 0]
    #disp(oargs)
		xyz <- cbind( x=getdim('x', xyzs), y=getdim('y',xyzs), z=getdim('z',xyzs))
    if ( is.character(xyz) ) {
        bbox <- par3d('bbox')
        mins <- matrix(bbox[c(1,3,5)], nrow = nrow(xyz), ncol=3, byrow = T)
        maxs <- matrix(bbox[1+c(1,3,5)], nrow = nrow(xyz), ncol=3, byrow = T)
        mids <- (mins + maxs)/2
        xyz[na2f(xyz=='min')] <- mins[na2f(xyz=='min')]
        xyz[na2f(xyz=='max')] <- maxs[na2f(xyz=='max')]
        xyz[na2f(xyz=='mid')] <- mids[na2f(xyz=='mid')]
        mode(xyz) <- "numeric"
    }
    a <- list(x = xyz)
    c(a,oargs)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
