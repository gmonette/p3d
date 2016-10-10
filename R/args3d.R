##
## p3d:args3d.R
## 2011-12-22
##

#' @export
args3d <-   # remove this version is the next one works
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
    #disp(args)
    getdim <- function(nam, args){
        pos <- regexpr(nam, names(args))
        #disp(nam)
        #disp(pos)
        arg.ind <- pos > 0
        #disp(arg.ind)
        if( length(pos <- pos [ pos > 0 ])==0) return(0)
        #disp( args[arg.ind] )
        #disp(cbind( args[arg.ind][[1]]))
        #disp(pos)

        #disp(cbind( args[arg.ind][[1]])[,pos])
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
        mins <- matrix(bbox[c(1,3,5)], nrow = nrow(xyz), ncol=3, byrow = TRUE)
        maxs <- matrix(bbox[1+c(1,3,5)], nrow = nrow(xyz), ncol=3, byrow = TRUE)
        mids <- (mins + maxs)/2
        xyz[na2f(xyz=='min')] <- mins[na2f(xyz=='min')]
        xyz[na2f(xyz=='max')] <- maxs[na2f(xyz=='max')]
        xyz[na2f(xyz=='mid')] <- mids[na2f(xyz=='mid')]
        mode(xyz) <- "numeric"
    }
    a <- list(x = xyz)
    #disp(c(a,oargs))
    c(a,oargs)
}



# Following is changed so
# 1) keeps unnamed arguments
# 2) changes 'col' to 'color'

#' @export
args3d <-
function (...)
{
    na2f <- function(x) {
        x[is.na(x)] <- FALSE
        x
    }
    args <- list(...)
    #disp(args)
    getdim <- function(nam, args) {
        pos <- regexpr(nam, names(args))
        arg.ind <- pos > 0
        if (length(pos <- pos[pos > 0]) == 0)
            return(0)
        cbind(args[arg.ind][[1]])[, pos]
    }
    if (is.null(names(args)))
        names(args) <- rep("", length(args))
    nn <- names(args)
    if ((length(nn) > 2) && all(nn[1:3] == ""))
        names(args)[1:3] <- c("x", "y", "z")
    nn <- names(args)
    if ((length(nn) > 0) && (nn[1] == "")) {
        names(args)[1] <- "xyz"
        args[[1]] = rbind(args[[1]])
    }
    #disp(args)
    nn <- names(args)
    #disp(nn)
    nxyz <- sapply(nn, regexpr, "xyzyxzxzyyzxzxyzyz")
    nxyz[nn==""]<--1
    #disp(nxyz)
    xyzs <- args[nxyz > 0]
    oargs <- args[nxyz < 0]
    xyz <- cbind(x = getdim("x", xyzs), y = getdim("y", xyzs),
        z = getdim("z", xyzs))
    if (is.character(xyz)) {
        bbox <- par3d("bbox")
        mins <- matrix(bbox[c(1, 3, 5)], nrow = nrow(xyz), ncol = 3,
            byrow = TRUE)
        maxs <- matrix(bbox[1 + c(1, 3, 5)], nrow = nrow(xyz),
            ncol = 3, byrow = TRUE)
        mids <- (mins + maxs)/2
        xyz[na2f(xyz == "min")] <- mins[na2f(xyz == "min")]
        xyz[na2f(xyz == "max")] <- maxs[na2f(xyz == "max")]
        xyz[na2f(xyz == "mid")] <- mids[na2f(xyz == "mid")]
        mode(xyz) <- "numeric"
    }
    a <- list(x = xyz)
    ret <- c(a, oargs)
    names(ret) <- sub("^col$","color",names(ret))
    # warning("Using an experimental version of args3d that changes argname from 'col' to 'color'")
    ret
}



