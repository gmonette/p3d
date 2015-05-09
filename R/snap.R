##
## p3d:snap.R
## 2011-12-22
##


snap <-
function(  n = Plot3d.par('snap.n'), fn = Plot3d.par('snap.fn'), suffix = "", inc = TRUE, fmt = "png") {
    pos <- rgl.cur()
    if ( is.null(n)) Plot3d.par( snap.n = n <- 0)
    if ( is.null(fn)) Plot3d.par( snap.fn = fn <- "Movie")
    filebase <- paste(fn,pos,sep='')     
    filename <- paste( filebase, sprintf("%05d",n), suffix, ".", fmt , sep = "")
    rgl.snapshot( filename, fmt = fmt)
    if (inc) Plot3d.par(snap.n = n + 1)
    Plot3d.par(snap.fn = fn)
    invisible( filename )
}

