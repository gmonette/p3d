##
## p3d:Evalf.R
## 2011-12-22
##


#' @export
Evalf <-
function( fun, data = newdata, newdata ) {
    # this is meant to evaluate a function much like predict 'evaluates' a model
    inds <- seq_along( rownames(data) )
    ret <- rep(NA, length(inds))
    for ( i in inds ) {
       ret[i] <- do.call(fun,data[i,])
    }
    ret
}

