##
## p3d:disp.R
## 2011-12-22
##

#' @export
disp <-
function( x ) {
    cat("\n::: ",deparse(substitute(x)), " :::\n")
    print(x)
}

