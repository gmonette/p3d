##
## p3d:Inc.R
## 2011-12-22
##


#' @export
Inc <-
function( n ) {
        n <- as.character(n)
        len <- nchar(n)
        np1 <- as.numeric(n) + 1
        np1c <- as.character(np1)
        len <- max( len, nchar(np1c))
        formatC( np1, flag = '0', width = len)
    }

