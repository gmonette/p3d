##
## p3d:Text3d.R
## 2011-12-22
##

#' @export
Text3d <-
function( obj, ..., col = "blue") {
    # Note that this is the 'same' as Lines3d
    "
    Text3d allows arguments to be vectors or matrices whose corresponding
    axes as specified with names of the form 'x', 'xy', 'yxz', etc.
    The text argument is called 'texts'. Other key arguments are adj and
    justify
    "
    UseMethod("Text3d")
}
#
# Text3d.default <-
# function(...,col='blue'){
# # disp( list(...,col=col))
#  do.call("rgl.texts", args3d(...,col=col))
# }


#' @export
Text3d.default <-
function(...,col='blue'){
  a <- list(...,col=col)
  if( !any(grepl("^text$",names(a)))) names(a)[names(a)==''] <- 'text'
  do.call("rgl.texts", do.call(args3d,a))
}
