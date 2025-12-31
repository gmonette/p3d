##
##
##   p3d:  miscellaneous functions
##   2013-09-08
##   Georges Monette <georges@yorku.ca>
##



# eventual addition to p3d


#' @export
fg <- function(stay = TRUE) {
  if (.Platform$OS.type != "windows") {
    rgl.bringtotop()
  } else {
    rgl.bringtotop(stay = stay)
    Plot3d_par(stay=stay)
  }
}

#' @export
fg_ <- function(stay = FALSE) {
  Plot3d_par(stay=stay)
  rgl.bringtotop(stay = stay)
}
