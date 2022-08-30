##
##
##   p3d:  miscellaneous functions
##   2013-09-08
##   Georges Monette <georges@yorku.ca>
##



# eventual addition to p3d


#' @export
fg <- function(stay = TRUE) {
  Plot3d.par(stay=stay)
  rgl.bringtotop(stay = stay)
}

#' @export
fg_ <- function(stay = FALSE) {
  Plot3d.par(stay=stay)
  rgl.bringtotop(stay = stay)
}
