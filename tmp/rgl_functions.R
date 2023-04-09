# RGL translations from rgl.fun to fun3d
#' Call rgl::fun3d function from rgl.fun function
#'
#' Provide calls to deprecated rgl.fun functions.
#'
#' See [rgl.text3d()]
#' @export
rgl.texts <- function (x, y = NULL, z = NULL, text, adj = 0.5, pos = NULL,
                  offset = 0.5, family = par3d("family"), font = par3d("font"),
                  usePlotmath = is.language(texts),
                  cex = par3d("cex"), useFreeType = par3d("useFreeType"), ...) {

  rgl::text3d(x, y = y, z = y, texts = text, adj = adj, pos = pos,
            offset = offset, usePlotmath = usePlotmath, family = family,
            font = font, cex = cex, useFreeType = useFreeType,
            ...)

}
#' @export
rgl_texts <- function (x, y = NULL, z = NULL, text, adj = 0.5, pos = NULL,
                       offset = 0.5, family = par3d("family"), font = par3d("font"),
                       usePlotmath = is.language(texts),
                       cex = par3d("cex"), useFreeType = par3d("useFreeType"), ...) {

  rgl::text3d(x, y = y, z = y, texts = text, adj = adj, pos = pos,
              offset = offset, usePlotmath = usePlotmath, family = family,
              font = font, cex = cex, useFreeType = useFreeType,
              ...)

}
