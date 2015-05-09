##
## p3d:p3d-internal.R
## 2011-12-22
##

p3d <- function() "Compiled 2011-12-19 15:37"



na.include  <- function (obj)
{
  # from library(Hmisc)
  if (inherits(obj, "data.frame"))
    for (i in seq(along = obj)) obj[[i]] <- na.include(obj[[i]])
  else {
    if (length(levels(obj)) && any(is.na(obj)))
      obj <- factor(obj, exclude = NULL)
  }
  obj
}
