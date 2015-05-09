##
## p3d:cell.R
## 2011-12-22
##

## Last modified: 6-14-2009 by Michael Friendly
#  - combined cell.R, cell.default, cell.glh into a single file
#  - renamed first argument to 'object' for consistency as S3 methods 

## TODO:  Since the ellipses aren't plotted directly, it's not clear why arguments such as
#         las, lwd, lty, etc. are needed here.  These would normally be used only if the add
#         argument was implemented in the code for cell.default.

cell <-
function(object, ... )  {
            UseMethod("cell")
            help <- "
            See help for car::confidence.ellipse.lm
            except that 'cell' returns the points to form the ellipse
            which must be plotted with plot(...,type='l') or lines(...)
            -- Use dfn to determine Sheffe dimension, i.e. dfn = 1 to generate ordinary CIs, dfn = 2 for 2-dim CE, etc.
            -- TODO: extend to 3 dimensions if which.coef has length 3
            "
    }


cell.default <-
function (object, which.coef, levels = 0.95, Scheffe = FALSE, dfn = 2,
					center.pch = 19, center.cex = 1.5, segments = 51, xlab, ylab,
					las = par("las"), col = palette()[2], lwd = 2, lty = 1,
					add = FALSE, ...)
	{
		
		which.coef <- if (length(coefficients(object)) == 2)
					c(1, 2)
				else {
					if (missing(which.coef)) {
						if (any(names(coefficients(object)) == "(Intercept)"))
							c(2, 3)
						else c(1, 2)
					}
					else which.coef
				}
		coef <- coefficients(object)[which.coef]
		xlab <- if (missing(xlab))
			paste(names(coef)[1], "coefficient")
		ylab <- if (missing(ylab))
			paste(names(coef)[2], "coefficient")
		if(missing(dfn)) {
## FIXME: df.terms below is considered a global (undefined) object
			if (Scheffe) dfn <- sum(df.terms(object))
			else 2
		}
		dfd <- df.residual(object)
		shape <- vcov(object)[which.coef, which.coef]
		ret <- numeric(0)
		
		ret <- ell( coef, shape, sqrt(dfn * qf(levels, dfn, dfd)))
		colnames(ret) <- c(xlab, ylab)
		ret
	}
	
cell.glh <-
function (object, which.coef = 1:2, levels = 0.95, Scheffe = FALSE, dfn = 2,
					center.pch = 19, center.cex = 1.5, segments = 51, xlab, ylab,
					las = par("las"), col = palette()[2], lwd = 2, lty = 1,
					add = FALSE, ...)
	{
		
		# BUGS: works only on first element of glh list
		# glh should be restructured to have two classes: waldList and wald
		
		obj <- object[[1]]
		coef <- obj$coef[which.coef]
		xlab <- if (missing(xlab))
			paste(names(coef)[1], "coefficient")
		ylab <- if (missing(ylab))
			paste(names(coef)[2], "coefficient")
		
		dfd <- obj$anova$denDF
		shape <- obj$vcov[which.coef, which.coef]
		ret <- ell( coef, shape , sqrt( dfn * qf( levels, dfn, dfd)))
		colnames(ret) <- c(xlab, ylab)
		ret
	}
	
	