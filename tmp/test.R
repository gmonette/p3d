## Not run:
library(p3d)
data(Smoking)
head(Smoking)
rownames(Smoking) = Smoking$Country

Init3d(family = 'serif', cex = 1.5)
Plot3d( LE ~ CigCon + HealthExpPC | Continent, Smoking) %>% system.time
Axes3d()
# Identify3d(pad=1)
Id3d()

if(FALSE){
  Text3d(xyz=c(2000,50,3000), text = 'HERE')
  Pop3d()
  Text3d(x=2000,yz=c(50,3000), text = 'HERE')
  Text3d(x=2000,zy=c(3000,50), text = 'HERE')
}



fit <- lm( LE ~ CigCon + log(HealthExpPC) +I(CigCon^2) + I(log(HealthExpPC)^2) + I(CigCon*log(HealthExpPC)), Smoking)
Fit3d( fit , grid.lines = 101, grid = F)
fitl <- lm( LE ~ CigCon + HealthExpPC, Smoking)
Fit3d( fitl, col = 'pink')
# HEpCap is highly 'skewed': dense on left, long tail on right
require( lattice )
densityplot( Smoking$HealthExpPC )

# Useful to use a transformation to make spread more even
#  e.g. log
# First make sure all values are positive:

sort( Smoking$HealthExpPC)

# Do log transformation:

Smoking$LogHE <- log(Smoking$HealthExpPC)    # create log HE

densityplot( Smoking$LogHE )

# Also usefult to have categories:

Smoking$HECat <- cut(Smoking$LogHE, 7)       # create categories
summary(Smoking)

Plot3d( LE ~ CigCon + LogHE |HECat, Smoking )  # condition on level of HEpC
Axes3d()
Ell3d()
Identify3d(pad=1)

# Simple regression

fit.lin <- lm( LE ~ CigCon, Smoking)
Fit3d( fit.lin )
fit.lin2 <- lm(LE ~ CigCon, subset(Smoking, Continent != "Africa"))
Fit3d( fit.lin2)
Pop3d(4)

# Use multiple regression (advanced version with quadratic surface)

fit = lm( LE ~ CigCon + I(LogHE^2) +I(CigCon^2) + I(LogHE^2) + I(CigCon*LogHE), Smoking)
Fit3d( fit, col = 'red' )
Pop3d(2)

# refit omitting Africa:

fit.na = lm( LE ~ CigCon + I(LogHE^2) +I(CigCon^2) + I(LogHE^2)
             + I(CigCon*LogHE), Smoking, subset = Continent != "Africa")
Fit3d( fit.na, col = 'red' )

# Marginal relationship

Pop3d(4)    # pop twice for each fit
fit.quad <- lm( LE ~ CigCon + I(CigCon^2) , Smoking)
Fit3d( fit.quad, col = 'green')

#  A quadratic surface within each Continent (overfitting?!)

fit2 = lm( LE ~ Continent*(CigCon + HealthExpPC +I(CigCon^2) +
                             I(HealthExpPC^2) + I(CigCon*HealthExpPC)), Smoking)
Fit3d( fit2 )

#  For a 2D view of the y versus x

view3d(0,0,0)   # note the first parameter, theta, rotates clockwise in the vertical axis
# from the initial position given by view3d(0,0,0); the second
# parameter, phi, tilts the graph around a horizontal screen axis
# towards the viewer.  The third parameter, fov, affects the
# amount of 'perspective'. fov = 0 yields an orthographic
# projection

## End(Not run)
