\name{Smoking}
\Rdversion{1.1}
\alias{Smoking}
\concept{Simpson's paradox}
\concept{causal infefence}
\concept{observational data}
\concept{ecological correlation}
\docType{data}
\title{
Cigarette consumption and life expectancy by country
}
\description{
The variables for this data set were assembled from various web sources. They consist of
life expectancy, cigarette consumption per capita and health expenditures per capita in 2004.
The relationship between life expectancy and cigarette consumption is paradoxical leading to
reflection on omitted variables and problems of causal inference with observational and aggregated
data.  The association between life expectancy and cigarette consumption could be
termed an 'ecological correlation'.
}
\usage{data(Smoking)}
\format{
  A data frame with 192 observations on the following 10 variables.
  \describe{
    \item{\code{Country}}{a factor with 192 levels \code{Afghanistan} \code{Albania} \code{Algeria} \code{Andorra} ...}
    \item{\code{Continent}}{a factor with 6 levels \code{Africa} \code{Asia} \code{Australia} \code{Europe} \code{North America} \code{South America}}
    \item{\code{LE}}{Life expectancy (combining sexes -- separate would be much more interesting), a numeric vector}
    \item{\code{CigCon}}{Annual cigarette consumption per capita, a numeric vector}
    \item{\code{LE.q}}{Life expectancy quartile, a factor with levels \code{(28.6,40.2]} \code{(40.2,51.8]} \code{(51.8,63.4]} \code{(63.4,75]}}
    \item{\code{Cont}}{short labels for Continent, a factor with levels \code{Afrc} \code{Asia} \code{Astr} \code{Eurp} \code{NrtA} \code{SthA}}
    \item{\code{Cont2}}{short labels for Continent, a factor with levels \code{Africa} \code{Asia} \code{Australia} \code{Europe} \code{N.America} \code{S.America}}
    \item{\code{HealthExpPC}}{Health expenditure per capita in US $, a numeric vector}
    \item{\code{Year}}{relevant year -- all 2004, a numeric vector}
    \item{\code{HE}}{Health expenditure quartile, a factor with levels \code{(152,476]} \code{(31.1,152]} \code{(476,6.1e+03]} \code{[0.3,31.1]}}
  }
}

\details{
Each row consists of the data for one country.
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
http://www.nationmaster.com
}

%\references{
%%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
\dontrun{
data(Smoking)
summary(Smoking)
head(Smoking)
fit1 <- lm( LE ~ CigCon, Smoking)
fit2 <- lm( LE ~ CigCon + HealthExpPC, Smoking)
summary(fit1)
summary(fit2) 
anova(fit1,fit2)

rownames(Smoking) <- Smoking$Country

Init3d(family = 'serif', cex = 1.5)
Plot3d( LE ~ CigCon + HealthExpPC | Continent, Smoking)
Axes3d()
Id3d(pad=1)

Fit3d(fit1)
Fit3d(fit2, col = 'green')

fit = lm( LE ~ CigCon + log(HealthExpPC) +I(CigCon^2) + I(log(HealthExpPC)^2) + I(CigCon*log(HealthExpPC)), Smoking)
Fit3d( fit , col = 'red')

# HealthExpPC is highly 'skewed': dense on left, long tail on right
require( lattice )
densityplot( Smoking$HealthExpPC )

# It can be useful to use a transformation to make 
# spread more even
#  e.g. log
# First make sure all values are positive:

sort( Smoking$HealthExpPC)

# Try a log transformation: 

Smoking$LogHE <- log(Smoking$HealthExpPC)    # create log HE

densityplot( Smoking$LogHE )

# Also useful to have categories:

Smoking$HECat <- cut(Smoking$LogHE, 7)       # create categories
summary(Smoking)

Plot3d( LE ~ CigCon + LogHE |Continent, Smoking )  # condition on level of HEpC
Axes3d()
Ell3d()
# Id3d(pad=1)

# Simple regression

Plot3d( LE ~ CigCon + LogHE |Continent, Smoking )  
fit.lin <- lm( LE ~ CigCon, Smoking)
Fit3d( fit.lin )

# Adjusting for LogHE with a linear model

fit.lin2 <- lm( LE ~ CigCon + LogHE, Smoking)
Fit3d( fit.lin2 , col = 'red')

Ell3d( use.groups = FALSE, partial = 3,
       partial.col = 'red', partial.lwd = 3)

# Note that the marginal model is determined by
# the projection of the 3d data ellipsoid on the 
# LE-CigCon plane.
# The slope wrt CigCon of the partial model 
# adjusting for LogHE is determined by the 
# red ellipse that is the frontal section of
# the 3d data ellipsoid through its center.
# 
# Simpson's paradox arises when the tilt of the
# frontal shadow of the 3d ellipsoid has the opposite
# direction to the tilt of its frontal section.
#
# i.e. the shadow and the slice of the ellipsoid
# tilt in different directions

# Stratifying on Continent
# No interaction

Plot3d( LE ~ CigCon + LogHE |Continent, Smoking )  
fit.add <- lm( LE ~ CigCon+Continent, Smoking)
Fit3d(fit.add)
Fit3d(fit.lin, col = 'black')

# With interaction

Plot3d( LE ~ CigCon + LogHE |Continent, Smoking ) 
fit.int<- lm( LE ~ CigCon*Continent, Smoking)
Fit3d( fit.int )

# Marginal without Africa

Plot3d( LE ~ CigCon + LogHE |Continent, Smoking ) 
fit.lin2 <- lm(LE ~ CigCon, subset(Smoking, Continent != "Africa"))
Fit3d( fit.lin2)


# Use multiple regression with quadratic surface

Plot3d( LE ~ CigCon + LogHE |Continent, Smoking ) 
fit = lm( LE ~ CigCon + I(LogHE^2) +I(CigCon^2) + I(LogHE^2) + I(CigCon*LogHE), Smoking)
Fit3d( fit, col = 'pink' )
# Pop3d(2)

# refit omitting Africa:

fit.na = lm( LE ~ CigCon + I(LogHE^2) +I(CigCon^2) + I(LogHE^2)
             + I(CigCon*LogHE), Smoking, subset = Continent != "Africa")
Fit3d( fit.na, col = 'red' )

# Marginal relationship

Pop3d(4)    # pop twice for each fit
fit.quad <- lm( LE ~ CigCon + I(CigCon^2) , Smoking)
Fit3d( fit.quad, col = 'green')

#  A quadratic surface within each Continent (overfitting?!)

fit2 = lm( LE ~ Continent*(CigCon + LogHE +I(CigCon^2) +
                             I(LogHE^2) + I(CigCon*LogHE)), Smoking)
Fit3d( fit2 )
}

}
\keyword{datasets}
