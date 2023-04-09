
library(p3d)
d <- mtcars
head(d)

Init3d()

Plot3d()

Plot3d(mpg ~ hp + gear | cyl, d[1:10,],  sphere.size = 0)
Plot3d(rbind(c(0,0,0),c(1,1,1)),alpha = 0)
Points3d()
Axes3d()
Plot3d(mpg ~ hp + gear, d)



library(spida2)
fit <- lm(mpg ~ hp * cyl, d)
done <- dropone(fit)
head(done)
dim(d)
dim(done)
Plot3d(b_hp ~ b_cyl + mpg | cyl , done[1:10,], thr = 0)
Points3d(b_hp ~ b_cyl + mpg | cyl , done[11:15,], thr = 0)

Axes3d()
Plot3d(rbind(c(16, 4, -.2)), clear = F)

