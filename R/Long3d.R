##
## p3d:Long3d.R
## 2011-12-22
##

Long3d <-
function( fmla, data, id, ..., col.lines = 'grey', lwd.lines = 1) {
      help <- "
      OBSOLETE: use Plot3d( ...., groups = id)
      
      Long3d( y ~ x + z | g, data, groups = id)
         will join points with the same 'groupid' with lines to
         show trajectories for longitudinal data.
         This function is still very rudimentary.
     
      "
         Plot3d( fmla, data, ...)
          lapply( split( 1:nrow(data), data[[id]]), function( ind ) {
              dz <- data[ ind,]
               fmla <- as.formula(paste( sub("\\|","+",as.character(fmla))[c(2,1,3)],collapse =""))
               Lines3d( yxz = model.frame( fmla, dz)[,1:3],col = col.lines,
                  lwd = lwd.lines)
         }
        )
        invisible(NULL)    
    }

