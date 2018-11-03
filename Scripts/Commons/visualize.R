plot.embeddings <- function(x, y = NA, z = NA, radius = 0.02, col.point = "blue", col.axis = "grey",
                            xlab = "", ylab="", zlab="", 
                            show.bbox = FALSE, col.bbox = c("#333377","black"))
{
  if(is.na(y) || is.na(z)){
    x <- princomp(x, cor = FALSE)
    
    x <- as.matrix(x$scores)
    
    z <- x[,3]
    y <- x[,2]
    x <- x[,1]
  }


  require(rgl)
  
  #rgl_init()
  rgl.spheres(x, y, z, r = radius, color = col.point)
  
  lim_max <- function(x){c(-max(abs(x)), max(abs(x))) * 1.1}
  lim_min <- function(x){c(-min(abs(x)), min(abs(x))) * -1.1}
  
  # Add axes
  rgl.lines(c(0, lim_max(x)[2]), c(0, 0), c(0, 0), color = col.axis)
  rgl.lines(c(0, 0), c(0,lim_max(y)[2]), c(0, 0), color = col.axis)
  rgl.lines(c(0, 0), c(0, 0), c(0,lim_max(y)[2]), color = col.axis)
  
  # Add a point at the end of each axes to specify the direction
  axes <- rbind(c(lim_max(x), lim_min(y), lim_min(z)),
                c(lim_min(x), lim_max(y), lim_min(z)), 
                c(lim_min(x), lim_min(y), lim_max(z)))
  
  rgl.points(axes, color = col.axis, size = 3)
  
  # Add axis labels
  rgl.texts(axes, text = c(xlab, ylab, zlab), color = col.axis,
            adj = c(0.5, -0.8), size = 2)
  
  # Add bounding box decoration
  if(show.bbox){
    rgl.bbox(color=c(col.bbox[1],col.bbox[2]), alpha = 0.5, 
             emission=col.bbox[1], specular=col.bbox[1], shininess=5, 
             xlen = 3, ylen = 3, zlen = 3) 
  }
}