confusion_scheme <- function(xleft=4.160,ybottom=0.630,xright=4.660,ytop=0.680,
                             col=c('darkgreen','darkblue','green','lightblue'), xpd=NA){
  # Plots a 2x2 confusion matrix scheme in your plot window. x and y values are based on 
  # axis values. You can specify the colors from upper left, upper right, lower left, lower right
  
  par(xpd=xpd) # xpd NA or FALSE, if NA you can also plot outside the plot window
  
  ymid <- (ytop+ybottom)/2 # define the middle along y axis
  xmid <- (xright+xleft)/2 # define the middle along x axis
  rect(xleft,ybottom,xright,ytop) # original rectangle (redundant)
  rect(xleft,ymid,xmid,ytop, col= col[1]) # upperleft
  rect(xmid,ymid,xright,ytop, col= col[2]) # upperright
  
  rect(xmid,ybottom,xright,ymid, col= col[3]) # lowerleft
  rect(xleft,ybottom,xmid,ymid, col= col[4]) # lowerright
}


