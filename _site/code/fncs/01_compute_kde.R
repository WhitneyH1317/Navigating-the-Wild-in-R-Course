# calculate individual HRs per population
kde <- function(spatial_dataset, grid=FALSE){
  
  if(grid==TRUE){
    xmin<- min(spatial_dataset@coords[,1])-100 ##found 100 to be absolute minimum that allowed kUD to get homerange area value for 100%
    xmax<- max(spatial_dataset@coords[,1])+100
    ymin<- min(spatial_dataset@coords[,2])-100
    ymax<- max(spatial_dataset@coords[,2])+100
    x <- seq(xmin,xmax,by=10)  # where "by = resolution" is the pixel size you desire 
    y <- seq(ymin,ymax,by=10) 
    xy <- expand.grid(x=x,y=y) 
    coordinates(xy) <- ~x+y 
    gridded(xy) <- TRUE 
  }
  
  # Extract KDEs
  spatial_dataset_kde <- spatial_dataset
  id <- factor(spatial_dataset_kde@data$aniyr)
  spatial_dataset_kde <- spatial_dataset_kde[,-c(1:ncol(spatial_dataset_kde@data))]
  spatial_dataset_kde@data$id <- id    
  
  if(grid==TRUE){
    roe_ud <- adehabitatHR::kernelUD(spatial_dataset_kde,h = "href", grid=xy)        
  } 
  if(grid==FALSE){
    roe_ud <- adehabitatHR::kernelUD(spatial_dataset_kde,h = "href", extent=1.5)        
  }
  roe_kde90_l <-  adehabitatHR::getverticeshr(roe_ud, 90)
  #roe_kde50_l[[i]] <-  adehabitatHR::getverticeshr(roe_ud, 50)
  
  roe_pop_l <- raster::buffer(aggregate(roe_kde90_l,dissolve=TRUE),100)
  
  return(list(roe_pop_l, roe_kde90_l))
}
