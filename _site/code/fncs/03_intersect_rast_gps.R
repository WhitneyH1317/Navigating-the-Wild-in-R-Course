# INTERSECT RASTER AND POINTS
intersect_rast_gps <- function(rast_clc,rast_tcd, points){ 
  gpsdata <- split(points, f=points$study_areas_id)
  # points are gps locations 
  # rast clc is list with rasters per population 
  # rast tcd is list with rasters per population 
  
  # extract per population
  for(i in 1:length(gpsdata)){
    gpsdata[[i]]@data$clc <- extract(rast_clc[[i]], gpsdata[[i]])
    gpsdata[[i]]@data$tcd <- extract(rast_tcd[[i]], gpsdata[[i]])
    gpsdata[[i]] <- st_as_sf(gpsdata[[i]])
  }
  gpsdata_df <- do.call(rbind.data.frame, gpsdata)
  
  # for the few locations that do not intersect with the spatial layer use the values from the database 
  gpsdata_df[which(is.na(gpsdata_df$clc) & gpsdata_df$corine_land_cover_2012_vector_code %in% c(311,312,313)), 'clc'] <- 1
  gpsdata_df[which(is.na(gpsdata_df$clc)), 'clc'] <- 0
  gpsdata_df[which(is.na(gpsdata_df$tcd) & gpsdata_df$corine_land_cover_2012_vector_code < 11), 'tcd'] <- 0
  gpsdata_df[which(is.na(gpsdata_df$tcd)), 'tcd'] <- 1
  gpsdata_df$aniyr_time <- paste0(gpsdata_df$aniyr, gpsdata_df$mid_or_noon)
  pts <- unique(as(gpsdata_df,'Spatial')@data [,c('animals_id','aniyr','study_areas_id','sex','mid_or_noon','aniyr_time')])
  
  points_l <- split(gpsdata_df, f=gpsdata_df$aniyr_time)
  
  for(i in 1:length(points_l)){
    df2 <- plyr::count(data.frame(clc=points_l[[i]]$clc, tcd=points_l[[i]]$tcd))
    points_loop <- points_l[[i]] 
    if(length(df2[which(df2$clc == 0 & df2$tcd == 0),'freq']) == 0){
      points_loop$clc0tcd0 <- 0 } else {
        points_loop$clc0tcd0 <- df2[which(df2$clc == 0 & df2$tcd == 0),'freq']
      }
    if(length(df2[which(df2$clc == 1 & df2$tcd == 1),'freq']) == 0){
      points_loop$clc1tcd1 <- 0 } else {
        points_loop$clc1tcd1 <- df2[which(df2$clc == 1 & df2$tcd == 1),'freq']
      }
    if(length(df2[which(df2$clc == 0 & df2$tcd == 1),'freq']) == 0){
      points_loop$clc0tcd1 <- 0 } else {
        points_loop$clc0tcd1 <- df2[which(df2$clc == 0 & df2$tcd == 1),'freq']
      }
    if(length(df2[which(df2$clc == 1 & df2$tcd == 0),'freq']) == 0){
      points_loop$clc1tcd0 <- 0 } else {
        points_loop$clc1tcd0 <- df2[which(df2$clc == 1 & df2$tcd == 0),'freq']
      }
    points_loop$total <- rowSums(as(points_loop,'Spatial')@data[,c('clc0tcd0','clc1tcd1','clc0tcd1','clc1tcd0')])
    points_loop$tcd_open_abs <- rowSums(as(points_loop,'Spatial')@data[,c('clc0tcd0','clc1tcd0')])
    points_loop$tcd_forest_abs <- rowSums(as(points_loop,'Spatial')@data[,c('clc1tcd1','clc0tcd1')])
    points_loop$clc_open_abs <- rowSums(as(points_loop,'Spatial')@data[,c('clc0tcd1','clc0tcd0')])
    points_loop$clc_forest_abs <- rowSums(as(points_loop,'Spatial')@data[,c('clc1tcd0','clc1tcd1')])
    
    points_prop <- as(points_loop,'Spatial')@data[,c('clc0tcd0','clc1tcd1','clc0tcd1','clc1tcd0','tcd_open_abs','tcd_forest_abs','clc_open_abs','clc_forest_abs')]/points_loop$total
    colnames(points_prop) <- paste0(gsub('_abs','',colnames(points_prop)),'_prop')
    points_loop <- cbind.data.frame(points_loop, points_prop)            
    
    
    points_l[[i]] <- points_loop
    
    
  }
  points_calc <- do.call(rbind.data.frame, points_l)
  
  return(points_calc)
}
