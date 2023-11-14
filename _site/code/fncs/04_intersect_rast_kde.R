# INTERSECT RASTER AND HRs
intersect_rast_kde <- function(rast_clc,rast_tcd, kde){
  
  ind_clc_rast <- list(data.frame())
  ind_tcd_rast <- list(data.frame())
  ind_diff_rast <- list(data.frame())
  ind_values_df <- list(data.frame())
  pops <- names(kde)
  for(i in 1:length(kde)){
    ind_clc <- list(data.frame())
    ind_tcd <- list(data.frame())
    ind_diff <- list(data.frame())
    ind_values <- list(data.frame())
    ind_kde <- list(data.frame())
    for(j in 1:nrow(kde[[i]])){
      # loopers
      kde_loop <- kde[[i]][j,]
      clc_loop <- rast_clc[[i]]
      tcd_loop <- rast_tcd[[i]]
      
      # extract for each individual the clc values 
      rast_c <- raster::crop(clc_loop, kde_loop)
      rast_m_clc <- raster::mask(rast_c, kde_loop)
      
      # extract for each individual the tcd values 
      rast_c <- raster::crop(tcd_loop, kde_loop)
      rast_m_tcd <- raster::mask(rast_c, kde_loop)
      
      # calculate the difference between both layers 
      rast_m_diff <- rast_m_clc - rast_m_tcd
      
      
      # compute the proportion of forest and open habitat with tcd 
      forestopen_tcd <- as.data.frame(table(factor(unlist(matrix(rast_m_tcd)),levels=c(0,1))))
      kde_loop$forest_tcd <- forestopen_tcd[which(forestopen_tcd$Var1 == 1),'Freq']
      kde_loop$open_tcd <- forestopen_tcd[which(forestopen_tcd$Var1 == 0),'Freq']  
      # compute the proportion of forest and open habitat with clc 
      forestopen_clc <- as.data.frame(table(factor(unlist(matrix(rast_m_clc)),levels=c(0,1))))
      kde_loop$forest_clc <- forestopen_clc[which(forestopen_clc$Var1 == 1),'Freq']
      kde_loop$open_clc <- forestopen_clc[which(forestopen_clc$Var1 == 0),'Freq']  
      
      #compute the mismatch total proportion
      df <- as.data.frame(table(factor(as.vector(unlist(matrix(rast_m_clc))) == as.vector(unlist(matrix(rast_m_tcd))), levels=c(TRUE,FALSE))))
      equal <- df[which(df$Var1 == TRUE),'Freq']
      notequal <- df[which(df$Var1 == FALSE),'Freq']
      kde_loop$diff <- equal/(equal+notequal)
      
      # compute the mismatch type
      df2 <- plyr::count(data.frame(clc=as.vector(unlist(matrix(rast_m_clc))), tcd=as.vector(unlist(matrix(rast_m_tcd)))))        
      if(length(df2[which(df2$clc == 0 & df2$tcd == 0),'freq']) == 0){
        kde_loop$clc0tcd0 <- 0 } else {
          kde_loop$clc0tcd0 <- df2[which(df2$clc == 0 & df2$tcd == 0),'freq']
        }
      if(length(df2[which(df2$clc == 1 & df2$tcd == 1),'freq']) == 0){
        kde_loop$clc1tcd1 <- 0 } else {
          kde_loop$clc1tcd1 <- df2[which(df2$clc == 1 & df2$tcd == 1),'freq']
        }
      if(length(df2[which(df2$clc == 0 & df2$tcd == 1),'freq']) == 0){
        kde_loop$clc0tcd1 <- 0 } else {
          kde_loop$clc0tcd1 <- df2[which(df2$clc == 0 & df2$tcd == 1),'freq']
        }
      if(length(df2[which(df2$clc == 1 & df2$tcd == 0),'freq']) == 0){
        kde_loop$clc1tcd0 <- 0 } else {
          kde_loop$clc1tcd0 <- df2[which(df2$clc == 1 & df2$tcd == 0),'freq']
        }
      
      # outputs 
      
      # table with the raster values for each individual 
      #ind_values[[j]] <- data.frame(clc=na.omit(values(rast_m_clc)), tcd=na.omit(values(rast_m_tcd)), diff=na.omit(values(rast_m_diff)), animals_id = kde[[i]]@data$id[j], population = names(kde)[i])
      ind_clc[[j]] <- rast_m_clc 
      ind_tcd[[j]] <- rast_m_tcd
      ind_diff[[j]] <- rast_m_diff 
      ind_kde[[j]] <- st_as_sf(kde_loop) 
      
      print(paste0('population ', i, ' individual', j))
    }
    ind_clc_rast[[i]] <- ind_clc
    ind_tcd_rast[[i]] <- ind_tcd
    ind_diff_rast[[i]] <- ind_diff
    #ind_values_df[[i]] <- do.call(rbind.data.frame,ind_values)
    kde[[i]] <- do.call(rbind.data.frame, ind_kde)
    kde[[i]]$population <- pops[i]
  }
  #values_all <- do.call(rbind.data.frame,ind_values_df)
  return(list(ind_clc_rast,ind_tcd_rast,ind_diff_rast, kde))
  # ind_clc_rast - rasters for clc per individual
  # ind_tcd_rast - raster for tcd per individual
  # ind_diff_rast - difference between tcd and clc 
  # ind_values_df - list of dataframes with all values
  # values all - all values of all animals and populations in one dataframe  
}
