# RECLASSIFY CLC
reclass_clc <- function(rast, clc_classes = c(311,312,313), value_forest= 1, value_open=0){
  rast_reclass <- rast
  rast_reclass[rast_reclass %in% clc_classes] <- value_forest
  rast_reclass[rast_reclass >1] <- value_open
  return(rast_reclass)
}

# RECLASSIFY TCD  
reclass_tcd <- function(rast, perc = 10, value_forest = 1, value_open = 0){
  rast_reclass <- rast 
  rast_reclass[rast_reclass < (perc+1)] <- value_open
  rast_reclass[rast_reclass > perc] <- value_forest
  return(rast_reclass)
}
