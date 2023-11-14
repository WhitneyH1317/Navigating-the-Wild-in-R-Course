# count the number of patches used in clc and in tcd 
patch_statistics <- function(dataset, patchsize){
  # count the number of patches used in clc and in tcd 
  patch <- plyr::count(unique(dataset[,c('TCD_forest_patch_size','animals_id')])[,c('animals_id')])
  colnames(patch) <- c('animals_id', 'TCD_F')
  patch$CLC_F <- plyr::count(unique(dataset[,c('CLC_forest_patch_size','animals_id')])[,c('animals_id')])$freq
  patch$TCD_O <- plyr::count(unique(dataset[,c('TCD_open_patch_size','animals_id')])[,c('animals_id')])$freq
  patch$CLC_O <- plyr::count(unique(dataset[,c('CLC_open_patch_size','animals_id')])[,c('animals_id')])$freq
  # count the number of fixes in patches smaller than 25 hectare 
  dataset$TCD_forest_patch_25m <- dataset$TCD_forest_patch_size < patchsize
  dataset$CLC_forest_patch_25m <- dataset$CLC_forest_patch_size < patchsize
  dataset$CLC_open_patch_25m <- dataset$CLC_open_patch_size < patchsize
  dataset$TCD_open_patch_25m <- dataset$TCD_open_patch_size < patchsize
  patch25m_TCD_F <- as.data.frame(proportions(table(dataset[which(is.na(dataset$TCD_forest_patch_size) == FALSE),c('animals_id','TCD_forest_patch_25m')]),1))
  colnames(patch25m_TCD_F) <- c('animals_id', 'patchsize','Freq')
  patch25m_TCD_F$variable <- 'TCD_F'
  patch25m_CLC_F  <- as.data.frame(proportions(table(dataset[which(is.na(dataset$CLC_forest_patch_size) == FALSE),c('animals_id','CLC_forest_patch_25m')]),1))
  colnames(patch25m_CLC_F) <- c('animals_id', 'patchsize','Freq')
  patch25m_CLC_F$variable <- 'CLC_F'
  patch25m_TCD_O  <- as.data.frame(proportions(table(dataset[which(is.na(dataset$TCD_open_patch_size) == FALSE),c('animals_id','TCD_open_patch_25m')]),1))
  colnames(patch25m_TCD_O) <- c('animals_id', 'patchsize','Freq')
  patch25m_TCD_O$variable <- 'TCD_O'
  patch25m_CLC_O  <- as.data.frame(proportions(table(dataset[which(is.na(dataset$CLC_open_patch_size) == FALSE),c('animals_id','CLC_open_patch_25m')]),1))
  colnames(patch25m_CLC_O) <- c('animals_id', 'patchsize','Freq')
  patch25m_CLC_O$variable <- 'CLC_O'
  patch25m <- rbind(patch25m_TCD_F,patch25m_CLC_F,patch25m_TCD_O,patch25m_CLC_O)
  patch25m <- subset(patch25m, patchsize == TRUE)
  return(list(patch,patch25m))
}
