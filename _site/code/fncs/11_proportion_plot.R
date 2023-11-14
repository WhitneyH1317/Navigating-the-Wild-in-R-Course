proportion_plot <- function(){ 
  layout(matrix(c(1,3,2,4),2,2))
  par(mar=c(1,2,2,1), oma=c(1,2,2,1))
  
  # KERNEL
  subset <- subset(kernel_data, aniyr %in% aniyr) # SUBSET
  boxplot(clc_forest_prop ~ time + species, data=subset, col=c('red','darkred','grey60','grey20')) # CLC
  mtext('CLC', font=1, padj=-0.5)
  boxplot(tcd_forest_prop ~ time + species, data=subset, col=c('red','darkred','grey60','grey20')) # TCD
  mtext('TCD', font=1, padj=-0.5)
  
  # POINTS
  subset <- subset(point_data, aniyr %in% aniyr) # SUBSET
  boxplot(clc_forest_prop ~ time + species, data=subset, col=c('red','darkred','grey60','grey20')) # CLC
  boxplot(tcd_forest_prop ~ time + species, data=subset, col=c('red','darkred','grey60','grey20')) # TCD
  mtext('KDE', 2,font=1,outer=TRUE, adj=0.75)
  mtext('GPS', 2,font=1,outer=TRUE, adj=0.225)
  
  mtext('Proportion of Forest', outer=TRUE, padj=-0.5, cex=1.1,font=2)
}
