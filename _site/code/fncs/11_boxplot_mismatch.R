boxplot_mismatch <- function(figure, withbox=TRUE){
  # This function makes the mismatch plot. Withbox is used to add a background color. 
  layout(matrix(c(1,3,2,4),2,2))
  par(mar=c(1,2,2,1), oma=c(1,2,2,1))
  
  # KERNEL
  subset <- subset(kernel_data, aniyr %in% aniyr)
  #subset$clc0tcd0_prop <- subset$clc0tcd0/subset$total
  #subset$clc1tcd1_prop <- subset$clc1tcd1/subset$total
  #subset$clc0tcd1_prop <- subset$clc0tcd1/subset$total
  #subset$clc1tcd0_prop <- subset$clc1tcd0/subset$total
  
  boxplot(clc0tcd1_prop ~ time + species, data=subset, col=c('red','darkred','grey60','grey20'),ylim=c(0,0.7), pch=19) 
  confusion_scheme(xleft=4.020-0.520,ybottom=0.748,xright=4.418-0.650,ytop=0.794,col=c(AddAlpha('white',0.3),'darkblue',AddAlpha('white',0.3),AddAlpha('white',0.3)))
  if(withbox == TRUE){rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = AddAlpha('darkblue',0.1))}
  box(lwd=2, col='darkblue')
  points(x=c(1,2,3,4),y=aggregate(subset$clc0tcd1_prop, list(subset$time,subset$species), mean)$x, col= 'gold', pch=17)
  mtext('TCD Forest - CLC Open', font=1, padj=-0.5)
  
  boxplot(clc1tcd0_prop ~ time + species, data=subset, col=c('red','darkred','grey60','grey20'),ylim=c(0,0.7), pch=19) 
  if(withbox == TRUE){rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = AddAlpha('lightblue',0.1))}
  confusion_scheme(xleft=4.020-0.520,ybottom=0.748,xright=4.418-0.650,ytop=0.794,col=c('white','white','white','lightblue'))
  box(lwd=2, col='lightblue')
  points(x=c(1,2,3,4),y=aggregate(subset$clc1tcd0_prop, list(subset$time,subset$species), mean)$x, col= 'gold', pch=17)
  mtext('TCD Open - CLC Forest', font=1, padj=-0.5)
  
  mtext('Mismatch TCD and CLC', outer=TRUE, padj=-0.5, cex=1.1,font=2)
  
  par(xpd=FALSE)
  subset <- subset(point_data, aniyr %in% aniyr)
  #subset$clc0tcd0_prop <- subset$clc0tcd0/subset$total
  #subset$clc1tcd1_prop <- subset$clc1tcd1/subset$total
  #subset$clc0tcd1_prop <- subset$clc0tcd1/subset$total
  #subset$clc1tcd0_prop <- subset$clc1tcd0/subset$total
  
  boxplot(clc0tcd1_prop ~ time + species, data=subset, col=c('red','darkred','grey60','grey20'),ylim=c(0,0.7), pch=19) 
  if(withbox == TRUE){rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = AddAlpha('darkblue',0.1))} 
  box(lwd=2, col='darkblue')
  points(x=c(1,2,3,4),y=aggregate(subset$clc0tcd1_prop, list(subset$time,subset$species), mean)$x, col= 'gold', pch=17)
  
  boxplot(clc1tcd0_prop ~ time + species, data=subset, col=c('red','darkred','grey60','grey20'),ylim=c(0,0.7), pch=19)
  if(withbox == TRUE){rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = AddAlpha('lightblue',0.1))}
  box(lwd=2, col='lightblue')
  points(x=c(1,2,3,4),y=aggregate(subset$clc1tcd0_prop, list(subset$time,subset$species), mean)$x, col= 'gold', pch=17)
  
  mtext('KDE', 2,font=1,outer=TRUE, adj=0.75)
  mtext('GPS', 2,font=1,outer=TRUE, adj=0.225)
}