forest_proportion_barboxplot<- function(){
  
  layout(matrix(c(rep(c(1,1,1,1,3,3,3,3,5,5,5,5,7,7),4),rep(c(2,2,2,2,4,4,4,4,6,6,6,6,8,8),4)),14,8))
  par(mar=c(1,1,1,1), oma=c(2,3,2,3))
  prop <- aggregate(. ~ popname + time ,data=point_data[, c('popname','total','time','clc0tcd0','clc1tcd0','clc1tcd1','clc0tcd1')],sum)
  prop$clc0tcd0 <- prop$clc0tcd0/prop$total
  prop$clc1tcd0 <- prop$clc1tcd0/prop$total
  prop$clc1tcd1 <- prop$clc1tcd1/prop$total
  prop$clc0tcd1 <- prop$clc0tcd1/prop$total
  prop <- plyr::join(prop,roered_pops_df[,c('ord','popname')], type='left', by='popname')
  prop <- prop[order(prop$ord, prop$time),]
  a <- t(as.matrix(prop[,c('clc1tcd1','clc0tcd1','clc0tcd0','clc1tcd0')]))
  colnames(a) <- paste0(prop$popname, '_',prop$time)
  bb <- barplot(a[1:2,], las=2, space=c(0,rep(c(0.1,0.5),10))[1:20], col=c('forestgreen','blue'), ylim=c(0,1), axes=FALSE,xaxt='n', border=NA)
  #axis(1, bb, labels= rep(c('d','n'),10), las=1,cex=0.5, tick=FALSE, col.ticks='white')
  mtext('Population-level',side=2, padj=-4.7, cex=0.7)
  mtext('Forest Proportion (GPS)',side=2, padj=-3.2, cex=0.7)
  mtext('TCD',side=3, padj=-1)
  axis(2, c(0,0.5,1),c(0,0.5,1))
  # text(bb[seq(1,length(bb),2)]+0.5, par("usr")[3]-0.001, 
  #      srt = 50, adj= 1, xpd = TRUE,
  #      labels= gsub('[0-9].*','',unique(paste0(prop$popname,prop$ord))))
  for (j in seq(1,length(bb),2)){lines(c(bb[j]-0.40,bb[j+1]+0.45), c(0,0),lwd=4,col=rep(c('grey70','darkred'),each=10)[j])}
  
  bb <- barplot(a[c(1,4),], las=2,space=c(0,rep(c(0.1,0.5),10))[1:20], col=c('forestgreen','lightblue'), ylim=c(0,1), axes=FALSE,xaxt='n', border=NA)
  #axis(1, bb, labels= rep(c('d','n'),10), las=1,cex=0.5, tick=FALSE, col.ticks='white')
  axis(2, c(0,0.5,1),c(0,0.5,1))
  # text(bb[seq(1,length(bb),2)]+0.5, par("usr")[3]-0.45, 
  #      srt = 50, adj= 1, xpd = TRUE,
  #      labels= gsub('[0-9].*','',unique(paste0(prop$popname,prop$ord))))
  for (j in seq(1,length(bb),2)){lines(c(bb[j]-0.40,bb[j+1]+0.45), c(0,0),lwd=4,col=rep(c('grey70','darkred'),each=10)[j])}
  mtext('CLC',side=3, padj=-1)
  
  
  #  par(mar=c(3,2,3,0))
  point_data <-point_data[order(point_data$ord,point_data$time,decreasing=TRUE),]
  bb <- boxplot(tcd_forest_prop~time+ord, data=point_data,boxwex=0.4, medlwd=3,las=2, outpch=19,outcol='darkgrey',col=c(rep(c('lightgrey','grey50'),5),rep(c('#fee0d2','brown3'),5)), lwd=0.25, axes=FALSE,xaxt='n')
  tcdpopmean <- aggregate(. ~time+ord,data=point_data[,c('ord','time','tcd_forest_prop')],mean)
  tcdpopsd <- aggregate(. ~time+ord,data=point_data[,c('ord','time','tcd_forest_prop')],sd)
  mtext('Individual Variability',side=2, padj=-4.7, cex=0.7)
  mtext('Forest Proportion (GPS)',side=2, padj=-3.2, cex=0.7)  
  axis(2, c(0,0.5,1),c(0,0.5,1))
  for (j in seq(1,length(bb$out),2)){lines(sort(c(seq(1,20,2)+0.01,seq(2,20,2)-0.01 ))[j:(j+1)], c(0,0),lwd=2,col=rep(c('grey60','darkred'),each=10)[j])}
  boxplot(tcd_forest_prop~time+ord, data=point_data,boxwex=0.4, medlwd=3,las=2, add=TRUE, col=c(rep(c('lightgrey','grey50'),5),rep(c('#fee0d2','brown3'),5)), lwd=0.25, axes=FALSE,xaxt='n')
  points(tcdpopmean$tcd_forest_prop, col= rep(c('red','blue'),10), pch=19)
  
  boxplot(clc_forest_prop~time+ord, data=point_data, las=2, boxwex=0.4,medlwd=3,outpch=19,outcol='darkgrey',col=c(rep(c('lightgrey','grey50'),5),rep(c('#fee0d2','brown3'),5)), lwd=0.25, axes=FALSE,xaxt='n')
  clcpopmean <- aggregate(. ~time+ord,data=point_data[,c('ord','time','clc_forest_prop')],mean)
  clcpopsd <- aggregate(. ~time+ord,data=point_data[,c('ord','time','clc_forest_prop')],sd)
  axis(2, c(0,0.5,1),c(0,0.5,1))
  # text(seq(1,length(bb$out),2), par("usr")[3]-0.25, 
  #      srt = 50, adj= 1, xpd = TRUE,
  #      labels= gsub('[0-9].*','',unique(paste0(prop$popname,prop$ord))))
  for (j in seq(1,length(bb$out),2)){lines(sort(c(seq(1,20,2)+0.01,seq(2,20,2)-0.01 ))[j:(j+1)], c(0,0),lwd=2,col=rep(c('grey60','darkred'),each=10)[j])}
  boxplot(clc_forest_prop~time+ord, data=point_data, las=2, boxwex=0.4,add=TRUE,medlwd=3,col=c(rep(c('lightgrey','grey50'),5),rep(c('#fee0d2','brown3'),5)), lwd=0.25, axes=FALSE,xaxt='n')
  points(clcpopmean$clc_forest_prop,  col= rep(c('red','blue'),10), pch=19)
  
  # par(mar=c(4,2,2,0))
  #levels(kernel_data$popname) <- levels(point_data$popname)
  kernel_data <-kernel_data[order(kernel_data$ord,kernel_data$time,decreasing=TRUE),]
  boxplot(tcd_forest_prop~time+ord, data=kernel_data, las=2, boxwex=0.4,medlwd=3,outpch=19,outcol='darkgrey',col=c(rep(c('lightgrey','grey50'),5),rep(c('#fee0d2','darkred'),5)), lwd=0.25, axes=FALSE,xaxt='n')
  tcdpopmean <- aggregate(. ~time+ord,data=kernel_data[,c('ord','time','tcd_forest_prop')],mean)
  tcdpopsd <- aggregate(. ~time+ord,data=kernel_data[,c('ord','time','tcd_forest_prop')],sd)
  mtext('Individual Variability',side=2, padj=-4.7, cex=0.7)
  mtext('Forest Proportion (KDE)',side=2, padj=-3.2, cex=0.7)  
  axis(2, c(0,0.5,1),c(0,0.5,1))
  axis(2, c(0,0.5,1),c(0,0.5,1))
  text(seq(1,length(bb$out),2), par("usr")[3]-0.25, 
       srt = 50, adj= 1, xpd = TRUE,
       labels= gsub('[0-9].*','',unique(paste0(prop$popname,prop$ord))))
  for (j in seq(1,length(bb$out),2)){lines(sort(c(seq(1,20,2)+0.01,seq(2,20,2)-0.01 ))[j:(j+1)], c(0,0),lwd=2,col=rep(c('grey60','darkred'),each=10)[j])}
  boxplot(tcd_forest_prop~time+ord, data=kernel_data, las=2, boxwex=0.4,add=TRUE,medlwd=3,col=c(rep(c('lightgrey','grey50'),5),rep(c('#fee0d2','darkred'),5)), lwd=0.25, axes=FALSE,xaxt='n')
  
  points(tcdpopmean$tcd_forest_prop, col= rep(c('red','blue'),10), pch=19)
  boxplot(clc_forest_prop~time+ord, data=kernel_data, las=2, boxwex=0.4,medlwd=3,outpch=19,outcol='darkgrey',col=c(rep(c('lightgrey','grey50'),5),rep(c('#fee0d2','brown3'),5)), lwd=0.25, axes=FALSE,xaxt='n')
  clcpopmean <- aggregate(. ~time+ord,data=kernel_data[,c('ord','time','clc_forest_prop')],mean)
  clcpopsd <- aggregate(. ~time+ord,data=kernel_data[,c('ord','time','clc_forest_prop')],sd)
  axis(2, c(0,0.5,1),c(0,0.5,1))
  text(seq(1,length(bb$out),2), par("usr")[3]-0.25, 
       srt = 50, adj= 1, xpd = TRUE,
       labels= gsub('[0-9].*','',unique(paste0(prop$popname,prop$ord))))
  for (j in seq(1,length(bb$out),2)){lines(sort(c(seq(1,20,2)+0.01,seq(2,20,2)-0.01 ))[j:(j+1)], c(0,0),lwd=2,col=rep(c('grey60','brown3'),each=10)[j])}
  boxplot(clc_forest_prop~time+ord, data=kernel_data, las=2, add=TRUE,boxwex=0.4,medlwd=3,col=c(rep(c('lightgrey','grey50'),5),rep(c('#fee0d2','brown3'),5)), lwd=0.25, axes=FALSE,xaxt='n')
  points(clcpopmean$clc_forest_prop,  col= rep(c('red','blue'),10), pch=19)
  
  #par(mar=c(0,0,0,0))
  #par(mar=c(2,2,2,0))
  a <- t(as.matrix(prop[,c('clc1tcd1','clc0tcd1','clc0tcd0','clc1tcd0')]))
  colnames(a) <- paste0(prop$popname, '_',prop$time)
  bb <- barplot(a[1:2,], las=2,space=c(0,rep(c(0,0.5),10))[1:20], border='white',col=c('white'), ylim=c(0,1), axes=FALSE,xaxt='n')
  # axis(1, bb, labels= rep(c('d','n'),10), las=1,cex=0.5, tick=FALSE, col.ticks='white')
  # axis(2, c(0,0.5,1),c(0,0.5,1))
  text(bb[seq(1,length(bb),2)]+0.5, par("usr")[3]+1.20,cex=0.9,
       srt = 60, adj= 1, xpd = TRUE,
       labels= gsub('[0-9].*','',unique(paste0(prop$popname,prop$ord))))
  
  bb <- barplot(a[1:2,], las=2,space=c(0,rep(c(0,0.5),10))[1:20], border='white',col=c('white'), ylim=c(0,1), axes=FALSE,xaxt='n')
  #  axis(1, bb, labels= rep(c('d','n'),10), las=1,cex=0.5, tick=FALSE, col.ticks='white')
  #  axis(2, c(0,0.5,1),c(0,0.5,1))
  text(bb[seq(1,length(bb),2)]+0.5, par("usr")[3]+1.20,cex=0.9,
       srt = 60, adj= 1, xpd = TRUE,
       labels= gsub('[0-9].*','',unique(paste0(prop$popname,prop$ord))))
  
}