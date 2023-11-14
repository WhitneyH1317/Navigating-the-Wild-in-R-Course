plot_predictions <- function(ylim=c(0.0,1), scale=15){
  # ylim - in case the predictions change the ylims can be modified
  # scale - the scale is used to play with the distance between errorbars
  
  # define position of errorbars for tcd on the x axis 
  x_tcd <-  sort(c(seq(1,scale,scale/5),
                   seq(1.10,(scale+0.5),
                       scale/5)))
  # define position of errorbars for clc on the x axis 
  x_clc <-  x_tcd+1
  
  # for plotting the x axis little lines 
  minmax <- sort(c(seq(1,scale,scale/5), 
                   seq(1.10,(scale+0.5),
                       scale/5)+1))        
  
  # for the position of the population names  
  mid <- c((minmax[1]+minmax[2])/2,
           (minmax[3]+minmax[4])/2,
           (minmax[5]+minmax[6])/2,
           (minmax[7]+minmax[8])/2,
           (minmax[9]+minmax[10])/2)
  
  par(mfrow=c(2,2), mar=c(1,1,1,1), oma=c(3,5,3,3))
  for(i in seq(1,length(models),2))
  {
    # plot error bars and points 
    errbar(x_tcd,res[[i]]$pred,res[[i]]$lower,res[[i]]$upper,col=c('darkgoldenrod1','black'),errbar.col=best$color[i],
           ylim=ylim,xlim=c(0,scale+1), axes=FALSE, ylab=NA,xlab=NA,lwd=2)
    points(x_tcd, res[[i]]$pred, col=c('darkgoldenrod1','black'),pch=19, cex=1.5)
    axis(1,at=mid,tick=FALSE,padj=-2, col = NA,labels=unique(res[[i]]$popname), col.ticks='white')
    axis(2)
    if(i %in% c(1,5)){mtext(side=2, 'Proportion of forest',padj=-3.2)}
    if(i == 1){mtext(side=2,'red',font=2,las=2,adj=3.5)}
    if(i == 1){mtext(side=2,'deer',font=2,las=2.5,adj=2.7,padj=2)}
    
    if(i == 5){mtext(side=2,'roe',font=2,las=2,adj=3.5)}
    if(i == 5){mtext(side=2,'deer',font=2,las=2,adj=2.7,padj=2)}
    if(i == 1){mtext(side=3,'GPS locations',font=2)}
    if(i == 3){mtext(side=3,'HRs',font=2)}
    for(j in seq(1,length(x_tcd),2)){lines(x=minmax[j:(j+1)],y=c(0.0,0.0))}
    
    errbar(x_clc,res[[i+1]]$pred,res[[i+1]]$lower,res[[i+1]]$upper,col=c('darkgoldenrod1','black'),errbar.col=best$color[i+1],
           ylim=ylim,add=TRUE,pch=17, axes=FALSE, ylab=NA,xlab=NA,lwd=2)
    points(x_clc, res[[i+1]]$pred, col=c('darkgoldenrod1','black'),pch=17, cex=1.5)
    axis(1,at=mid,tick=FALSE,padj=-2, col = NA,labels=unique(res[[i+1]]$popname), col.ticks='white')
    if(i == 5){mtext(side=2,'darkgoldenrod1',font=2,las=2,adj=3.5)}
  }
  
}
