library(scatterplot3d)
library(randomcoloR)
library(scales)

CS1<-read.delim("HDD_10cat_BPL.txt")
CS1<-CS1[,5:ncol(CS1)]

CS2<-read.delim("culturespace2_autoencoders.txt")

dp<-cbind(CS2[1:8],CS1)


#Colors were generated using the code in 04_order_timeplaces
mycols<-c("#e8ef23","#472ed3","#ffa642","#318a96","#cb53ef","#870402","#f418c8","#8dbc34","#e2cf61","#2372db","#ff3d3a","#f4f446","#5b5fd3","#e81b6a","#bc07b0","#379fe5","#bfb200","#399102","#472ce0","#3ddd1c","#7e3cad","#5eb6ff","#29aa05","#2ce890","#d13277","#9843e8","#83e52d","#f4d869","#2f02f9","#c4e011","#6e45ad","#e8d106","#641489","#e059db","#a022f4")
cols<-mycols



#Show trajectory function, works for any NGA
showTraj<-function(names,col,cexscale=1/8,size=17,colp="#50505080",lwd=2,alphap=0.8,alphal=0.6,overwrite=NA,leg.ncol=1){
  
  if(is.na(overwrite)){lab.bit<-paste(names,collapse="_")}else{lab.bit<-overwrite}
  
  tiff(paste("trejectory_",lab.bit,".tiff",sep=""),width=size,height=size,units="cm",res=600,compression="lzw")
  pl<-scatterplot3d(dp$MilTech_agg,dp$Info_agg,dp$Norms_agg,color=colp,pch=16,xlab="military tech",ylab="information tech",zlab="norms")
  
  for(n in 1:length(names)){
    name<-names[n]
    dt<-dp[dp$place==name,]
    dt<-dt[order(dt$time1),]  #dt as for data target
    
    sizes<-log(dt$Population.of.the.largest.settlement)*cexscale
    pl$points3d(dt$MilTech_agg,dt$Info_agg,dt$Norms_agg,col=alpha(col[n],alphap),pch=16,cex=sizes)
    lines(pl$xyz.convert(cbind(dt$MilTech_agg,dt$Info_agg,dt$Norms_agg)), col=alpha(col[n],alphal),lwd=2)
  }
  
  pl$box3d()
  pu<-par("usr")
  legend(pu[1]*1.2,pu[4]*1.2,col=col,legend=names,pch=16,bty="n",xpd=NA,ncol=leg.ncol)
  dev.off()
}

origins<-tapply(dp$time1,dp$place,min)
origins<-sort(origins)

places<-names(origins)

#draw all images
for(i in 1:length(places)){
  showTraj(names=places[i],col=cols[i])  
}

#Plot few of these
cherrypick<-c("Crete","Kachi Plain","Kansai","Konya Plain","Middle Yellow River Valley","Southern Mesopotamia","Latium","Susiana","Upper Egypt")
showTraj(names=cherrypick,col=cols[match(cherrypick,places)],overwrite="selected",leg.ncol=3)  

showTraj(names=places,col=cols,overwrite="all")  

