library(scatterplot3d)
library(randomcoloR)
library(scales)
library(animation)
#Do not forget to set a path towards build of ffmpeg animations if you want to export mp4 animations
oopts = if (.Platform$OS.type == "windows") {
  ani.options(ffmpeg = "D:/servis/ffmpeg/bin/ffmpeg.exe")
}
ani.options(autobrowse = FALSE)

dp<-read.delim("culturespace2_autoencoders.txt",header=T)

dp<-dp[order(dp$time1),]
dp<-dp[order(dp$place),]

t.range<-c(min(dp$time1),max(dp$time2))

#Function dhat draws one panelat time t 
showTrajTime<-function(names,col,time,cexscale=1/8,colp="#50505080",lwd=2,alphap=0.8,alphal=0.6,overwrite=NA,leg.ncol=1,only.until=F){
  
  pl<-scatterplot3d(dp$dim1,dp$dim2,dp$dim3,color=colp,pch=16,xlab="dim1",ylab="dim2",zlab="dim3",type="n")
  
  if(only.until==T){
    pl$points3d(dp$dim1[dp$time1<=time],dp$dim2[dp$time1<=time],dp$dim3[dp$time1<=time],col=colp,pch=16) 
  }else{
    pl$points3d(dp$dim1,dp$dim2,dp$dim3,col=colp,pch=16)
  }
  
  for(n in 1:length(names)){
    name<-names[n]
    dt<-dp[dp$place==name,]
    dt<-dt[order(dt$time1),]  #dt as for data target
    
    dt2<-dt[dt$time1<=time,]
    
    sizes<-log(dt2$Population.of.the.largest.settlement)*cexscale
    pl$points3d(dt2$dim1,dt2$dim2,dt2$dim3,col=alpha(col[n],alphap),pch=16,cex=sizes)
    lines(pl$xyz.convert(cbind(dt2$dim1,dt2$dim2,dt2$dim3)), col=alpha(col[n],alphal),lwd=2)
    
    if(time<max(dt$time2)){
      extra<-dt[which(dt$time1==max(dt2$time1))+c(0,1),]
      gofrom<-c(extra$dim1[1],extra$dim2[1],extra$dim3[1])
      goto<-c(extra$dim1[2],extra$dim2[2],extra$dim3[2])
      vector<-(goto-gofrom)*((time-extra$time1[1])/(extra$time1[2]-extra$time1[1]))
      endpoint<-gofrom+vector
      extra[2,9:11]<-endpoint
      lines(pl$xyz.convert(cbind(extra$dim1,extra$dim2,extra$dim3)), col=alpha(col[n],alphal),lwd=2)
    }
  }  
  
  pl$box3d()
  pu<-par("usr")
  legend(pu[1]*1.1,pu[4]*1.18,col=col,legend=names,pch=16,bty="n",xpd=NA,ncol=leg.ncol)
  text(pu[1]*1.0,pu[3]-c(pu[4]-pu[3])*0.1,paste("year",round(time)),xpd=NA,pos=4,font=2)
}



animate<-function(dp,names,times,col="#FF0000",overwrite=NA,format="pdf",fps=25,res=120,scale=2.5,only.until=F){
  
  if(is.na(overwrite)){lab.bit<-paste(names,collapse="_")}else{lab.bit<-overwrite}
  
  myanim<-function(names,times,col,leg.ncol,only.until, ...){
    for(i in times){
      
      dev.hold()
      showTrajTime(names=names,col=col,time=i,leg.ncol=leg.ncol,only.until=only.until)
      ani.pause()
      
    }
  }
  
  
  if("pdf" %in% format){
    
    folder0<-getwd()
    lab.bit<-gsub(" ",".",lab.bit)
    dir.create(lab.bit)
    setwd(lab.bit)
    
    #Save the animation as a convenient .pdf
    saveLatex({
      myanim(names=names,times=times,col=col,leg.ncol=3,only.until=only.until)
    }, img.name = 'frame', ani.opts = 'controls,width=0.95\\textwidth',
    latex.filename = paste("Change_pdf",lab.bit,'.anim.tex',sep=""), nmax = length(times),
    interval = 1/fps,
    ani.dev = function(...){pdf(...)}, ani.type = 'pdf',ani.width=8,ani.height=8,
    documentclass = paste('\\documentclass{article}','\\thispagestyle{empty}','\\usepackage{hyperref}','\\hypersetup{pdfstartview={XYZ null null 1.5}}',
                          paste('\\usepackage[papersize={',8+1,'cm,',8+1,'cm},margin=0.5cm]{geometry}',sep=""), sep = '\n'))
    
    #Set folder back to the original file, copy the animation here and delete the file with latext log etc.  
    setwd(folder0)
    file.copy(from = paste(folder0,"\\",lab.bit,"\\","Change_pdf",lab.bit,".anim.pdf",sep=""),
              to   = folder0,overwrite=T)
    unlink(paste(folder0,"\\",lab.bit,sep=""),recursive=T)
  }
  
  if("mp4" %in% format){
    
    folder0<-getwd()
    lab.bit<-gsub(" ",".",lab.bit)
    dir.create(lab.bit)
    setwd(lab.bit)
    
    grain<-res/2.5
    
    #Save the animation as .mp4
    saveVideo({
      myanim(names=names,times=times,col=col,leg.ncol=3,only.until=only.until)
    }, img.name = 'frame',
    video.name = paste("Change_Mp4",lab.bit,'.anim.mp4',sep=""), nmax = length(times),
    interval = 1/fps,
    ani.dev = function(...){png(...,res=res)}, ani.type = 'png',ani.width=round(scale*8.2*grain),ani.height=round(scale*8*grain),other.opts = paste("-s ",round(scale*8.2*grain),"x",round(scale*8*grain)," -pix_fmt yuv420p -b 1000k",sep=""))
    
    #Set folder back to the original file, copy the animation here and delete the file steps etc.  
    setwd(folder0)
    file.copy(from = paste(folder0,"\\",lab.bit,"\\","Change_Mp4",lab.bit,".anim.mp4",sep=""),
              to   = folder0, overwrite=T)
    unlink(paste(folder0,"\\",lab.bit,sep=""),recursive=T)
  }
}

origins<-tapply(dp$time1,dp$place,min)
origins<-sort(origins)

places<-names(origins)

#Colors were generated using the code in 04_order_timeplaces
mycols<-c("#e8ef23","#472ed3","#ffa642","#318a96","#cb53ef","#870402","#f418c8","#8dbc34","#e2cf61","#2372db","#ff3d3a","#f4f446","#5b5fd3","#e81b6a","#bc07b0","#379fe5","#bfb200","#399102","#472ce0","#3ddd1c","#7e3cad","#5eb6ff","#29aa05","#2ce890","#d13277","#9843e8","#83e52d","#f4d869","#2f02f9","#c4e011","#6e45ad","#e8d106","#641489","#e059db","#a022f4")
cols<-mycols

cherrypick<-c("Crete","Kachi Plain","Kansai","Konya Plain","Middle Yellow River Valley","Southern Mesopotamia","Latium","Susiana","Upper Egypt")

animate(dp=dp,names=cherrypick,times=c(seq(min(dp$time1),max(dp$time2),by=20),max(dp$time2)),col=cols[match(cherrypick,places)],format=c("pdf","mp4"),fps=25,scale=2.5,only.until=T,res=200,overwrite="selected")

animate(dp=dp,names=cherrypick,times=c(seq(-9000,max(dp$time2),by=20),max(dp$time2)),col=cols[match(cherrypick,places)],format=c("pdf","mp4"),fps=25,scale=2.5,only.until=T,res=200,overwrite="selected2")


for(i in 1:length(places)){
  place<-places[i]
  animate(dp=dp,names=place,times=c(seq(min(dp$time1[dp$place==place]),max(dp$time2[dp$place==place]),by=20),max(dp$time2[dp$place==place])),col=cols[i],format=c("pdf","mp4"),fps=25,scale=2.5,only.until=T,res=200)
}

