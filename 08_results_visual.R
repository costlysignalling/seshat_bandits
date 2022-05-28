load("m.Null.Rdata")
load("m.Push.Rdata")
load("m.GP.Rdata")
load("m.GPcor.Rdata")
load("m.VDI.Rdata")

precis(m.Null)
precis(m.Push)
precis(m.GP)
precis(m.GPcor)
precis(m.VDI,prob=0.9)

post.Null<-extract.samples(m.Null)
post.Push<-extract.samples(m.Push)
post.GP<-extract.samples(m.GP)
post.GPcor<-extract.samples(m.GPcor)
post.VDI<-extract.samples(m.VDI)

give<-function(x){
  if(length(x)==0){
    return(as.numeric(NA))
  }else{
    return(x)
  }
}


posts<-lapply(list(post.Null,post.Push,post.GP,post.GPcor,post.VDI),function(p){
  data.frame(eta=give(p$eta),
        nu=give(p$nu),
        rhosq=give(p$rhosq),
        deltas=give(p$deltas))})

PI(posts[[5]]$deltas)

precis()

str(posts)

ofs<-seq(-0.1,0.1,l=5)
cols<-rainbow(5)
pchs<-c(25:21)

tiff("parameter_visual.tif",width=12,height=10,units="cm",res=600,compression="lzw")
par(mar=c(3.2,10,2,1),mgp=c(2,0.6,0))
plot(NULL,xlim=c(0,1.5),ylim=c(4.5,0.5),yaxt="n",xlab="Parameter Estimate",ylab="")
rect(-1000,-1000,1000,1000,col="#E0E0DE")
abline(h=1:4,col="white")
abline(v=seq(0,1.5,by=0.5),col="white",lty=1)
abline(v=seq(0.25,2,by=0.5),col="white",lty=2)
axis(2,at=1:4,labels=c("constant variance (eta)","neighbour proportional\nvariance (nu)","influence decline\nwith distance (rho)","Preceding polity\nbonus (delta/10)"),las=2)
for(i in 1:4){
  for(j in 1:length(posts)){
    thisplot<-posts[[j]][,i]
    if(!is.na(thisplot[1])){
      CI<-PI(thisplot,prob=0.90)
      lines(CI,rep(i+ofs[j],2),col=cols[j])
      points(mean(thisplot,na.rm=T),i+ofs[j],col=cols[j],pch=pchs[j],bg="white") 
    }
  }
}
box()
legend("topright",col=cols,lwd=1,pch=pchs,pt.bg="white",legend=c("Null","Convergence","GP","GPcor","VDI"),ncol=1,bty="o",title="model",cex=0.75)
dev.off()

#Empirical distances
dists<-read.csv("SeshatData_DistanceMatrix.csv")
nam<-dists$X
dists<-dists[,2:36]
rownames(dists)<-nam
colnames(dists)<-nam
dists<-dists/1000 #In thousands km

#decline of weight with the distance
D<-seq(0,20,by=0.1)

deltas<-posts[[5]]$deltas
rhosq<-posts[[5]]$rhosq

w<-sapply(1:length(rhosq),function(i){exp(-rhosq[i]*D^2)})
str(w)
muw<-apply(w,1,mean)
CIw<-apply(w,1,HPDI,prob=0.9)

mud<-mean(deltas+1)
CId<-HPDI(deltas+1,prob=0.9)

cold<-"#008080"
cold2<-"#379fe5"


tiff("distance_visual.tif",width=12,height=12,units="cm",res=600,compression="lzw")
par(mar=c(3.2,3.2,2,1),mgp=c(2.1,0.7,0))
plot(NULL,xlim=c(0,20),ylim=c(0,8),ylab="Weight of the polity in the distribution parameters",xlab="Distance between polities (thousands km)")
shade(CIw,D,col=alpha(cold,0.3))
lines(D,muw,lwd=1,col=cold)
abline(h=0,col=alpha("black",0.5),lty=2)
abline(v=0,col=alpha("black",0.5),lty=2)

lines(rep(0,2),CId,col=alpha(cold,0.8),lwd=2)
points(0,mud,col=cold,pch=16)

show<-dists[5,c(1,8,20)]
labs<-paste("|",paste(rownames(show),colnames(show),sep=" -\n"),"|",sep="")
abline(v=show,col=cold2,lty=2)

pos.labs<-c(2.7,4.2,1.2)

rect(0.4,5.3,9.5,6.5,col=alpha("white",0.8),border=NA)
rect(show[1]+0.3,pos.labs[1]-0.4,show[1]+7,pos.labs[1]+0.5,col=alpha("white",0.8),border=NA)
rect(show[2]+0.3,pos.labs[2]-0.4,show[2]+10,pos.labs[2]+0.5,col=alpha("white",0.8),border=NA)
rect(show[3]+0.3,pos.labs[3]-0.4,show[3]+11,pos.labs[3]+0.5,col=alpha("white",0.8),border=NA)

text(0,mud,"1 + Extra bonus for the\ndirectly preceding polity\nwithin the same NGA",col=cold,pos=4,cex=0.9)
text(show,pos.labs,labs,pos=4,col=cold2,cex=0.9)
dev.off()

