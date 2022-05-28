library(ellipse)
library(rethinking)

#Nice variance/covariance functions
var2<-function(x){sum((x-mean(x))^2)/length(x)}
cov2<-function(x,y){sum((x-mean(x))*(y-mean(y)))/length(x)}

vcov<-function(x,y){return(matrix(c(var2(x),rep(cov2(x,y),2),var2(y)),ncol=2))}

#Colour of parental points and colour of self
colp<-"#606080"
cols<-"#D000D0"

#Color of the gid
grcol1<-"#202020"
grcol2<-"#E0E0E0"

#First example
d1<-data.frame(point=c("o","p1","p2","p3"),x=c(0,-1,0.2,1.2),y=c(0,-0.5,1.8,1.2),col=as.character(c(cols,rep(colp,3))))
#Second example - same correlation, larger variance
d2<-d1
d2$x<-d2$x*1.5
d2$y<-d2$y*1.5
cor(d1$x,d1$y)
cor(d2$x,d2$y)

#Convergence vector end
vx<-0.8
vy<-0.3


png("model_illustration.png",width=15,height=19,units="cm",res=600)

par(oma=c(0.5,0.5,0.5,0.5))
layout(rbind(matrix(1:6,ncol=3),matrix(c(7:11,11),ncol=3)))

#Null model
par(mar=c(0,0.5,0.5,0.5))
plot(d1$x,d1$y,xlim=c(-2,2.5),ylim=c(-1.5,3),pch=16,col=as.character(d1$col),axes=F,bty="n")
points(mean(d1$x),mean(d1$y),pch=3,col=alpha(colp,0.5))
with(d1, for(l in c(0.5,0.9)){lines(ellipse(vcov(x,y),centre=c(mean(x),mean(y)),level=l),col=alpha(colp,0.5))})
points(d1$x[1],d1$y[1],pch=3,col=alpha(cols,0.5))
with(d1, for(l in c(0.5,0.9)){lines(ellipse(matrix(c(1,0,0,1),ncol=2)*0.75^2,centre=c(x[1],y[1]),level=l),col=alpha(cols,0.5))})
box(col=grcol1,lwd=1.5,xpd=NA)
pu<-par("usr")
segments(pu[1],pu[3],pu[2],pu[3],lwd=3,col=grcol2,xpd=NA)
text(pu[1],pu[3]+(pu[4]-pu[3])*0.93,"Null model",font=2,pos=4)
text(pu[1],pu[3]+(pu[4]-pu[3])*0.85,bquote( eta == 0.75 ),font=2,pos=4)

par(mar=c(0.5,0.5,0,0.5))
plot(d2$x,d2$y,xlim=c(-2,2.5),ylim=c(-1.5,3),pch=16,col=as.character(d2$col),axes=F,bty="n")
points(mean(d2$x),mean(d2$y),pch=3,col=alpha(colp,0.5))
with(d2, for(l in c(0.5,0.9)){lines(ellipse(vcov(x,y),centre=c(mean(x),mean(y)),level=l),col=alpha(colp,0.5))})
points(d2$x[1],d2$y[1],pch=3,col=alpha(cols,0.5))
with(d2, for(l in c(0.5,0.9)){lines(ellipse(matrix(c(1,0,0,1),ncol=2)*0.75^2,centre=c(x[1],y[1]),level=l),col=alpha(cols,0.5))})
box(col=grcol1,lwd=1.5,xpd=NA)
pu<-par("usr")
segments(pu[1],pu[4],pu[2],pu[4],lwd=3,col=grcol2,xpd=NA)
segments(pu[1],pu[4]-0.5,pu[1],pu[4]+0.5,lwd=1.5,col=grcol1,xpd=NA)
segments(pu[2],pu[4]-0.5,pu[2],pu[4]+0.5,lwd=1.5,col=grcol1,xpd=NA)


#Convergence model
par(mar=c(0,0.5,0.5,0.5))
plot(d1$x,d1$y,xlim=c(-2,2.5),ylim=c(-1.5,3),pch=16,col=as.character(d1$col),axes=F,bty="n")
points(mean(d1$x),mean(d1$y),pch=3,col=alpha(colp,0.5))
with(d1, for(l in c(0.5,0.9)){lines(ellipse(vcov(x,y),centre=c(mean(x),mean(y)),level=l),col=alpha(colp,0.5))})
arrows(d1$x[1],d1$y[1],d1$x[1]+vx,d1$y[1]+vy,col=cols,length=0.05)
points(d1$x[1]+vx,d1$y[1]+vy,pch=3,col=alpha(cols,0.5))
with(d1, for(l in c(0.5,0.9)){lines(ellipse(matrix(c(1,0,0,1),ncol=2)*0.75^2,centre=c(x[1]+vx,y[1]+vy),level=l),col=alpha(cols,0.5))})
box(col=grcol1,lwd=1.5,xpd=NA)
pu<-par("usr")
segments(pu[1],pu[3],pu[2],pu[3],lwd=3,col=grcol2,xpd=NA)
text(pu[1],pu[3]+(pu[4]-pu[3])*0.93,"Convergence model",font=2,pos=4)
text(pu[1],pu[3]+(pu[4]-pu[3])*0.85,bquote( eta == 0.75 ),font=2,pos=4)

par(mar=c(0.5,0.5,0,0.5))
plot(d2$x,d2$y,xlim=c(-2,2.5),ylim=c(-1.5,3),pch=16,col=as.character(d2$col),axes=F,bty="n")
points(mean(d2$x),mean(d2$y),pch=3,col=alpha(colp,0.5))
with(d2, for(l in c(0.5,0.9)){lines(ellipse(vcov(x,y),centre=c(mean(x),mean(y)),level=l),col=alpha(colp,0.5))})
arrows(d2$x[1],d2$y[1],d2$x[1]+vx,d2$y[1]+vy,col=cols,length=0.05)
points(d2$x[1]+vx,d2$y[1]+vy,pch=3,col=alpha(cols,0.5))
with(d2, for(l in c(0.5,0.9)){lines(ellipse(matrix(c(1,0,0,1),ncol=2)*0.75^2,centre=c(x[1]+vx,y[1]+vy),level=l),col=alpha(cols,0.5))})
box(col=grcol1,lwd=1.5,xpd=NA)
pu<-par("usr")
segments(pu[1],pu[4],pu[2],pu[4],lwd=3,col=grcol2,xpd=NA)
segments(pu[1],pu[4]-0.5,pu[1],pu[4]+0.5,lwd=1.5,col=grcol1,xpd=NA)
segments(pu[2],pu[4]-0.5,pu[2],pu[4]+0.5,lwd=1.5,col=grcol1,xpd=NA)


#Galton-Pearson model
par(mar=c(0,0.5,0.5,0.5))
plot(d1$x,d1$y,xlim=c(-2,2.5),ylim=c(-1.5,3),pch=16,col=as.character(d1$col),axes=F,bty="n")
points(mean(d1$x),mean(d1$y),pch=3,col=alpha(colp,0.5))
with(d1, for(l in c(0.5,0.9)){lines(ellipse(vcov(x,y),centre=c(mean(x),mean(y)),level=l),col=alpha(colp,0.5))})
points(mean(d1$x),mean(d1$y),pch=3,col=alpha(cols,0.5))
with(d1, for(l in c(0.5,0.9)){lines(ellipse(matrix(c(1,0,0,1),ncol=2)*0.75^2,centre=c(mean(x),mean(y)),level=l),col=alpha(cols,0.5))})
box(col=grcol1,lwd=1.5,xpd=NA)
pu<-par("usr")
segments(pu[1],pu[3],pu[2],pu[3],lwd=3,col=grcol2,xpd=NA)
text(pu[1],pu[3]+(pu[4]-pu[3])*0.93,"Galton-Pearson model",font=2,pos=4)
text(pu[1],pu[3]+(pu[4]-pu[3])*0.85,bquote( eta == 0.75 ),font=2,pos=4)

par(mar=c(0.5,0.5,0,0.5))
plot(d2$x,d2$y,xlim=c(-2,2.5),ylim=c(-1.5,3),pch=16,col=as.character(d2$col),axes=F,bty="n")
points(mean(d2$x),mean(d2$y),pch=3,col=alpha(colp,0.5))
with(d2, for(l in c(0.5,0.9)){lines(ellipse(vcov(x,y),centre=c(mean(x),mean(y)),level=l),col=alpha(colp,0.5))})
points(mean(d2$x),mean(d2$y),pch=3,col=alpha(cols,0.5))
with(d2, for(l in c(0.5,0.9)){lines(ellipse(matrix(c(1,0,0,1),ncol=2)*0.75^2,centre=c(mean(x),mean(y)),level=l),col=alpha(cols,0.5))})
box(col=grcol1,lwd=1.5,xpd=NA)
pu<-par("usr")
segments(pu[1],pu[4],pu[2],pu[4],lwd=3,col=grcol2,xpd=NA)
segments(pu[1],pu[4]-0.5,pu[1],pu[4]+0.5,lwd=1.5,col=grcol1,xpd=NA)
segments(pu[2],pu[4]-0.5,pu[2],pu[4]+0.5,lwd=1.5,col=grcol1,xpd=NA)


#Multivariate constant iheriatnce with preserved covariance pattern preserved
par(mar=c(0,0.5,0.5,0.5))
plot(d1$x,d1$y,xlim=c(-2,2.5),ylim=c(-1.5,3),pch=16,col=as.character(d1$col),axes=F,bty="n")
points(mean(d1$x),mean(d1$y),pch=3,col=alpha(colp,0.5))
with(d1, for(l in c(0.5,0.9)){lines(ellipse(vcov(x,y),centre=c(mean(x),mean(y)),level=l),col=alpha(colp,0.5))})
points(mean(d1$x),mean(d1$y),pch=3,col=alpha(cols,0.5))
with(d1, for(l in c(0.5,0.9)){lines(ellipse(((vcov(x,y)/(sd2(x)*sd2(y)))*0.8^2)+matrix(c(1,0,0,1),ncol=2)*0.1^2,centre=c(mean(x),mean(y)),level=l),col=alpha(cols,0.5))})
box(col=grcol1,lwd=1.5,xpd=NA)
pu<-par("usr")
segments(pu[1],pu[3],pu[2],pu[3],lwd=3,col=grcol2,xpd=NA)
text(pu[1],pu[3]+(pu[4]-pu[3])*0.93,"Covariance pattern preserved",font=2,pos=4)
text(pu[1],pu[3]+(pu[4]-pu[3])*0.85,bquote( eta == 0.1 ),font=2,pos=4)
text(pu[1],pu[3]+(pu[4]-pu[3])*0.77,bquote( nu == 0.8 ),font=2,pos=4)

par(mar=c(0.5,0.5,0,0.5))
plot(d2$x,d2$y,xlim=c(-2,2.5),ylim=c(-1.5,3),pch=16,col=as.character(d2$col),axes=F,bty="n")
points(mean(d2$x),mean(d2$y),pch=3,col=alpha(colp,0.5))
with(d2, for(l in c(0.5,0.9)){lines(ellipse(vcov(x,y),centre=c(mean(x),mean(y)),level=l),col=alpha(colp,0.5))})
points(mean(d2$x),mean(d2$y),pch=3,col=alpha(cols,0.5))
with(d2, for(l in c(0.5,0.9)){lines(ellipse(((vcov(x,y)/(sd2(x)*sd2(y)))*0.8^2)+matrix(c(1,0,0,1),ncol=2)*0.1^2,centre=c(mean(x),mean(y)),level=l),col=alpha(cols,0.5))})
box(col=grcol1,lwd=1.5,xpd=NA)
pu<-par("usr")
segments(pu[1],pu[4],pu[2],pu[4],lwd=3,col=grcol2,xpd=NA)
segments(pu[1],pu[4]-0.5,pu[1],pu[4]+0.5,lwd=1.5,col=grcol1,xpd=NA)
segments(pu[2],pu[4]-0.5,pu[2],pu[4]+0.5,lwd=1.5,col=grcol1,xpd=NA)

#Variance Dependent Inheritance model
par(mar=c(0,0.5,0.5,0.5))
plot(d1$x,d1$y,xlim=c(-2,2.5),ylim=c(-1.5,3),pch=16,col=as.character(d1$col),axes=F,bty="n")
points(mean(d1$x),mean(d1$y),pch=3,col=alpha(colp,0.5))
with(d1, for(l in c(0.5,0.9)){lines(ellipse(vcov(x,y),centre=c(mean(x),mean(y)),level=l),col=alpha(colp,0.5))})
points(mean(d1$x),mean(d1$y),pch=3,col=alpha(cols,0.5))
with(d1, for(l in c(0.5,0.9)){lines(ellipse((vcov(x,y)*0.8^2)+matrix(c(1,0,0,1),ncol=2)*0.1^2,centre=c(mean(x),mean(y)),level=l),col=alpha(cols,0.5))})
box(col=grcol1,lwd=1.5,xpd=NA)
pu<-par("usr")
segments(pu[1],pu[3],pu[2],pu[3],lwd=3,col=grcol2,xpd=NA)
text(pu[1],pu[3]+(pu[4]-pu[3])*0.93,"Variance Dependent Inheritance",font=2,pos=4)
text(pu[1],pu[3]+(pu[4]-pu[3])*0.85,bquote( eta == 0.1 ),font=2,pos=4)
text(pu[1],pu[3]+(pu[4]-pu[3])*0.77,bquote( nu == 0.8 ),font=2,pos=4)

par(mar=c(0.5,0.5,0,0.5))
plot(d2$x,d2$y,xlim=c(-2,2.5),ylim=c(-1.5,3),pch=16,col=as.character(d2$col),axes=F,bty="n")
points(mean(d2$x),mean(d2$y),pch=3,col=alpha(colp,0.5))
with(d2, for(l in c(0.5,0.9)){lines(ellipse(vcov(x,y),centre=c(mean(x),mean(y)),level=l),col=alpha(colp,0.5))})
points(mean(d2$x),mean(d2$y),pch=3,col=alpha(cols,0.5))
with(d2, for(l in c(0.5,0.9)){lines(ellipse((vcov(x,y)*0.8^2)+matrix(c(1,0,0,1),ncol=2)*0.1^2,centre=c(mean(x),mean(y)),level=l),col=alpha(cols,0.5))})
box(col=grcol1,lwd=1.5,xpd=NA)
pu<-par("usr")
segments(pu[1],pu[4],pu[2],pu[4],lwd=3,col=grcol2,xpd=NA)
segments(pu[1],pu[4]-0.5,pu[1],pu[4]+0.5,lwd=1.5,col=grcol1,xpd=NA)
segments(pu[2],pu[4]-0.5,pu[2],pu[4]+0.5,lwd=1.5,col=grcol1,xpd=NA)


plot(NULL,xlim=c(0,4.5),ylim=c(0,9),axes=F,bty="n",xaxs="i",yaxs="i")
start<-8.5
ofs<-c(-0.8,-0.8,-1,-1,-1.5,-1.2)
pos<-start;counter<-1
text(0,pos,"Legend",font=2,pos=4)
pos<-pos+ofs[counter];counter<-counter+1
points(0.2,pos,pch=16,col=colp)
text(0.5,pos,"Neigbouring polities'\nculturespace positions",pos=4,xpd=NA)
pos<-pos+ofs[counter];counter<-counter+1
points(0.2,pos,pch=16,col=cols)
text(0.5,pos,"NGA's culturespace position\nbefore the change of polity",pos=4,xpd=NA)

pos<-pos+ofs[counter];counter<-counter+1
for(l in c(0.5,0.9)){lines(ellipse(matrix(c(1,0.25,0.25,1),ncol=2)*0.2^2,centre=c(0.3,pos),level=l),col=alpha(colp,0.5),xpd=NA)}
points(0.3,pos,pch=3,col=alpha(colp,0.5),cex=0.7)
text(0.8,pos,"Distribution of influential\npolities (neigbours and target)",pos=4,xpd=NA)

pos<-pos+ofs[counter];counter<-counter+1
for(l in c(0.5,0.9)){lines(ellipse(matrix(c(1,0.25,0.25,1),ncol=2)*0.2^2,centre=c(0.3,pos),level=l),col=alpha(cols,0.5),xpd=NA)}
points(0.3,pos,pch=3,col=alpha(cols,0.5),cex=0.7)
text(0.8,pos,"Distribution of target NGA's\nposition after the change",pos=4,xpd=NA)

pos<-pos+ofs[counter];counter<-counter+1
text(0,pos,"The elispses oultine 50% and 90%\nhighest-density areas\nof multivariate Gaussian distribution,\ncross marks its centre",pos=4)

pos<-pos+ofs[counter];counter<-counter+1
arrows(0,pos,0.4,pos,length=0.05,col=cols)
text(0.5,pos,"Expected change based\non all trajectories",pos=4)
dev.off()



