library(randomcoloR)

dp<-read.delim("culturespace2_autoencoders.txt",header=T)

dp<-dp[order(dp$time1),]
dp<-dp[order(dp$place),]

head(dp)

origins<-tapply(dp$time1,dp$place,min)
origins<-sort(origins)

places<-names(origins)

continent<-c("Asia","Middle","Mesopot","Middle","India","Europe","India","America","Asia","Africa","Europe","Asia","Middle","Europe","India","Middle","Asia","America","Africa","America","Oceania","Africa","America","America","Europe","Oceania","America","Asia","Africa","America","Oceania","Asia","Oceania","India","Oceania")

#I assign hue to each continent and then merge near east and africa, becuase "monochrome" did not look nice
table(continent,as.factor(continent))
hues<-sapply(as.factor(continent),function(x){switch(as.numeric(x),"blue","green","yellow","red","pink","orange","blue","purple")})

# Uncomment this if you want to randomly generate new set of colors (shown at the end)
# cols<-sapply(1:length(places),function(i){randomColor(1,luminosity="bright",hue=hues[i])})
# mycols<-paste(cols,collapse=",")

mycols<-c("#e8ef23","#472ed3","#ffa642","#318a96","#cb53ef","#870402","#f418c8","#8dbc34","#e2cf61","#2372db","#ff3d3a","#f4f446","#5b5fd3","#e81b6a","#bc07b0","#379fe5","#bfb200","#399102","#472ce0","#3ddd1c","#7e3cad","#5eb6ff","#29aa05","#2ce890","#d13277","#9843e8","#83e52d","#f4d869","#2f02f9","#c4e011","#6e45ad","#e8d106","#641489","#e059db","#a022f4")
cols<-mycols

sc<-1.2
png("timescale.png",width=19.2*sc,height=10.8*sc,units="cm",res=600)
par(bg="#000000")
par(mar=c(2,3.5,2,2),mgp=c(2.1,0.7,0))
plot(NULL,xlab="place",ylab="year",xlim=c(0,length(places)+1),ylim=c(min(dp$time1)-2500,max(dp$time2)),xaxt="n",col.lab="white")
axis(2,col="white",col.axis="white")
for(i in 1:nrow(dp)){
  row<-dp[i,]
  nth<-which(places==row$place)
  rect(nth-0.5,row$time1,nth+0.5,row$time2,col=cols[nth],border="white")
}
text(1:length(places)+0.55,origins-500,places,pos=2,srt=90,col=cols,cex=1,xpd=NA)
dev.off()


#You need to get it to before present
dp$bp1<-2022-dp$time1
dp$bp2<-2022-dp$time2

origins<-tapply(dp$bp1,dp$place,max)
origins<-sort(origins,decreasing=T)

png("timescale_log.png",width=19.2*sc,height=10.8*sc,units="cm",res=600)
par(bg="#000000")
par(mar=c(2,3.5,2,2),mgp=c(2.1,0.7,0))
plot(NULL,xlab="place",ylab="years before present",xlim=c(0,length(places)),ylim=c(max(dp$bp1)+1+150000,40),log="y",xaxt="n",col.lab="white")
axis(2,col="white",col.axis="white")
for(i in 1:nrow(dp)){
  row<-dp[i,]
  nth<-which(places==row$place)
  rect(nth-0.5,row$bp1+1,nth+0.5,row$bp2+1,col=cols[nth],border="white")
}
text(1:length(places)+0.55,origins*1.15,places,pos=2,srt=90,col=cols,cex=1,xpd=NA)
dev.off()

mycols

