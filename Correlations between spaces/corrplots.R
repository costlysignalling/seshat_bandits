library(corrplot)

CS1<-read.delim("HDD_10cat_BPL.txt")
CS2<-read.delim("culturespace2_autoencoders.txt")
CS3<-read.delim("culturespace4_FA.txt")
CS4<-read.delim("MCA_Coordinatesv2.txt")
CS5<-read.csv("PCAscores.csv")


names(CS4)
CS1<-CS1[,5:ncol(CS1)]
CS2<-CS2[,9:ncol(CS2)]
CS3<-CS3[,9:ncol(CS3)]
CS4<-CS4[,2:ncol(CS4)]
CS5<-CS5[,2:ncol(CS5)]


names(CS2)<-paste("AE",names(CS2),sep="")
names(CS5)<-paste("PC",1:ncol(CS5),sep="")

names(CS3)<-c("military_attack","infrastructure","literature","military_defend","manipulative morals","transcendental morals","advanced military","Equality as ideology","primitive military","instant karma","chinese bureaucracy","record keeping")

#How many dimensions different spaces have
(dims<-sapply(list(CS1,CS2,CS3,CS4,CS5),ncol))


w<-15
png("corrplot_CS1_CS2.png",width=w,height=w,units="cm",res=600)
cortab<-cor(CS1,CS2)
corrplot(cortab,method="ellipse")
dev.off()

png("corrplot_CS1_CS3.png",width=w*1.2,height=w*1.2,units="cm",res=600)
cortab<-cor(CS1,CS3)
corrplot(cortab,method="ellipse")
dev.off()

png("corrplot_CS1_CS4.png",width=w,height=w,units="cm",res=600)
cortab<-cor(CS1,CS4)
corrplot(cortab,method="ellipse")
dev.off()

png("corrplot_CS1_CS5.png",width=w,height=w,units="cm",res=600)
cortab<-cor(CS1,CS5)
corrplot(cortab,method="ellipse")
dev.off()


png("corrplot_CS2_CS3.png",width=w*1.2,height=w*1.2,units="cm",res=600)
cortab<-cor(CS2,CS3)
corrplot(cortab,method="ellipse")
dev.off()

png("corrplot_CS2_CS4.png",width=w*0.5,height=w*0.5,units="cm",res=600)
cortab<-cor(CS2,CS4)
corrplot(cortab,method="ellipse")
dev.off()

png("corrplot_CS2_CS5.png",width=w*0.7,height=w*0.7,units="cm",res=600)
cortab<-cor(CS2,CS5)
corrplot(cortab,method="ellipse")
dev.off()



png("corrplot_CS3_CS4.png",width=w*1.2,height=w*1.2,units="cm",res=600)
cortab<-cor(CS3,CS4)
corrplot(cortab,method="ellipse")
dev.off()

png("corrplot_CS3_CS5.png",width=w*1.2,height=w*1.2,units="cm",res=600)
cortab<-cor(CS3,CS5)
corrplot(cortab,method="ellipse")
dev.off()


png("corrplot_CS4_CS5.png",width=w*0.5,height=w*0.5,units="cm",res=600)
cortab<-cor(CS4,CS5)
corrplot(cortab,method="ellipse")
dev.off()