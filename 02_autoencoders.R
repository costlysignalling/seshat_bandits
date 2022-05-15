# Autoencoder neural networks creating the culturespace representation
library(h2o)  # library for fitting (not only) autoencoders, check https://bradleyboehmke.github.io/HOML/autoencoders.html for details
h2o.init(max_mem_size = "5g")  # initialize H2O instance
#h2o.shutdown() #sometimes you need to turn the autoencoder on and off (press y)

#For visualizations
library(scatterplot3d)


db<-read.delim("data_indicator_binary_filled.txt",stringsAsFactors=F)
names(db)

#Taking the cultural variables only. Not the utility or fitness variables

d<-db[,9:ncol(db)]
dim(d)


#Format in the way that suits the h2o infrastructure
features <- as.h2o(d)

#In the beginning each polity-in-time (428) has in this way of representing the data 283 dimensions 
#List of 
hyper_grid <- list(hidden = list(
  c(100),
  c(50),
  c(20),
  c(10),
  c(3),
  c(50, 20, 50),
  c(100, 3, 100),
  c(50, 20, 10, 20, 50),
  c(100, 50, 20, 50, 100),
  c(100, 50, 3, 50, 100),
  c(90, 30, 3, 30, 90),
  c(100, 50, 20, 10, 20, 50, 100)
))

#Fitting the autoencoders defined above by the number of hidden layers
ae_grid <- h2o.grid(
  algorithm = 'deeplearning',
  x = seq_along(features),
  training_frame = features,
  grid_id = 'autoencoder_grid',
  autoencoder = TRUE,
  activation = 'Tanh',
  hyper_params = hyper_grid,
  sparse = FALSE,
  ignore_const_cols = FALSE,
  seed = 123
)

# Which model is the best
h2o.getGrid('autoencoder_grid', sort_by = 'mse', decreasing = FALSE)

# Get sampled polity features
take <- sample(1:nrow(d), 4)
sampled <- d[take, ]
sampl.labs<- db$pol[take]
rownames(sampled) <- sampl.labs

# Predict reconstructed pixel values
str(ae_grid)
model_id <- ae_grid@model_ids[[10]] #For the first visualizations I take the best model that worked with 3 dimensions
model <- h2o.getModel(model_id)

reconstructed <- predict(model, as.h2o(sampled))
names(reconstructed) <- colnames(sampled)

combine <- rbind(sampled, as.matrix(reconstructed))
#Add one row of NAs to complete the 71×4 image for visualization
combine <- cbind(combine,rep(NA,nrow(combine)))

par(mfrow = c(1, 3), mar=c(1, 1, 1, 1))
layout(matrix(seq_len(nrow(combine)), 4, 2, byrow = FALSE))
for(i in seq_len(nrow(combine))) {
  image(matrix(unlist(combine[i, ]), 4, 71,byrow=T)[,71:1], xaxt="n", yaxt="n")
  box()
}
dev.off()

#Create CultureSpace
CS <- h2o.deepfeatures(model, features, layer = 2) #Watch oout for the correct layer
CS<-as.matrix(CS)
colnames(CS)<-paste("dim",1:ncol(CS),sep="")
str(CS)

#Col of a point
colp<-"#50505080"

#Simple plot
pl<-scatterplot3d(CS[,1],CS[,2],CS[,3],color=colp,pch=16)

#Culturespace coordinates with utility functions
dp<-cbind(db[,1:8],CS) #for d polities or d places
dt<-dp[dp$place=="Upper Egypt",]
dt<-dt[order(dt$time1),]  #dt as for data target

#Color of the first target
col1<-c("#2080F0E0")

#To pick sizes I use logarithm of the largest settlement size, other things might be ok as well, but they are incomplete, or there is something strateg going on (probably lot of unknowns and not verz well treated missing value completion or range treatment)
sizes<-log(dt$Population.of.the.largest.settlement)/8
pl$points3d(dt$dim1,dt$dim2,dt$dim3,col=col1,pch=16,cex=sizes)
pl$box3d()
lines(pl$xyz.convert(cbind(dt$dim1,dt$dim2,dt$dim3)), col=col1,lwd=2)

pu<-par("usr")
legend(pu[1]*1.2,pu[4]*1.2,col=col1,legend="Upper Egypt",pch=16,bty="n",xpd=NA)

#One possible culturespace, saving it for future use - mostly for visualizations
write.table(dp,"culturespace2_autoencoders.txt",sep="\t",row.names=F)

