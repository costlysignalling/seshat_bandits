###############Performing Factor Analysis
# considering only multistate variables (14:15) and  binary variable (16:128)

#######library 
library(psych)
library(nFactors)
library(qgraph)
library(corrplot)

options(max.print = 10000)


#Loading the data, the code below should upload the 3rd dataset with the respective name 
#FYI: the name of the file is d in 01_process_data.R file
d<-read.delim("data_numbers_filled.txt",stringsAsFactors=F)  #d was the name 
names(d)



######## creating the subset from the whole dataset
BinaryTable <-d[14:128] # defining a new table containing only the variables of interest, multistate and binary

#Performing some tests
bartlett.test(BinaryTable) ##Bartletts test of sphericity, just to check the significance
KMO(BinaryTable)  ## if it is above 0.7
#It gives the warning message for KMO, I cheked in the dataset and the longwall variable as it has zero variance

#new dataset #Deleting longwall variable as it has zero variance
BinaryTable <- subset(BinaryTable, select = - Long.walls) 

#again doing the test
bartlett.test(BinaryTable) ##Bartletts test of spherecity, just to check the significance
KMO(BinaryTable)  ## if it is above 0.7
# NO warning message and good to go




######Correlation matrix of the dataset
rPearson <- cor(BinaryTable)  # calculate Pearson correlation matrix
rPolychoric <- cor_auto(BinaryTable) # calculate polychoric correlations

corrplot(rPolychoric, method="ellipse")
corrplot.mixed(rPolychoric, lower = "number", upper = "circle")

#to export large correlogram  into an image
pdf("corrplot.pdf",width=16,height=16)
corrplot.mixed(rPolychoric, lower = "number", upper = "circle")
dev.off()

# correlation visualizations using heatmap
palette<-colorRampPalette(c("red","white","green"))(200)
heatmap(rPolychoric, col=palette, symm=T)




######Parallel Analysis to determine the number of Factors needed
#It is a statistical test to tell us how many eigen values are greater than chance
nofactors <- fa.parallel(rPolychoric, n.obs = 428, fm="ml", fa="fa") #maximum likelihood
nofactors
#Parallel analysis suggests that the number of factors =  16

#However,
#based on eigen value  greater than one method
sum(nofactors$fa.values > 1)
#Eighen Value method suggests that the number of factors =  12

#Here onwards 12 factors has been considered to calculate the loadings




###### Factor Analysis
fa12 <- fa(r = rPolychoric, #factor analysis by principal axis
           nfactors = 12,
           rotate = "Promax",   #Oblique rotation
           fm="ml",
           residuals = TRUE)

fa12
options(max.print = 10000)

#Result: 52 percentage of variability explained on changing the number of 12 factors 

######checking the loadings with the cutoff at 0.3
fa12cutoff <- print(fa12$loadings, cutoff = 0.3, sort = TRUE)
fa12cutoff


#######scree plot
plot(fa12$values, type = "b", xlim = c(1,20), 
     main = "Scree Plot", xlab = "Number of Factors",ylab = "Eigenvalues")




##########plotting factors comparisons
#comparison of factor 2 and 3 with factor 1

plot(fa12$loadings[, 1], fa12$loadings[, 2], xlim = c(-1, 1), ylim = c(-1,1),
     xlab = "Factor1", ylab = "Factor2 & Factor3", main = "Factor comparision:with Rotation", col="blue")

abline(h=0, v=0)  # draws bisector line

points(fa12$loadings[, 1], fa12$loadings[, 3], col="red")  #plot point of factor3
legend("bottomright", c("Factor2", "Factor3"), pch=1, col = c("blue", "red")) #provides legend


#if wanted to add the names of variables in respective factor, then run the code below:
text(fa12$loadings[, 1]-0.07, #display names of variables in factor2
     fa12$loadings[, 2],
     colnames(rPolychoric),
     col = "blue")
text(fa12$loadings[, 1]+0.07, #display names of variables in factor3
     fa12$loadings[, 3],
     colnames(rPolychoric),
     col = "red")

#Export factor scores
scores<-factor.scores(BinaryTable,fa12, method = "tenBerge")
culturespace<-scores$scores

output<-cbind(d[,1:8],culturespace) #for d polities or d places
write.table(output,"culturespace4_FA.txt",sep="\t",row.names=F)

#more figures and plot  (if required)
#fa.diagram(fa12, main = "For 12 factor loading")
#fa.plot(fa12)


#####Fit Statistics (only if necessary)
#fa12$rms            #Root mean square of the residuals
#fa12$RMSEA          #Root meansquare of the approximation
#fa12$TLI            #Goodness of fit statistics #Tucker lewis index '
