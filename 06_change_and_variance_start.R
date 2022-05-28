library(rethinking)

dp<-read.delim("culturespace2_autoencoders.txt",header=T)

#Order by time and place
dp<-dp[order(dp$time1),]
dp<-dp[order(dp$place),]

#Get the length between time2 and time1
dp$duration<-dp$time2-dp$time1

#Get order of polity within NGA
places<-unique(dp$place)
dp$order<-unlist(lapply(places,function(p){1:sum(dp$place==p)}))

#Get a ding polities
precede<-lapply(1:nrow(dp),function(i){dp[dp$order==(dp$order[i]-1) & dp$place==dp$place[i],]})
precede[lapply(precede, nrow)==0]<-as.data.frame(rep(NA,ncol(precede[[1]])))
precede<-do.call(rbind,precede)

#Get a dataframe of coexisting polities for each polity
coex<-lapply(1:nrow(dp),function(i){dp[dp$time1<dp$time1[i] & dp$time2>=dp$time1[i] & dp$place!=dp$place[i],]})

#HERE IS AN EXAMPLE OF WHAT WE MEAN BY WEIGHTED MEAN AND VARIANCE
v<-c(1,8,3,4)
w1<-c(1,1,1,1) #Equal weights
w2<-c(6,4,4,2) #Unequal weights 1
w3<-c(1,1,5,5) #Unequal weights 2
#w1<-w1/sum(w1) #not necessary
#w2<-w2/sum(w2)
#w3<-w3/sum(w3)

wmean<-function(v,w){sum(v*w)/sum(w)}
mean(v)
wmean(v,w1)
wmean(v,w2)
wmean(v,w3)

wvar<-function(v,w){sum(w*(v-wmean(v,w))^2)/sum(w)}
var2(v)
wvar(v,w1)
wvar(v,w2)
wvar(v,w3)

wsd<-function(v,w){sqrt(wvar(v,w))}
sd2(v)
wsd(v,w1)
wsd(v,w2)
wsd(v,w3)

#Covariance that does not divide by n-1
cov2<-function(v1,v2,w){sum((v1-mean(v1))*(v2-mean(v2)))/length(v1)}
wcov<-function(v1,v2,w){sum(w*(v1-wmean(v1,w))*(v2-wmean(v2,w)))/sum(w)}
cov(v,v)
wcov(v,v,w1)
wcov(v,v,w2)
wcov(v,v,w3)
cov(w2,w3)
wcov(w2,w3,w1)

#Variance-covariance matrix (not weighted) of the set of the coexisting polities construction
#Extract the culturespace dimensions only
CS<-lapply(coex,function(x){x[,substr(names(x),1,3)=="dim"]})

#Self and preceding
CSself<-dp[,substr(names(dp),1,3)=="dim"]
CSprec<-precede[,substr(names(precede),1,3)=="dim"]

#Add the previous self polity
CSs<-lapply(1:length(CS),function(i){rbind(CSprec[i,],CS[[i]])})

#This will be replaced with a better version - vector of push (take self position and add to in a d-dimensional vector in the direction in which polities tend to move through culturespace regartes of their coexistence and distance)
CSpush<-CSself+matrix(rep(c(0.11,0.12,0.13),times=nrow(CSself)),ncol=ncol(CSself),byrow=T)

#Assign distances for weight calculation
dists<-read.csv("SeshatData_DistanceMatrix.csv")
nam<-dists$X
dists<-dists[,2:36]
rownames(dists)<-nam
colnames(dists)<-nam

#we wnat the dists in thousands of km (not km)
dists<-dists/1000

list.d<-lapply(1:nrow(dp),function(i){c(0,unlist(dists[which(dp$place[i]==nam),match(coex[[i]]$place,nam)]))})

#Check NAs in the influence sets and remove rows and weights corresponding to incomplete observations (mostly precending polities of polities that have order 1 within given NGA)
complete<-lapply(CSs,function(x){rowSums(is.na(x))==0})
CSs<-lapply(1:length(CSs),function(i){CSs[[i]][complete[[i]],]})
list.d<-lapply(1:length(CSs),function(i){list.d[[i]][complete[[i]]]})


#Weighted covariance function for matrix
wcov.m<-function(m,w){
  sapply(1:ncol(m),function(i){sapply(1:ncol(m),function(j){wcov(m[,i],m[,j],w)})})
}

#Get weighted variance/covarinace matrix
# wvarcov<-lapply(1:length(CSs),function(i){wcov.m(CSs[[i]],list.w[[i]])})
# wvarcov[1:3]

#Weighted means
# wmeans<-lapply(1:length(CSs),function(i){apply(CSs[[i]],2,wmean,list.w[[i]])})
# wmeans[1:3]
# 
# #How many coexisting polities are there - important for later exclusion of the oldest polity
# nmodels<-sapply(CSs,nrow)
# 
# #Total variance (as weighted mean distance from the weighted mean)
# totalvar<-sapply(1:length(CSs),function(i){ifelse(nmodels[i]==0,0,sqrt(wmean(rowSums((as.matrix(CSs[[i]])-matrix(rep(wmeans[[i]],each=nrow(CSs[[i]])),nrow=nrow(CSs[[i]])))^2),list.w[[i]])))})
# 
# 
# #size of the change
# change.size<-sqrt(rowSums((as.matrix(CSself)-as.matrix(CSprec))^2))
# 
# #Change per year (measured from the beginning to the end of duration of the previous polity)
# prec.t<-sapply(1:nrow(dp),function(i){ifelse(dp$order[i]==1,NA,dp$duration[dp$order==(dp$order[i]-1) & dp$place==dp$place[i]])})
# change.per.t<-change.size/prec.t
# 
# #Very rough bit - does more variance mean more change?
# plot(totalvar,change.size)
# 
# plot(totalvar,change.per.t)
# plot(log(totalvar),log(change.per.t))
# ln.change<-log(change.per.t)
# ln.totvar<-log(totalvar)
# d<-data.frame(ln.totvar,ln.change,time2=dp$time2)
# d<-d[complete.cases(d),]
# nrow(d)
# d<-d[is.finite(d$ln.change),]
# m<-lm(d$ln.change~d$ln.totvar)
# m2<-lm(d$ln.change~d$ln.totvar+d$time2)
# 
# summary(m)
# summary(m2)
# #It seems that the relationship between totoal variance and the magniture of change is due to the time
# 
# abline(m)

# We need a better model

#Rotate array - useful function that we will need later
rotArr<-function(a){
  dims<-dim(a)
  a2<-array(NA,dim=c(dims[3],dims[1],dims[2]))
  for(i in 1:dims[1]){
    for(j in 1:dims[2]){
      for(k in 1:dims[3]){
        a2[k,i,j]<-a[i,j,k]
      }
    }
  }
  return(a2)
}

#Make array - similar - nicecly rotataed array from list
makeArr<-function(l){
  a<-array(unlist(l),dim=c(dim(l[[1]]),length(l)))
  return(rotArr(a))
}






