library(rethinking)
source("06_change_and_variance_start.R")

# Resulting position that we want to model
pos<-CSself[dp$order>1,]

#How many rows do these neibour culturespaces have
neighb<-sapply(CSs,nrow)
nmax<-max(neighb)

CSs.fill<-lapply(CSs,function(x){
                      newr<-matrix(rep(0,(nmax-nrow(x))*ncol(x)),ncol=ncol(x))
                      x<-as.matrix(x)
                      return(rbind(x,newr))})

d.fill<-lapply(list.d,function(x){c(x,rep(99999,nmax-length(x)))})
#Now we also have a filled weight vector that is 2 for the previous polity, 1 for coexisting polity at the time of chnge and 0 for filled row that is there to keep number of rows in CSs.fill constant 

CSs.fill<-makeArr(CSs.fill)
str(CSs.fill)

d.fill<-do.call(rbind,d.fill)
str(d.fill)

#Again, we need to get rid of the initial rows
CSpush<-CSpush[dp$order>1,]
CSs.fill<-CSs.fill[dp$order>1,,]
d.fill<-d.fill[dp$order>1,]
neighb<-neighb[dp$order>1]

#Create datasets (see the respective models below)
d.CSs<-list(
  N=nrow(pos),
  D=ncol(pos),
  nmax=nmax,
  neighb=neighb,
  pos=as.matrix(pos),
  CSpush=as.matrix(CSpush),
  CSs=CSs.fill,
  distanc=d.fill,
  unitm=diag(ncol(pos)))

#Null model where the new position is just draw randomly around the original position
code.Null<-"
data{
  int N;
  int D;
  int nmax;
  int neighb[N];
  matrix[N,D] pos;
  matrix[N,D] CSpush;
  matrix[nmax,D] CSs[N];
  matrix[N,nmax] distanc;
  matrix[D,D] unitm;
}
parameters{
  real<lower=0> eta;
}
model{
  eta ~ exponential( 1 );
    for ( i in 1:N ){
     target += multi_normal_lpdf(pos[i,] | CSs[i][1,], unitm*(eta^2));
    }
}
generated quantities{
  vector[N] log_lik;
  for ( i in 1:N ) {
    log_lik[i] = multi_normal_lpdf(pos[i,] | CSs[i][1,], unitm*(eta^2)); 
  }
}"

#Model of convergence where the new position is draw randomly around the position where the "general flow of trajectories" pushes 
code.Push<-"
data{
  int N;
  int D;
  int nmax;
  int neighb[N];
  matrix[N,D] pos;
  matrix[N,D] CSpush;
  matrix[nmax,D] CSs[N];
  matrix[N,nmax] distanc;
  matrix[D,D] unitm;
}
parameters{
  real<lower=0> eta;
}
model{
  eta ~ exponential( 1 );
    for ( i in 1:N ){
     target += multi_normal_lpdf(pos[i,] | CSpush[i,], unitm*(eta^2));
    }
}
generated quantities{
  vector[N] log_lik;
  for ( i in 1:N ) {
    log_lik[i] = multi_normal_lpdf(pos[i,] | CSs[i][1,], unitm*(eta^2)); 
  }
}"

#Model that works with both constant polity variance (Galton-Pearson)
code.GP<-"
data{
  int N;
  int D;
  int nmax;
  int neighb[N];
  matrix[N,D] pos;
  matrix[N,D] CSpush;
  matrix[nmax,D] CSs[N];
  matrix[N,nmax] distanc;
  matrix[D,D] unitm;
}
parameters{
  real<lower=0> eta;
  real<lower=0> rhosq;
  real<lower=0> deltas;
}
model{
  eta ~ exponential( 1 );
  rhosq ~ exponential( 1 );
  deltas ~ exponential( 1 );
    for ( i in 1:N ){
    vector[neighb[i]] w;
    vector[D] wmeans;
    w[1] = zetasq + deltas;
    for(nb in 2:neighb[i]){
    w[nb] = exp(-rhosq*((distanc[i,nb])^2));
    }
    for(d in 1:D){
    wmeans[d]=dot_product(CSs[i][1:neighb[i],d],w)/sum(w);
    }
     target += multi_normal_lpdf(pos[i,] | wmeans, unitm*(eta^2));
    }
}
generated quantities{
  vector[N] log_lik;
  for ( i in 1:N ) {
    vector[neighb[i]] w;
    vector[D] wmeans;
    w[1] = zetasq + deltas;
    for(nb in 2:neighb[i]){
    w[nb] = exp(-rhosq*((distanc[i,nb])^2));
    }
    for(d in 1:D){
    wmeans[d]=dot_product(CSs[i][1:neighb[i],d],w)/sum(w);
    }
    log_lik[i] = multi_normal_lpdf(pos[i,] | wmeans, unitm*(eta^2)); 
  }
}"

#Model that cosideres only the shape but not magnitude of the multivariate neigbour distribution
code.GPcor<-"
data{
  int N;
  int D;
  int nmax;
  int neighb[N];
  matrix[N,D] pos;
  matrix[N,D] CSpush;
  matrix[nmax,D] CSs[N];
  matrix[N,nmax] distanc;
  matrix[D,D] unitm;
}
parameters{
  real<lower=0> eta;
  real<lower=0> nu;
  real<lower=0> rhosq;
  real<lower=0> deltas;
}
model{
  eta ~ exponential( 1 );
  nu ~ exponential( 1 );
  rhosq ~ exponential( 1 );
  deltas ~ exponential( 1 );
   for ( i in 1:N ){
    vector[neighb[i]] w;
    vector[D] wmeans;
    vector[D] wsds;
    matrix[D,D] varcovar;
    real sdprod;
    real two;
    sdprod = 1;
    two = 2;
    w[1] = zetasq + deltas;
    for(nb in 2:neighb[i]){
    w[nb] = exp(-rhosq*((distanc[i,nb])^2));
    }
    for(d in 1:D){
    vector[neighb[i]] centd;
    wmeans[d]=dot_product(CSs[i][1:neighb[i],d],w)/sum(w);
    centd = to_vector(CSs[i][1:neighb[i],d]-wmeans[d]);
    wsds[d]=sqrt(dot_product(centd .* w,centd)/sum(w));
    sdprod = sdprod * wsds[d];
    }
    for(d in 1:D){
    for(e in 1:D){
    vector[neighb[i]] centd;
    vector[neighb[i]] cente;
    centd = to_vector(CSs[i][1:neighb[i],d]-wmeans[d]);
    cente = to_vector(CSs[i][1:neighb[i],e]-wmeans[e]);
    varcovar[d,e] = dot_product(centd .* w,cente)/(sum(w)*(sdprod^(two/D)));
    }
    }
     target += multi_normal_lpdf(pos[i,] | wmeans, varcovar*(nu^2) + unitm*(eta^2));
    }
}
generated quantities{
    vector[N] log_lik;
    for ( i in 1:N ){
    vector[neighb[i]] w;
    vector[D] wmeans;
    vector[D] wsds;
    matrix[D,D] varcovar;
    real sdprod;
    real two;
    sdprod = 1;
    two = 2;
    w[1] = zetasq + deltas;
    for(nb in 2:neighb[i]){
    w[nb] = exp(-rhosq*((distanc[i,nb])^2));
    }
    for(d in 1:D){
    vector[neighb[i]] centd;
    wmeans[d]=dot_product(CSs[i][1:neighb[i],d],w)/sum(w);
    centd = to_vector(CSs[i][1:neighb[i],d]-wmeans[d]);
    wsds[d]=sqrt(dot_product(centd .* w,centd)/sum(w));
    sdprod = sdprod * wsds[d];
    }
    for(d in 1:D){
    for(e in 1:D){
    vector[neighb[i]] centd;
    vector[neighb[i]] cente;
    centd = to_vector(CSs[i][1:neighb[i],d]-wmeans[d]);
    cente = to_vector(CSs[i][1:neighb[i],e]-wmeans[e]);
    varcovar[d,e] = dot_product(centd .* w,cente)/(sum(w)*(sdprod^(two/D)));
    }
    }
    log_lik[i] = multi_normal_lpdf(pos[i,] | wmeans, varcovar*(nu^2) + unitm*(eta^2)); 
    }
}"

#Model that works with both constant and neigbour proportional polity variance
code.VDI<-"
data{
  int N;
  int D;
  int nmax;
  int neighb[N];
  matrix[N,D] pos;
  matrix[N,D] CSpush;
  matrix[nmax,D] CSs[N];
  matrix[N,nmax] distanc;
  matrix[D,D] unitm;
}
parameters{
  real<lower=0> eta;
  real<lower=0> nu;
  real<lower=0> rhosq;
  real<lower=0> deltas;
}
model{
  eta ~ exponential( 1 );
  nu ~ exponential( 1 );
  rhosq ~ exponential( 1 );
  deltas ~ exponential( 1 );
    for ( i in 1:N ){
    vector[neighb[i]] w;
    vector[D] wmeans;
    matrix[D,D] varcovar;
    w[1] = zetasq + deltas;
    for(nb in 2:neighb[i]){
    w[nb] = exp(-rhosq*((distanc[i,nb])^2));
    }
    for(d in 1:D){
    wmeans[d]=dot_product(CSs[i][1:neighb[i],d],w)/sum(w);
    }
    for(d in 1:D){
    for(e in 1:D){
    vector[neighb[i]] centd;
    vector[neighb[i]] cente;
    centd = to_vector(CSs[i][1:neighb[i],d]-wmeans[d]);
    cente = to_vector(CSs[i][1:neighb[i],e]-wmeans[e]);
    varcovar[d,e] = dot_product(centd .* w,cente)/sum(w);
    }
    }
     target += multi_normal_lpdf(pos[i,] | wmeans, varcovar*(nu^2) + unitm*(eta^2));
    }
}
generated quantities{
    vector[N] log_lik;
    for ( i in 1:N ){
    vector[neighb[i]] w;
    vector[D] wmeans;
    matrix[D,D] varcovar;
    w[1] = zetasq + deltas;
    for(nb in 2:neighb[i]){
    w[nb] = exp(-rhosq*((distanc[i,nb])^2));
    }
    for(d in 1:D){
    wmeans[d]=dot_product(CSs[i][1:neighb[i],d],w)/sum(w);
    }
    for(d in 1:D){
    for(e in 1:D){
    vector[neighb[i]] centd;
    vector[neighb[i]] cente;
    centd = to_vector(CSs[i][1:neighb[i],d]-wmeans[d]);
    cente = to_vector(CSs[i][1:neighb[i],e]-wmeans[e]);
    varcovar[d,e] = dot_product(centd .* w,cente)/sum(w);
    }
    }
    log_lik[i] = multi_normal_lpdf(pos[i,] | wmeans, varcovar*(nu^2) + unitm*(eta^2)); 
    }
}"

#Fit the models
m.Null<-stan(model_code=code.Null,data=d.CSs,chains=4,cores=4)
m.Push<-stan(model_code=code.Push,data=d.CSs,chains=4,cores=4)
m.GP<-stan(model_code=code.GP,data=d.CSs,chains=4,cores=4)
m.GPcor<-stan(model_code=code.GPcor,data=d.CSs,chains=4,cores=4)
m.VDI<-stan(model_code=code.VDI,data=d.CSs,chains=4,cores=4)

save(m.Null,file="m.Null.Rdata")
save(m.Push,file="m.Push.Rdata")
save(m.GP,file="m.GP.Rdata")
save(m.GPcor,file="m.GPcor.Rdata")
save(m.VDI,file="m.VDI.Rdata")

load("m.Null.Rdata")
load("m.Push.Rdata")
load("m.GP.Rdata")
load("m.GPcor.Rdata")
load("m.VDI.Rdata")

precis(m.Null)
precis(m.Push)
precis(m.GP)
precis(m.GPcor)
precis(m.VDI)

(WAICs<-compare(m.Null,m.Push,m.GP,m.GPcor,m.VDI))

write.table(WAICs,"WAICs.txt",sep="\t",col.names=NA)
