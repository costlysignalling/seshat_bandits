raw<-read.csv("equinox2020_dataset_seshat.csv",stringsAsFactors=F)

head(raw)

(sumar<-summary(as.factor(raw$Variable),maxsum=2000))
length(sumar)

#One NGA.y thing is multiple PolID, so we are mostlz concerned with how 
table(raw$NGA.y,raw$PolID)[rownames(table(raw$NGA.y,raw$PolID))=="Kachi Plain",]
table(raw$NGA.y,raw$PolID)[,colnames(table(raw$NGA.y,raw$PolID))=="PkAcerN"]

#Create a composite variable of a polity p at time t
raw$pol<-paste(raw$NGA.y,raw$Date.From,raw$Date.To)

#So these are my new row names
(newrows<-names(summary(as.factor(raw$pol),maxsum=2000)))

#these are the correpsonding place names, start and end times
place<-sapply(1:length(newrows),function(i){take<-strsplit(newrows[i]," ")[[1]];return(paste(take[-c(length(take)-1,length(take))],collapse=" "))})
time1<-as.numeric(sapply(1:length(newrows),function(i){take<-strsplit(newrows[i]," ")[[1]];return(take[length(take)-1])}))
time2<-as.numeric(sapply(1:length(newrows),function(i){take<-strsplit(newrows[i]," ")[[1]];return(take[length(take)])}))

#create the new dataset
d<-data.frame(pol=newrows,place,time1,time2)

#Attempt to assign one level of the "from" value to each polity at each time step (used example is Administrative levels)
subs<-raw[raw$Variable=="Administrative levels",]
(Administrative.levels.from<-subs$Value.From[match(d$pol,subs$pol)])

nrow(d)
nrow(subs)

#apparently there are more inputs for some place-times
summary(as.factor(subs$pol))

#Ok, it just looks that there are duplicities
subs[subs$pol=="Susiana -3800 -3101",]
subs[subs$pol=="Southern Mesopotamia -312 -144",]

#In such cases - for now - I will take the minimum (and maximum for the Value.to bit). I will write it in a way that allows me to apply it to all columns
#Also apparently there are information missing for some combinations of place and time
subs[subs$pol=="Middle Ganga -3000 -600",]

vars<-names(sumar)

for(v in vars){
  subs<-raw[raw$Variable==v,]
  
  #New variable for the given placetime, retains the information about multiple levels
  newvar.from<-sapply(d$pol,function(x){
    candidates<-subs$Value.From[subs$pol==x]
    #What to do with unknowns and suspected unknowns
    candidates[candidates=="suspected unknown"]<-NA
    candidates[candidates=="unknown"]<-NA
    if(length(candidates)==0){
      return(NA)
    }
    if(length(candidates)==1){
      return(candidates)
    }else{
      return(paste(candidates,collapse="|"))
    }
  })
  
  newvar.to<-sapply(d$pol,function(x){
    candidates<-subs$Value.To[subs$pol==x]
    candidates[candidates=="suspected unknown"]<-NA
    candidates[candidates=="unknown"]<-NA
    if(length(candidates)==0){
      return(NA)
    }
    if(length(candidates)==1){
      return(candidates)
    }else{
      return(paste(candidates,collapse="|"))
    }
  })
  
  d<-cbind(d,as.character(newvar.from),as.character(newvar.to))
  
  #Name the columns properly and get rid of the gap
  names(d)[c(ncol(d)-c(1,0))]<-paste(gsub(" ",".",v),c("from","to"),sep=".")
}

#Not all variables are equally interesting, some (such as aletrantive names) seem useless. Here we do a bit of a variable selection
dr<-d #save it into a "d.rough" object and start tossing the cleande data there

names(dr)
d<-dr[,1:4]

#Administrative levels are that kind of numeric variable that can have range, we will treat them all the smae - take the from-to mean, solve multiple options by taking the min in "from" and max in "to"
treat1<-function(dim.from,dim.to){
  from<-lapply(as.character(dim.from),
               function(x){if(is.na(x)){return(NA)}else{return(strsplit(x,"|",fixed=T)[[1]])}})
  from<-sapply(from,function(x){min(as.numeric(x))})
  
  to<-lapply(as.character(dim.to),
             function(x){if(is.na(x)){return(NA)}else{return(strsplit(x,"|",fixed=T)[[1]])}})
  to<-sapply(to,function(x){x<-as.numeric(x);x<-x[!is.na(x)];if(length(x)==0){return(NA)}else{return(max(x,na.rm=T))}})
  
  output<-apply(cbind(from,to),1,mean,na.rm=T)
  output[is.nan(output)]<-NA
  return(output)
}

d$Administrative.levels<-treat1(dr$Administrative.levels.from,dr$Administrative.levels.to)

#Binary variables such as "Articles" have no "to". In the "from" column, sometimes there are multiple options. We will treat them in another way: If at least one of them says "present" we treat it as present. inferred levels reduce to the normal levels, inferred present to present and inferred absent to absent
treat2<-function(dim.from,dim.to){
  from<-lapply(as.character(dim.from),
               function(x){if(is.na(x)){return(NA)}else{return(strsplit(x,"|",fixed=T)[[1]])}})
  from<-sapply(from,function(x){ifelse("present" %in% x, "present",ifelse("inferred present" %in% x, "present",ifelse("absent" %in% x, "absent",ifelse("inferred absent" %in% x, "absent",NA))))})
  return(from)
}

d$Articles<-treat2(dr$Articles.from)

#Here we can continue in the same way (either treat1 or treat2), most variables fit tnto one of these two categories
d$Atlatl<-treat2(dr$Atlatl.from)
d$Battle.axes<-treat2(dr$Battle.axes.from)
d$Breastplates<-treat2(dr$Breastplates.from)

