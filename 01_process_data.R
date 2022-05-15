raw<-read.csv("equinox2020_dataset_seshat.csv",stringsAsFactors=F,encoding="UTF-8")

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
dr<-d #save it into a "d.rough" object and start tossing the cleaned data there

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
treat2<-function(dim.from){
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
d$Bridges<-treat2(dr$Bridges.from)
d$Bronze<-treat2(dr$Bronze.from)
d$Calendar<-treat2(dr$Calendar.from)
d$Camels<-treat2(dr$Camels.from)
d$Canals<-treat2(dr$Canals.from)

#Irrelevant variable: Capital.from

d$Complex.fortifications<-treat2(dr$Complex.fortifications.from)
d$Composite.bow<-treat2(dr$Composite.bow.from)
d$Constraint.on.executive.by.government<-treat2(dr$Constraint.on.executive.by.government.from)
d$Constraint.on.executive.by.nongovernment<-treat2(dr$`Constraint.on.executive.by.non-government.from`)
d$Copper<-treat2(dr$Copper.from)
d$Couriers<-treat2(dr$Couriers.from)
d$Courts<-treat2(dr$Courts.from)
d$Crossbow<-treat2(dr$Crossbow.from)
d$Daggers<-treat2(dr$Daggers.from)

#Possible source of problems, first we need to decide the undecide cases. WE TAKE THE MOST ADVANCED VERSION AND MERGE SEVERAL BITS, WE LET NONE AND UNKNOWN AS SEPARAETD
treat3<-function(dim.from){
  from<-lapply(as.character(dim.from),
               function(x){if(is.na(x)){return(NA)}else{return(strsplit(x,"|",fixed=T)[[1]])}})
  from<-sapply(from,function(x){ifelse("unitary state" %in% x, "unitary state",ifelse("unitary" %in% x, "unitary state",ifelse("confederated state (1792 BCE-)" %in% x, "confederated state",ifelse("confederated state" %in% x, "confederated state",ifelse("loose" %in% x, "loose",ifelse("nominal allegiance" %in% x, "nominal allegiance",ifelse("nominal" %in% x, "nominal allegiance",ifelse("quasi-polity" %in% x, "quasi-polity",ifelse("quasi" %in% x, "quasi-polity",ifelse("polity" %in% x, "quasi-polity",ifelse("none" %in% x, "quasi-polity",ifelse("unknown" %in% x, "unknown",NA))))))))))))})
  return(from)
}

d$Degree.of.centralization<-treat3(dr$Degree.of.centralization.from)

sum(is.na(dr$Degree.of.centralization.from))
sum(is.na(d$Degree.of.centralization))
#We assigned all succesfully

d$Ditch<-treat2(dr$Ditch.from)
d$Dogs<-treat2(dr$Dogs.from)
d$Donkeys<-treat2(dr$Donkeys.from)
d$drinking.water.supply.systems<-treat2(dr$drinking.water.supply.systems.from)
d$Earth.ramparts<-treat2(dr$Earth.ramparts.from)
d$Elephants<-treat2(dr$Elephants.from)
d$elite.status.is.hereditary<-treat2(dr$elite.status.is.hereditary.from)
d$Examination.system<-treat2(dr$Examination.system.from)
d$Fiction<-treat2(dr$Fiction.from)
d$food.storage.sites<-treat2(dr$food.storage.sites.from)
d$Foreign.coins<-treat2(dr$Foreign.coins.from)
d$Formal.legal.code<-treat2(dr$Formal.legal.code.from)
d$Fortified.camps<-treat2(dr$Fortified.camps.from)
d$Fulltime.bureaucrats<-treat2(dr$`Full-time.bureaucrats.from`)
d$General.postal.service<-treat2(dr$General.postal.service.from)
d$Gunpowder.siege.artillery<-treat2(dr$Gunpowder.siege.artillery.from)
d$Handheld.firearms<-treat2(dr$Handheld.firearms.from)
d$Helmets<-treat2(dr$Helmets.from)
d$History<-treat2(dr$History.from)
d$Horses<-treat2(dr$Horses.from)
d$Chainmail<-treat2(dr$Chainmail.from)
d$Ideological.reinforcement.of.equality<-treat2(dr$Ideological.reinforcement.of.equality.from)
d$Ideological.thought.equates.elites.and.commoners<-treat2(dr$Ideological.thought.equates.elites.and.commoners.from)
d$Ideological.thought.equates.rulers.and.commoners<-treat2(dr$Ideological.thought.equates.rulers.and.commoners.from)
d$Ideology.reinforces.prosociality<-treat2(dr$Ideology.reinforces.prosociality.from)
d$Impeachment<-treat2(dr$Impeachment.from)
d$Indigenous.coins<-treat2(dr$Indigenous.coins.from)
d$Iron<-treat2(dr$Iron.from)
d$irrigation.systems<-treat2(dr$irrigation.systems.from)
d$Javelins<-treat2(dr$Javelins.from)
d$Judges<-treat2(dr$Judges.from)
d$Laminar.armor<-treat2(dr$Laminar.armor.from)

#I will translate the Language variable slightly differently than might be expected - into the number of languages that are spoken in given polity, since I am interested in cultural variance
treat4<-function(dim.from){
  from<-lapply(as.character(dim.from),
               function(x){if(is.na(x)){return(NA)}else{return(strsplit(x,"|",fixed=T)[[1]])}})
  from<-lapply(from,unique)
  return(sapply(from,length))
}
d$Languages<-treat4(dr$Language.from)

d$Leather.cloth<-treat2(dr$Leather.cloth.from)
d$Limb.protection<-treat2(dr$Limb.protection.from)
d$Lists.tables.and.classifications<-treat2(dr$Lists.tables.and.classifications.from)
d$Long.walls<-treat2(dr$Long.walls.from)
d$markets<-treat2(dr$markets.from)
d$Merchant.ships.pressed.into.service<-treat2(dr$Merchant.ships.pressed.into.service.from)
d$Merit.promotion<-treat2(dr$Merit.promotion.from)
d$Military.levels<-treat1(dr$Military.levels.from,dr$Military.levels.to)
d$Mines.or.quarries<-treat2(dr$Mines.or.quarries.from)
d$Mnemonic.devices<-treat2(dr$Mnemonic.devices.from)
d$Moat<-treat2(dr$Moat.from)
d$Modern.fortifications<-treat2(dr$Modern.fortifications.from)
d$Moral.concern.is.primary<-treat2(dr$Moral.concern.is.primary.from)
d$Moralizing.enforcement.in.afterlife<-treat2(dr$Moralizing.enforcement.in.afterlife.from)
d$Moralizing.enforcement.in.this.life<-treat2(dr$Moralizing.enforcement.in.this.life.from)
d$Moralizing.enforcement.is.agentic<-treat2(dr$Moralizing.enforcement.is.agentic.from)
d$Moralizing.enforcement.is.certain<-treat2(dr$Moralizing.enforcement.is.certain.from)
d$Moralizing.enforcement.is.targeted<-treat2(dr$Moralizing.enforcement.is.targeted.from)
d$Moralizing.enforcement.of.rulers<-treat2(dr$Moralizing.enforcement.of.rulers.from)
d$Moralizing.norms.are.broad<-treat2(dr$Moralizing.norms.are.broad.from)
d$Moralizing.religion.adopted.by.commoners<-treat2(dr$Moralizing.religion.adopted.by.commoners.from)
d$Moralizing.religion.adopted.by.elites<-treat2(dr$Moralizing.religion.adopted.by.elites.from)
d$Nonphonetic.writing<-treat2(dr$`Non-phonetic.writing.from`)
d$Nonwritten.records<-treat2(dr$Nonwritten.records.from)
d$Paper.currency<-treat2(dr$Paper.currency.from)
d$Philosophy<-treat2(dr$Philosophy.from)
d$Phonetic.alphabetic.writing<-treat2(dr$Phonetic.alphabetic.writing.from)
d$Plate.armor<-treat2(dr$Plate.armor.from)
d$Polearms<-treat2(dr$Polearms.from)

d$Polity.Population<-treat1(dr$Polity.Population.from,dr$Polity.Population.to) #This variable is a utility function! Not to be used in the culturespace construction!
#Old upper Egypt is the NA
cbind(as.character(d$pol),d$Polity.Population,dr$Polity.Population.from,dr$Polity.Population.to)[is.na(d$Polity.Population) & !is.na(dr$Polity.Population.from)]

d$Polity.territory<-treat1(dr$Polity.territory.from,dr$Polity.territory.to) #This variable is a utility function! Not to be used in the culturespace construction!
d$Population.of.the.largest.settlement<-treat1(dr$Population.of.the.largest.settlement.from,dr$Population.of.the.largest.settlement.to) #This variable is a utility function! Not to be used in the culturespace construction!

d$Ports<-treat2(dr$Ports.from)
d$Postal.stations<-treat2(dr$Postal.stations.from)
d$Practical.literature<-treat2(dr$Practical.literature.from)

#Irrelevant variable: dr$`preceding.(quasi)polity.from`

d$Precious.metals<-treat2(dr$Precious.metals.from)
d$production.of.public.goods<-treat2(dr$production.of.public.goods.from)
d$Professional.Lawyers<-treat2(dr$Professional.Lawyers.from)
d$Professional.military.officers<-treat2(dr$Professional.military.officers.from)
d$Professional.priesthood<-treat2(dr$Professional.priesthood.from)
d$Professional.soldiers<-treat2(dr$Professional.soldiers.from)

#Irrelevant variable: dr$RA.from (Research Assistant)
#Irrelevant variable: dr$`relationship.to.preceding.(quasi)polity.from`

d$Religious.levels<-treat1(dr$Religious.levels.from,dr$Religious.levels.to)

d$Religious.literature<-treat2(dr$Religious.literature.from)
d$Roads<-treat2(dr$Roads.from)
d$Rulers.are.gods<-treat2(dr$Rulers.are.gods.from)
d$Rulers.are.legitimated.by.gods<-treat2(dr$Rulers.are.legitimated.by.gods.from)
d$Sacred.Texts<-treat2(dr$Sacred.Texts.from)

d$scale.of.supracultural.interaction<-treat1(dr$`scale.of.supra-cultural.interaction.from`,dr$`scale.of.supra-cultural.interaction.to`) #This variable is a utility function! Not to be used in the culturespace construction!

d$Scaled.armor<-treat2(dr$Scaled.armor.from)
d$Scientific.literature<-treat2(dr$Scientific.literature.from)
d$Script<-treat2(dr$Script.from)
d$Self.bow<-treat2(dr$Self.bow.from)

d$Settlement.hierarchy<-treat1(dr$Settlement.hierarchy.from,dr$Settlement.hierarchy.to)

d$Settlements.in.a.defensive.position<-treat2(dr$Settlements.in.a.defensive.position.from)
d$Shields<-treat2(dr$Shields.from)
d$Slings<-treat2(dr$Slings.from)
d$Small.vessels.canoes.etc<-treat2(dr$`Small.vessels.(canoes.etc).from`)
d$Spears<-treat2(dr$Spears.from)
d$Specialized.government.buildings<-treat2(dr$Specialized.government.buildings.from)
d$Specialized.military.vessels<-treat2(dr$Specialized.military.vessels.from)
d$Steel<-treat2(dr$Steel.from)
d$Stone.walls.mortared<-treat2(dr$`Stone.walls.(mortared).from`)
d$Stone.walls.nonmortared<-treat2(dr$`Stone.walls.(non-mortared).from`)

#Irrelevant variable: dr$`succeeding.(quasi)polity.from`

# unknown/ none/ alliance/ nominal allegiance/ personal union/ vassalage/

#Possible source of problems, first we need to decide the undecide cases. WE TAKE THE MOST ADVANCED VERSION AND MERGE SEVERAL BITS, WE LET NONE AND UNKNOWN AS SEPARAETD
treat5<-function(dim.from){
  from<-lapply(as.character(dim.from),
               function(x){if(is.na(x)){return(NA)}else{return(strsplit(x,"|",fixed=T)[[1]])}})
  from<-sapply(from,function(x){ifelse("vassalage" %in% x, "vassalage",ifelse("personal union" %in% x, "personal union",ifelse("nominal allegiance" %in% x, "nominal allegiance",ifelse("alliance" %in% x, "alliance",ifelse("Alliance" %in% x, "alliance",ifelse("none" %in% x, "none",ifelse("unknown" %in% x, "unknown",ifelse("uncoded" %in% x, NA,NA))))))))})
  return(from)
}

d$Suprapolity.relations<-treat5(dr$`Supra-polity.relations.from`) #Also not very cultural dimension

#Irrelevant variable: dr$Supracultural.entity.from

d$Swords<-treat2(dr$Swords.from)
d$Tension.siege.engines<-treat2(dr$Tension.siege.engines.from)
d$Tokens<-treat2(dr$Tokens.from)
d$War.clubs<-treat2(dr$War.clubs.from)
d$Wood.bark.etc<-treat2(dr$Wood.bark.etc.from)
d$Wooden.palisades<-treat2(dr$Wooden.palisades.from)
d$Written.records<-treat2(dr$Written.records.from)

#Rearange in way suc that there are first variables that indicate which place and time it referes to
#then that there are utility function variables (pop size territory etc.)
#then treat1-like variables (+language diversity) things with levels
#multistate variables
#then treat2 (binary) variables

names(d)
ncol(d)

d<-cbind(d[1:4],d[c(87:89,105)],d[c(5,65,99,110,57)],d[c(24,121)],d[c(6:23,25:56,58:64,66:86,90:98,100:104,106:109,111:120,122:128)])
ncol(d)

names(d) #Now 1:4 are identifiers, 5:8 utility function noncultural variables, 9:13 quntitative variables, 14:15 multistate variables, 16:128 (end) simple binary variables

#d is the not filled dataset
d


# we convert d to a version of a dataset that binarizes everything in principle (not even multistate, but also "continuous" variables - I have seen this in a piece of work by Jakub Binter and Hermann Prossinger)

db<-d[1:8] #db as d binary

levels(as.factor(d$Administrative.levels))
levels(as.factor(d$Military.levels))
levels(as.factor(d$Religious.levels))
levels(as.factor(d$Settlement.hierarchy))
levels(as.factor(d$Languages))

#For now we will treat all the levels as multistate essentially, if it is intermediate level (x.5) we will indicate it as 0.5 to x and 0.5 to x+1

continuousToBinary<-function(name){
  v<-c(d[name])[[1]]
  levs<-unique(c(ceiling(v),floor(v)))
  levs<-sort(levs[!is.na(levs)])
  cols<-lapply(levs,function(x){ifelse(v==x,1,ifelse(v==x+0.5,0.5,ifelse(v==x-0.5,0.5,ifelse(is.na(x),NA,0))))})
  columns<-as.data.frame(do.call(cbind,cols))
  names(columns)<-paste(name,levs,sep=".")
  return(columns)
}

#The function works like this:
continuousToBinary("Administrative.levels")

for(name in names(d)[9:13]){
  db<-cbind(db,continuousToBinary(name))
}

d$Degree.of.centralization<-as.factor(d$Degree.of.centralization)
levels(d$Degree.of.centralization)<-c("confederated.state","loose","nominal.allegiance","quasi.polity","unitary.state")

multistateToBinary<-function(name){
  v<-c(d[name])[[1]]
  levs<-unique(v)
  levs<-sort(levs[!is.na(levs)])
  cols<-lapply(levs,function(x){ifelse(v==x,1,ifelse(is.na(x),NA,0))})
  columns<-as.data.frame(do.call(cbind,cols))
  names(columns)<-paste(name,levs,sep=".")
  return(columns)
}

for(name in names(d)[14:15]){
  db<-cbind(db,multistateToBinary(name))
}

#The same function can be used to present/absent
for(name in names(d)[16:128]){
  db<-cbind(db,multistateToBinary(name))
}

#Simple labels
write.table(d,"data_labels.txt",sep="\t",row.names=F)

#Turn ordered variables to numeric
#unknown/ quasi-polity/ nominal/ loose/ confederated state /unitary state 
v<-d$Degree.of.centralization
levels(v)
d$Degree.of.centralization<-ifelse(v=="none" | v=="quasi.polity",0,ifelse(v=="nominal",1,ifelse(v=="loose",2,ifelse(v=="nominal",3,ifelse(v=="confederated.state",4,ifelse(v=="unitary.state",5,NA))))))

v<-d$Suprapolity.relations
levels(as.factor(v))
d$Suprapolity.relations<-ifelse(v=="none",0,ifelse(v=="alliance",1,ifelse(v=="nominal allegiance",2,ifelse(v=="personal union",3,ifelse(v=="vassalage",4,NA)))))

for(i in 16:128){
  v<-d[,i]
  d[,i]<-ifelse(v=="absent",0,ifelse(v=="present",1,NA))
}

write.table(d,"data_numbers_not_filled.txt",sep="\t",row.names=F)

#Filling the data with arithmetic mean is stupid (early missing data are overrepresented), but we will fix this later
#Fill the missing data with the arithmetic mean
for(i in 5:128){
    v<-d[,i]
    v[is.na(v)]<-mean(v,na.rm=T)
    d[,i]<-v
}

write.table(d,"data_numbers_filled.txt",sep="\t",row.names=F)
names(db)

write.table(db,"data_indicator_binary_not_filled.txt",sep="\t",row.names=F)

for(i in 5:ncol(db)){
  v<-db[,i]
  v[is.na(v)]<-mean(v,na.rm=T)
  db[,i]<-v
}

write.table(db,"data_indicator_binary_filled.txt",sep="\t",row.names=F)
