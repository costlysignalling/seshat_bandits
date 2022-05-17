
d<-read.delim("data_numbers_filled.txt")
dv<-d[,1:8]
names(dv)
d<-d[,9:ncol(d)]
library(ggfortify)

PC <- 
princomp(~Administrative.levels+Articles+Atlatl+Battle.axes+Breastplates+Bridges+Bronze+Calendar+Camels+Canals+Chainmail+Complex.fortifications+Composite.bow+Constraint.on.executive.by.government+Constraint.on.executive.by.nongovernment+Copper+Couriers+Courts+Crossbow+Daggers+Degree.of.centralization+Ditch+Dogs+Donkeys+drinking.water.supply.systems+Earth.ramparts+Elephants+elite.status.is.hereditary+Examination.system+Fiction+food.storage.sites+Foreign.coins+Formal.legal.code+Fortified.camps+Fulltime.bureaucrats+General.postal.service+Gunpowder.siege.artillery+Handheld.firearms+Helmets+History+Horses+Ideological.reinforcement.of.equality+Ideological.thought.equates.elites.and.commoners+Ideological.thought.equates.rulers.and.commoners+Ideology.reinforces.prosociality+Impeachment+Indigenous.coins+Iron+irrigation.systems+Javelins+Judges+Laminar.armor+Languages+Leather.cloth+Limb.protection+Lists.tables.and.classifications+Long.walls+markets+Merchant.ships.pressed.into.service+Merit.promotion+Military.levels+Mines.or.quarries+Mnemonic.devices+Moat+Modern.fortifications+Moral.concern.is.primary+Moralizing.enforcement.in.afterlife+Moralizing.enforcement.in.this.life+Moralizing.enforcement.is.agentic+Moralizing.enforcement.is.certain+Moralizing.enforcement.is.targeted+Moralizing.enforcement.of.rulers+Moralizing.norms.are.broad+Moralizing.religion.adopted.by.commoners+Moralizing.religion.adopted.by.elites+Nonphonetic.writing+Nonwritten.records+Paper.currency+Philosophy+Phonetic.alphabetic.writing+Plate.armor+Polearms+Ports+Postal.stations+Practical.literature+Precious.metals+production.of.public.goods+Professional.Lawyers+Professional.military.officers+Professional.priesthood+Professional.soldiers+Religious.levels+Religious.literature+Roads+Rulers.are.gods+Rulers.are.legitimated.by.gods+Sacred.Texts+Scaled.armor+Scientific.literature+Script+Self.bow+Settlement.hierarchy+Settlements.in.a.defensive.position+Shields+Slings+Small.vessels.canoes.etc+Spears+Specialized.government.buildings+Specialized.military.vessels+Steel+Stone.walls.mortared+Stone.walls.nonmortared+Suprapolity.relations+Swords+Tension.siege.engines+Tokens+War.clubs+Wood.bark.etc+Wooden.palisades+Written.records,
         cor=FALSE, data=d)
print(unclass(loadings(PC)))
print(PC$sd^2)
print(PC$scores)
print(summary(PC))
screeplot(PC)
d <<- within(d, {
    PC5 <- PC$scores[,5]
    PC4 <- PC$scores[,4]
    PC3 <- PC$scores[,3]
    PC2 <- PC$scores[,2]
    PC1 <- PC$scores[,1]
})

PCscores <- cbind(dv,PC$scores[,1],PC$scores[,2],PC$scores[,3],PC$scores[,4],PC$scores[,5])

write.csv(PCscores, "PCAscores.csv")

autoplot(PC, loadings = TRUE)
plot(PC$scores)
