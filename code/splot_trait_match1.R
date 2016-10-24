### Create the workspace for sPlot2.0 ###
### written by Helge Bruelheide, 20.10.2016 ###

# There are x files:
# 1. DT.Rdata: a file in long format, including, among others, 
#    PlotObservationID, species, Cover
#    and Relative.cover, while DT holds all original information,
#    including the "Matched concept" which is the unified name from 
#    Turboveg 3,
#    DT2 has been shortened to the relevant fields and 
#    contains only data with species that have a resolved name in the backbone
#    Both DT and DT contain species for which we do not have traits
# 2. splot.header, read in from sPlot_2015_07_29_header.csv: 
#    header file for DT, holding plot information on database origin, 
#    country, lat, long, layer height and cover etc.
#    splot.header matches to DT, but not to DT2
# 3. backbone.splot2.1.try3.Rdata: taxonomic backbone holding all sPlot and
#    TRY species names. These names are in names.sPlot.TRY, the resolved
#    names are in name.short.correct
# 4. environment
# 5. Export_sPlot_2016_09_12.csv: gap filled data from TRY3.0
# 6. CWM.Rdata: Community weighted means based on all TRY gap filled data
# 7. FD.Rao.Rdata, Rao's Q for all plots, using divc from ade4
# 8. mpd.abu.Rdata, mntd.abu.Rdata, mpd.pa.Rdata, mntd.pa.Rdata
#    mean pairwise distance and mean nearest neighbour distance, 
#    both based on abundances and presence/absence, using Picante

library(data.table)

load("/data/sPlot2.0/DT_20161021.RData") # server directory
load("data/DT_20161021.RData") # local directory
# 23586216 obs. of  14 variables.
# the field "species" holdes the resolved name according to the backbone
# the field "Matched concept" is the original Turboveg3 name.
setkey(DT,PlotObservationID)

splot.header <- fread("/data/sPlot2.0/sPlot_2015_07_29_header.csv", 
                      sep = "\t", na.strings=c("","NA"))
# You get a warning, which you can ignore. 
#  1121244 rows and 43 columns.


###### Taxonomic Backbone #####
load("/data/sPlot2.0/backbone.splot2.1.try3.Rdata")
str(backbone.splot2.1.try3) 
#data.frame':	130602 obs. of  33 variables:
' $ Name_number                 : int  1 2 3 4 5 6 7 8 9 10 ...
$ names.sPlot.TRY             : chr  "?" "0" "[1269 Chlorophytum platt]" "[1284 Echinochloa]" ...
$ names.corr.string           : chr  "?" "0" "[1269 Chlorophytum platt]" "[1284 Echinochloa]" ...
$ sPlot.TRY                   : chr  "S" "S" "S" "S" ...
$ Name_submitted              : chr  "Spermatophyta sp." "Spermatophyta sp." "Chlorophytum sp. [1269]" "Echinochloa sp." ...
$ Overall_score               : num  0 0 0.9 0.9 0.9 0.9 0.9 0 0.9 0 ...
$ Name_matched                : chr  "No suitable matches found." "No suitable matches found." "Chlorophytum" "Echinochloa" ...
$ Name_matched_rank           : chr  "" "" "genus" "genus" ...
$ Name_score                  : num  0 0 1 1 1 1 1 0 1 0 ...
$ Family_score                : num  0 0 NA NA NA NA 1 0 1 0 ...
$ Name_matched_accepted_family: chr  "" "" "Asparagaceae" "Poaceae" ...
$ Genus_matched               : chr  "" "" "Chlorophytum" "Echinochloa" ...
$ Genus_score                 : num  0 0 1 1 1 1 NA 0 NA 0 ...
$ Specific_epithet_matched    : chr  "" "" "" "" ...
$ Specific_epithet_score      : num  0 0 NA NA NA NA NA 0 NA 0 ...
$ Unmatched_terms             : chr  "" "" "\"\"sp. [1269]" "\"\"sp." ...
$ Taxonomic_status            : chr  "" "" "Accepted" "Accepted" ...
$ Accepted_name               : chr  "" "" "Chlorophytum" "Echinochloa" ...
$ Accepted_name_author        : chr  "" "" "" "" ...
$ Accepted_name_rank          : chr  "" "" "genus" "genus" ...
$ Accepted_name_url           : chr  "" "" "http://www.theplantlist.org/tpl1.1/search?q=Chlorophytum" "http://www.theplantlist.org/tpl1.1/search?q=Echinochloa" ...
$ Accepted_name_species       : chr  "" "" "" "" ...
$ Accepted_name_family        : chr  "" "" "Asparagaceae" "Poaceae" ...
$ Selected                    : chr  "true" "true" "true" "true" ...
$ Source                      : chr  "" "" "tpl" "tpl" ...
$ Warnings                    : chr  " " " " " " " " ...
$ Manual.matching             : chr  NA NA NA NA ...
$ Status.correct              : chr  "No suitable matches found." "No suitable matches found." "Accepted" "Accepted" ...
$ name.correct                : chr  "No suitable matches found." "No suitable matches found." "Chlorophytum" "Echinochloa" ...
$ rank.correct                : chr  "higher" "higher" "genus" "genus" ...
$ family.correct              : chr  "" "" "Asparagaceae" "Poaceae" ...
$ name.short.correct          : chr  NA NA "Chlorophytum" "Echinochloa" ...
$ rank.short.correct          : chr  "higher" "higher" "genus" "genus" ...
'
# TRY 3.0
TRY3.0 <- read.csv("/data/sPlot2.0/Export_sPlot_2016_09_12.csv")
str(TRY3.0)
#632938 obs. of  33 variables:
any(is.na(TRY3.0[,c(2:19)])) #F
any(TRY3.0[,c(2:19)]==0) #F


index2 <- match(TRY3.0$Species,backbone.splot2.1.try3$names.sPlot.TRY)
str(index2)
TRY3.0$name.short.correct <- backbone.splot2.1.try3$name.short.correct[index2]
library(stringr)
TRY3.0$genus.short.correct <- word(TRY3.0$name.short.correct,1)
head(TRY3.0[,c(34,35)],1000)
TRY3.0$rank.short.correct <- backbone.splot2.1.try3$rank.short.correct[index2]
table(TRY3.0$rank.short.correct, exclude=NULL)
'family   genus  higher species    <NA> 
     39   21021       1  611655     222 '

### Calculations ###
### take the log of all trait values in TRY3.0 ###
for (i in 2:19){
  TRY3.0[,i] <- log(TRY3.0[,i])
}
any(is.na(TRY3.0[,c(2:19)])) #F

TRY.all.n.3 <- aggregate(TRY3.0[,1], by=list(TRY3.0$name.short.correct), FUN=length)
head(TRY.all.n.3)
str(TRY.all.n.3) #52032 obs. of  2 variables:
names(TRY.all.n.3)
names(TRY.all.n.3) <- c("StandSpeciesName","n")

TRY.all.mean.3 <- aggregate(TRY3.0[,c(2:19)], by=list(TRY3.0$name.short.correct), FUN=mean)
str(TRY.all.mean.3) #52032 obs. of  19 variables:
names(TRY.all.mean.3)
' [1] "Group.1" "X1"      "X4"      "X11"     "X13"     "X14"     "X15"     "X18"     "X26"     "X27"     "X47"    
[12] "X50"     "X56"     "X78"     "X138"    "X163"    "X169"    "X237"    "X282"  '
names(TRY.all.mean.3) <- c("StandSpeciesName","LeafArea.mean", "StemDens.mean", "SLA.mean", "LeafC.perdrymass.mean",
                           "LeafN.mean","LeafP.mean", "PlantHeight.mean", "SeedMass.mean", "Seed.length.mean",
                           "LDMC.mean","LeafNperArea.mean", "LeafNPratio.mean", "Leaf.delta.15N.mean", 
                           "Seed.num.rep.unit.mean", "Leaffreshmass.mean", "Stem.cond.dens.mean", 
                           "Disp.unit.leng.mean", "Wood.vessel.length.mean")
any(is.na(TRY.all.mean.3$SLA.mean)) #F

TRY.all.sd.3 <- aggregate(TRY3.0[,c(2:19)], by=list(TRY3.0$name.short.correct), FUN=sd)
str(TRY.all.sd.3) #52032 obs. of  19 variables:
names(TRY.all.sd.3)
names(TRY.all.sd.3) <- c("StandSpeciesName","LeafArea.sd", "StemDens.sd", "SLA.sd", "LeafC.perdrymass.sd",
                         "LeafN.sd","LeafP.sd", "PlantHeight.sd", "SeedMass.sd", "Seed.length.sd",
                         "LDMC.sd","LeafNperArea.sd", "LeafNPratio.sd", "Leaf.delta.15N.sd", 
                         "Seed.num.rep.unit.sd", "Leaffreshmass.sd", "Stem.cond.dens.sd", 
                         "Disp.unit.leng.sd", "Wood.vessel.length.sd")
any(is.na(TRY.all.sd.3$SLA.sd)) #T
TRY.all.mean.sd.3.by.taxon <- data.frame(TRY.all.n.3,TRY.all.mean.3[,c(2:19)],TRY.all.sd.3[,c(2:19)])
str(TRY.all.mean.sd.3.by.taxon) #52032  obs. of  38 variables
'data.frame:	52032  obs. of  38 variables:
$ StandSpeciesName       : chr  "Aa" "Aaronsohnia pubescens" "Abarema" "Abarema adenophora" ...
 $ n                      : int  1 1 8 4 1 1 1 38 4 87 ...
$ LeafArea.mean          : num  6.61 5.33 7.2 6.96 7.37 ...
$ StemDens.mean          : num  -0.822 -0.823 -0.534 -0.427 -1.02 ...
$ SLA.mean               : num  2.24 3.09 2.43 2.38 2.7 ...
$ LeafC.perdrymass.mean  : num  6.17 6.11 6.18 6.26 6.14 ...
$ LeafN.mean             : num  2.92 3.07 3.3 3.19 3.38 ...
$ LeafP.mean             : num  -0.0143 1.0433 -0.1211 0.3403 0.4916 ...
$ PlantHeight.mean       : num  -0.498 -1.611 2.598 2.874 1.83 ...
$ SeedMass.mean          : num  -4.08 -1.69 4.27 4.49 4.26 ...
$ Seed.length.mean       : num  -0.317 0.337 1.949 2.021 2.244 ...
$ LDMC.mean              : num  -1.471 -1.586 -1.01 -0.798 -0.986 ...
$ LeafNperArea.mean      : num  0.8956 0.0878 0.8763 0.8343 0.6596 ...
$ LeafNPratio.mean       : num  2.54 1.95 3.49 3.26 3.06 ...
$ Leaf.delta.15N.mean    : num  0.38 0.4 0.888 1.258 1.36 ...
$ Seed.num.rep.unit.mean : num  9.56 10.73 2.04 1.64 5.76 ...
$ Leaffreshmass.mean     : num  -1.3303 -2.7662 0.0832 -0.2516 -0.7861 ...
$ Stem.cond.dens.mean    : num  3.67 4.47 1.77 2.14 2.78 ...
$ Disp.unit.leng.mean    : num  -0.555 0.445 2.734 2.916 2.235 ...
$ Wood.vessel.length.mean: num  6.13 5.32 5.88 5.73 5.32 ...
$ LeafArea.sd            : num  NA NA 0.0698 0.0303 NA ...
$ StemDens.sd            : num  NA NA 0.00778 0.11012 NA ...
$ SLA.sd                 : num  NA NA 0.0227 0.1063 NA ...
$ LeafC.perdrymass.sd    : num  NA NA 0.0014 0.0086 NA ...
$ LeafN.sd               : num  NA NA 0.025 0.0638 NA ...
$ LeafP.sd               : num  NA NA 0.0614 0.3936 NA ...
$ PlantHeight.sd         : num  NA NA 0.053 0.0626 NA ...
$ SeedMass.sd            : num  NA NA 0.0684 0.0315 NA ...
$ Seed.length.sd         : num  NA NA 0.0307 0.0457 NA ...
$ LDMC.sd                : num  NA NA 0.0346 0.0553 NA ...
$ LeafNperArea.sd        : num  NA NA 0.0412 0.098 NA ...
$ LeafNPratio.sd         : num  NA NA 0.0354 0.291 NA ...
$ Leaf.delta.15N.sd      : num  NA NA 0.0326 0.1651 NA ...
$ Seed.num.rep.unit.sd   : num  NA NA 0.192 0.211 NA ...
$ Leaffreshmass.sd       : num  NA NA 0.0702 0.0748 NA ...
$ Stem.cond.dens.sd      : num  NA NA 0.413 0.139 NA ...
$ Disp.unit.leng.sd      : num  NA NA 0.033 0.0927 NA ...
$ Wood.vessel.length.sd  : num  NA NA 0.288 0.147 NA ...'

save(TRY.all.mean.sd.3.by.taxon, file="/data/sPlot2.0/TRY.all.mean.sd.3.by.taxon.Rdata")


TRY.all.n.3 <- aggregate(TRY3.0[,1], by=list(TRY3.0$genus.short.correct), FUN=length)
head(TRY.all.n.3)
str(TRY.all.n.3) #7873 obs. of  2 variables:
names(TRY.all.n.3)
names(TRY.all.n.3) <- c("StandSpeciesName","n")

TRY.all.mean.3 <- aggregate(TRY3.0[,c(2:19)], by=list(TRY3.0$genus.short.correct), FUN=mean)
str(TRY.all.mean.3) #7873 obs. of  19 variables:
names(TRY.all.mean.3)
' [1] "Group.1" "X1"      "X4"      "X11"     "X13"     "X14"     "X15"     "X18"     "X26"     "X27"     "X47"    
[12] "X50"     "X56"     "X78"     "X138"    "X163"    "X169"    "X237"    "X282"  '
names(TRY.all.mean.3) <- c("StandSpeciesName","LeafArea.mean", "StemDens.mean", "SLA.mean", "LeafC.perdrymass.mean",
                           "LeafN.mean","LeafP.mean", "PlantHeight.mean", "SeedMass.mean", "Seed.length.mean",
                           "LDMC.mean","LeafNperArea.mean", "LeafNPratio.mean", "Leaf.delta.15N.mean", 
                           "Seed.num.rep.unit.mean", "Leaffreshmass.mean", "Stem.cond.dens.mean", 
                           "Disp.unit.leng.mean", "Wood.vessel.length.mean")
any(is.na(TRY.all.mean.3$SLA.mean)) #F

TRY.all.sd.3 <- aggregate(TRY3.0[,c(2:19)], by=list(TRY3.0$genus.short.correct), FUN=sd)
str(TRY.all.sd.3) #7873 obs. of  19 variables:
names(TRY.all.sd.3)
names(TRY.all.sd.3) <- c("StandSpeciesName","LeafArea.sd", "StemDens.sd", "SLA.sd", "LeafC.perdrymass.sd",
                         "LeafN.sd","LeafP.sd", "PlantHeight.sd", "SeedMass.sd", "Seed.length.sd",
                         "LDMC.sd","LeafNperArea.sd", "LeafNPratio.sd", "Leaf.delta.15N.sd", 
                         "Seed.num.rep.unit.sd", "Leaffreshmass.sd", "Stem.cond.dens.sd", 
                         "Disp.unit.leng.sd", "Wood.vessel.length.sd")
any(is.na(TRY.all.sd.3$SLA.sd)) #T

TRY.all.mean.sd.3.by.genus.species <- TRY.all.mean.sd.3.by.taxon
names(TRY.all.mean.sd.3.by.genus.species)
index3 <- match(TRY.all.mean.sd.3.by.taxon$StandSpeciesName, 
                backbone.splot2.1.try3$name.short.correct)
any(is.na(index3)) #F

which(backbone.splot2.1.try3$rank.short.correct[index3]=="genus")
table(backbone.splot2.1.try3$rank.short.correct[index3])
'family   genus  higher species 
      6    2342       1   49683 '
#backbone.splot2.1.try3$rank.short.correct[index3]=="genus"

index4 <- match(TRY.all.mean.sd.3.by.genus.species[backbone.splot2.1.try3$rank.short.correct[index3]=="genus",1],
                TRY.all.n.3$StandSpeciesName)
str(index4)                
      

TRY.all.mean.sd.3.by.genus.species[backbone.splot2.1.try3$rank.short.correct[index3]=="genus",2] <- TRY.all.n.3[index4,2]
for (i in 1:18){
  TRY.all.mean.sd.3.by.genus.species[backbone.splot2.1.try3$rank.short.correct[index3]=="genus",i+2] <- TRY.all.mean.3[index4,i+1]
  TRY.all.mean.sd.3.by.genus.species[backbone.splot2.1.try3$rank.short.correct[index3]=="genus",i+20] <- TRY.all.sd.3[index4,i+1]
}
head(TRY.all.mean.sd.3.by.genus.species[,c(1:3)],50)
head(TRY.all.mean.sd.3.by.taxon[,c(1:3)],50)
save(TRY.all.mean.sd.3.by.genus.species, file="/data/sPlot2.0/TRY.all.mean.sd.3.by.genus.species.Rdata")
#load("/data/sPlot2.0/TRY.all.mean.sd.3.by.genus.species.Rdata")
any(is.na(TRY.all.mean.sd.3.by.genus.species[,c(3:21)]))
#T
any(is.na(TRY.all.mean.sd.3.by.taxon[,c(3:21)]))
#T
###


any(is.na(DT$species)) #T
# there NA cases as not all "Matched concept" names in DT have resolved names
length(unique(DT$PlotObservationID)) #1121244
length(DT$species[!is.na(DT$species)]) #23555942
length(DT$species) #23586216
23586216-23555942 # 30274 NA names!!!
# it gives nonsense to match them with traits

index7 <- match(DT$species,TRY.all.mean.sd.3.by.genus.species$StandSpeciesName)
length(index7) #23586216
length(index7[!is.na(index7)]) 
# 21172989, with TRY3.0
# 21040927, with TRY2.0
# 19841429, with first TRY version
23586216 - 21172989 # 2413227 entries have no species in CWM
# traits are fewer as not all CWM rows have traits
(23586216 - 21172989)/23586216*100 
# 10.23151% of all entries 
(21172989)/23586216*100 
# which are  89.76849% of all entries
# previously: 86.79555% with TRY2.0
# previously: 18.15247 % and with first TRY version

DT2 <- DT[!is.na(DT$species),]
any(is.na(DT2$Relative.cover)) # F
length(unique(DT2$PlotObservationID)) #1121145
#length(unique(sPlot3$PlotObservationID)) #1121245
names(DT2)
names(DT2)[3] <- "Taxon.group"
names(DT2)[6] <- "Matched.concept"
names(DT2)
library(dplyr)
DT2 <- select(DT2,PlotObservationID,species, Taxon.group, Matched.concept, Layer, Cover, Relative.cover)
str(DT2) #23555942 obs. of  7 variables:
length(unique(DT2$PlotObservationID)) #1121145
plot.total.cover4 <- DT2[, list(total.cover=sum(Cover)), by=PlotObservationID]
str(plot.total.cover4) #1121145  
min(plot.total.cover4$total.cover) #0.001
max(plot.total.cover4$total.cover) #104704.1
any(is.na(plot.total.cover4$total.cover)) # F
length(plot.total.cover4$total.cover[plot.total.cover4$total.cover==0]) 
# 0 !!!
# all plots have cover

index4 <- match(DT2$PlotObservationID,plot.total.cover4$PlotObservationID)
length(index4) #23555942
any(is.na(index4)) # F

# recalculate Relative cover
### CAREFUL! This has to be done by layer now ###
DT2$Relative.cover <-  DT2$Cover/plot.total.cover4$total.cover[index4]

plot.total.cover5 <- DT2[, list(total.cover=sum(Relative.cover)), by=PlotObservationID]
min(plot.total.cover5$total.cover) #1
max(plot.total.cover5$total.cover) #1

save(DT2,file = "/data/sPlot2.0/DT2_20161021.RData")


length(unique(DT2$PlotObservationID)) #1121145 versus 1121244 before
index7 <- match(DT2$species,TRY.all.mean.sd.3.by.genus.species$StandSpeciesName)
length(index7) #23555942
length(index7[!is.na(index7)]) 
# 21172989, with TRY3.0
23555942 - 21172989 
# 2382953 entries with valid species names have no species in the 
# gap-filled trait file
# not all species there have traits
(23555942 - 21172989)/23555942*100 
# 10.11614% of all entries have no traits
(21172989)/23555942*100 
# which are  89.88386% of all entries

### CWM ###
mean(TRY.all.mean.sd.3.by.genus.species$SLA.mean) # NA
mean(TRY.all.mean.sd.3.by.genus.species$SLA.mean,na.rm=T) # 2.63365
# example
DT2$trait <- NA
DT2$trait <- TRY.all.mean.sd.3.by.genus.species$SLA.mean[index7]
length(DT2$trait[!is.na(DT2$trait)]) # 21172949
length(DT2$trait[is.na(DT2$trait)]) #   2382993
(21172949-2382993)/21172949
# 0.8874511 % of all records in sPlot have a valid trait
length(unique(DT2$species)) #61131
length(unique(DT2$species[!is.na(DT2$trait)])) #26666
26666/61131 # 0.4362108
str(DT2)

colnames(TRY.all.mean.sd.3.by.genus.species)
CWM2 <-  DT2[,list(CWM.SLA = weighted.mean(trait,Cover,na.rm = T)),by=PlotObservationID]
# I have checked that we do not need to prefilter only those entries that have a trait value
# this works fine
str(CWM2)
dim(CWM2) #1121145
which(colnames(TRY.all.mean.sd.3.by.genus.species)=="LeafArea.mean") # 3
which(colnames(TRY.all.mean.sd.3.by.genus.species)=="Wood.vessel.length.mean") # 20
CWM <- array(NA,c(dim(CWM2)[1],18),dimnames=list(CWM2$PlotObservationID,
                    colnames(TRY.all.mean.sd.3.by.genus.species)[3:20]))
rm(CWM2)
for (i in 1:18){
  DT2$trait <- NA
  DT2$trait <- TRY.all.mean.sd.3.by.genus.species[index7,i+2]
  CWM[,i] <-  DT2[,list(CWM.trait= weighted.mean(trait,Relative.cover,na.rm = T)),by=PlotObservationID]$CWM.trait
}
dim(CWM) #1121145      18
length(CWM[,"SLA.mean"][!is.na(CWM[,"SLA.mean"])]) #1116113
1116113/1121145 # 0.9955117% of all plots in sPlot2 have a CWM value

save(CWM, file="/data/sPlot2.0/CWM.Rdata") # on the server
save(CWM, file="data/CWM.Rdata") # on the local system
#load("/data/sPlot2.0/CWM.Rdata") # on the server

## FD Raos Q based on divc in ade4
library(ade4)    # f?r divc (Rao's Q)
FD.fun <- function(trait, abu){
  res <- as.double(NA)
  if (length(trait[!is.na(trait)])>0){
    res <- 0
    #nam <- as.character(nam[!is.na(trait)])
    abu <- as.data.frame(abu[!is.na(trait)])
    #rownames(abu) <- nam
    trait <- as.data.frame(trait[!is.na(trait)])
    #rownames(trait) <- nam
    dis <- dist(trait,method="euclidean")
    if (sum(dis)>1){
      sqrt.dis <- sqrt(dis)
      # taking the square root is required to obtain the same result from the divc function
      # as divc takes the square of distances values, as suggested by
      # Champely & Chessel (2002, Env. Ecol. Stat. 9: 167-177)
      #abu <- as.data.frame(abu)
      res <- unlist(divc(abu, sqrt.dis))
      #Rao's Q: sumi sumj (dist ij prop i prop j)'
    } 
  }
  res
}


library(picante)
# define function for mean pairwise distance
'mpd.fun.dt <- function(abu, dis, nam, abundance.weighted){
  abu <- t(abu)
  colnames(abu) <- nam
  res <- mpd(abu, dis, abundance.weighted=abundance.weighted)
}
# define funtion of mean nearest neighbor distance
mntd.fun.dt <- function(abu, dis, nam, abundance.weighted){
  abu <- t(abu)
  colnames(abu) <- nam
  res <- mntd(abu, dis, abundance.weighted=abundance.weighted)
  res
}
'

mpd.fun <- function(nam, trait, abu,abundance.weighted){
  #res <- numeric(1) # carries result from the function
  res <- as.double(NA)
  nam <- nam[!is.na(trait)]
  abu <- abu[!is.na(trait)]
  trait <- trait[!is.na(trait)]
  if (length(trait)>0){
    #  dis <- as.matrix(dist(t(t1),method="euclidean"))   
    abu <- t(abu)
    colnames(abu) <- nam
    trait <- t(trait)
    colnames(trait) <- nam
    #dis <- as.matrix(vegdist(t(t1),method="euclidean", na.rm=T))
    dis <- as.matrix(dist(t(trait),method="euclidean"))
    res <- mpd(abu, dis, abundance.weighted=abundance.weighted)
  }
  res
}
mntd.fun <- function(nam, trait, abu,abundance.weighted){
  #res <- numeric(1) # carries result from the function
  res <- as.double(NA)
  nam <- nam[!is.na(trait)]
  abu <- abu[!is.na(trait)]
  trait <- trait[!is.na(trait)]
  if (length(trait)>0){
    #  dis <- as.matrix(dist(t(t1),method="euclidean"))   
    abu <- t(abu)
    colnames(abu) <- nam
    trait <- t(trait)
    colnames(trait) <- nam
    #dis <- as.matrix(vegdist(t(t1),method="euclidean", na.rm=T))
    dis <- as.matrix(dist(t(trait),method="euclidean"))
    res <- mntd(abu, dis, abundance.weighted=abundance.weighted)
  }
  res
}
test<- DT[c(1:10000),list(mpd.trait= mpd.fun(species, trait, Relative.cover, abundance.weighted=TRUE)),by=PlotObservationID]
str(test)
length(test$mpd.trait[is.na(test$mpd.trait)])
head(test$mpd.trait)
DT[PlotObservationID==30,]
test$mpd.trait[test$PlotObservationID==30]
length(test$mntd.trait[is.na(test$mntd.trait)])

test<- DT[c(1:10000),list(mntd.trait= mntd.fun(species, trait, Relative.cover, abundance.weighted=TRUE)),by=PlotObservationID]
test$mntd.trait[test$PlotObservationID==30]

FD.Rao <- array(NA,c(dim(CWM)[1],18),dimnames=list(dimnames(CWM)[[1]],
                            colnames(TRY.all.mean.sd.3.by.genus.species)[3:20]))
mpd.abu <- array(NA,c(dim(CWM)[1],18),dimnames=list(dimnames(CWM)[[1]],
                            colnames(TRY.all.mean.sd.3.by.genus.species)[3:20]))
mpd.pa <- array(NA,c(dim(CWM)[1],18),dimnames=list(dimnames(CWM)[[1]],
                            colnames(TRY.all.mean.sd.3.by.genus.species)[3:20]))
mntd.abu <- array(NA,c(dim(CWM)[1],18),dimnames=list(dimnames(CWM)[[1]],
                            colnames(TRY.all.mean.sd.3.by.genus.species)[3:20]))
mntd.pa <- array(NA,c(dim(CWM)[1],18),dimnames=list(dimnames(CWM)[[1]],
                            colnames(TRY.all.mean.sd.3.by.genus.species)[3:20]))

str(FD.Rao) #logi [1:1121145, 1:18] 

for (i in 1:18){
  print(i)  
  DT2$trait <- NA
  DT2$trait <- TRY.all.mean.sd.3.by.genus.species[index7,i+2]
  #FD.Rao[,i] <-  DT2[,list(FD.trait= FD.fun(trait, Relative.cover)),by=PlotObservationID]$FD.trait
  #mpd.abu[,i] <- DT2[,list(mpd.trait= mpd.fun(species, trait, Relative.cover, abundance.weighted=TRUE)),by=PlotObservationID]$mpd.trait
  mpd.pa[,i] <- DT2[,list(mpd.trait= mpd.fun(species, trait, Relative.cover, abundance.weighted=FALSE)),by=PlotObservationID]$mpd.trait
  mntd.abu[,i] <- DT2[,list(mntd.trait= mntd.fun(species, trait, Relative.cover, abundance.weighted=TRUE)),by=PlotObservationID]$mntd.trait
  #mntd.pa[,i] <- DT2[,list(mntd.trait= mntd.fun(species, trait, Relative.cover, abundance.weighted=FALSE)),by=PlotObservationID]$mntd.trait
}
save(FD.Rao, file="/data/sPlot2.0/FD.Rao.Rdata") # on the server
save(mpd.abu, file="/data/sPlot2.0/mpd.abu.Rdata") # on the server
save(mpd.pa, file="/data/sPlot2.0/mpd.pa.Rdata") # on the server
save(mntd.abu, file="/data/sPlot2.0/FD.Rao.Rdata") # on the server
save(mntd.pa, file="/data/sPlot2.0/mntd.pa.Rdata") # on the server
dim(mpd.pa) #1121145      18
length(mpd.pa[,"SLA.mean"][!is.na(mpd.pa[,"SLA.mean"])]) #1100058
1100058/1121145 #0.9811915

library(vegan)
head(dimnames(TRY.all.mean.sd.3.by.genus.species)[[1]])
dimnames(TRY.all.mean.sd.3.by.genus.species)[[2]]
TRY.all.mean.sd.3.by.genus.species2 <- TRY.all.mean.sd.3.by.genus.species[!is.na(TRY.all.mean.sd.3.by.genus.species[,"SLA.mean"]),]
trait.pca1 <- rda(TRY.all.mean.sd.3.by.genus.species2[,c(5,9,10)],scale=T)
## do this for 3 traits
nam <- DT2$species[DT$PlotObservationID==1]
pca1 <- DT2$pca1[DT$PlotObservationID==1]
pca2 <- DT2$pca2[DT$PlotObservationID==1]
pca3 <- DT2$pca3[DT$PlotObservationID==1]
abu <- DT2$Relative.cover[DT$PlotObservationID==1]

mpd.fun.multitrait <- function(nam, pca1, pca2, pca3, abu,abundance.weighted){
  #res <- numeric(1) # carries result from the function
  trait <- as.matrix(cbind(pca1, pca2, pca3))
  res <- as.double(NA)
  nam <- nam[!is.na(rowSums(trait))]
  abu <- abu[!is.na(rowSums(trait))]
  trait <- trait[!is.na(rowSums(trait)),]
  if (length(trait)>3){
    # then there is more than one species (with 3 trait values)
    abu <- t(abu)
    colnames(abu) <- nam
    trait <- t(trait)
    colnames(trait) <- nam
    dis <- as.matrix(dist(t(trait),method="euclidean"))
    res <- mpd(abu, dis, abundance.weighted=abundance.weighted)
  }
  res
}
mntd.fun.multitrait <- function(nam, pca1, pca2, pca3, abu,abundance.weighted){
  #res <- numeric(1) # carries result from the function
  trait <- as.matrix(cbind(pca1, pca2, pca3))
  res <- as.double(NA)
  nam <- nam[!is.na(rowSums(trait))]
  abu <- abu[!is.na(rowSums(trait))]
  trait <- trait[!is.na(rowSums(trait)),]
  if (length(trait)>3){
    abu <- t(abu)
    colnames(abu) <- nam
    trait <- t(trait)
    colnames(trait) <- nam
    dis <- as.matrix(dist(t(trait),method="euclidean"))
    res <- mntd(abu, dis, abundance.weighted=abundance.weighted)
  }
  res
}

identical(dimnames(TRY.all.mean.sd.3.by.genus.species2)[[1]],
          dimnames(summary(trait.pca1)$sites)[[1]])
#T
index8 <- match(DT2$species,TRY.all.mean.sd.3.by.genus.species2$StandSpeciesName)
length(index8) #23555942
length(index8[!is.na(index8)]) #21172949


DT2$pca1 <- summary(trait.pca1)$sites[index8,1]
DT2$pca2 <- summary(trait.pca1)$sites[index8,2]
DT2$pca3 <- summary(trait.pca1)$sites[index8,3]
names(DT2)
# pca scores in 9:11
mpd.abu.LHS <- DT2[,list(mpd.trait= mpd.fun.multitrait(species, pca1, pca2, pca3, Relative.cover, abundance.weighted=TRUE)),by=PlotObservationID]$mpd.trait
mpd.pa.LHS <- DT2[,list(mpd.trait= mpd.fun.multitrait(species, pca1, pca2, pca3, Relative.cover, abundance.weighted=FALSE)),by=PlotObservationID]$mpd.trait
mntd.abu.LHS <- DT2[,list(mntd.trait= mntd.fun.multitrait(species, pca1, pca2, pca3, Relative.cover, abundance.weighted=TRUE)),by=PlotObservationID]$mntd.trait
mntd.pa.LHS <- DT2[,list(mntd.trait= mntd.fun.multitrait(species, pca1, pca2, pca3, Relative.cover, abundance.weighted=FALSE)),by=PlotObservationID]$mntd.trait

mpd.mntd.LHS <- data.frame(mpd.abu.LHS,mpd.pa.LHS,mntd.abu.LHS, mntd.pa.LHS)
str(mpd.mntd.LHS)
#data.frame':	1121145 obs. of  4 variables:
save(mpd.mntd.LHS, file="/data/sPlot2.0/mpd.mntd.LHS.Rdata") # on the server

