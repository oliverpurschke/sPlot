getwd()
###### Traits #####
# TRY 2.0
load("data/TRY.all.mean.sd.2.Rdata")
str(TRY.all.mean.sd.2)
dim(TRY.all.mean.sd.2)
# 40791 obs. of  38 variables
# tests
any(is.na(TRY.all.mean.sd.2$SLA.sd)) #T

# TRY 3.0
TRY3.0 <- read.csv("data/Export_sPlot_2016_09_12.csv")
str(TRY3.0)
levels(TRY3.0$Species)[100] # "Abronia villosa", just an example
which(TRY3.0$Species==levels(TRY3.0$Species)[100]) # an example of one species with several (7) observations
TRY3.0[which(TRY3.0$Species==levels(TRY3.0$Species)[100]),c(1:19)] # trait values differ between observations
any(is.na(TRY3.0[,c(2:19)])) #F
any(TRY3.0[,c(2:19)]==0) #F

for (i in 2:19){
  TRY3.0[,i] <- log(TRY3.0[,i])
}
any(is.na(TRY3.0[,c(2:19)])) #F

TRY.all.n.3 <- aggregate(TRY3.0[,1], by=list(TRY3.0$Species), FUN=length)
head(TRY.all.n.3)
str(TRY.all.n.3) #59319 obs. of  2 variables:
names(TRY.all.n.3)
names(TRY.all.n.3) <- c("StandSpeciesName","n")

TRY.all.mean.3 <- aggregate(TRY3.0[,c(2:19)], by=list(TRY3.0$Species), FUN=mean)
str(TRY.all.mean.3) #59319 obs. of  19 variables:
names(TRY.all.mean.3)
' [1] "Group.1" "X1"      "X4"      "X11"     "X13"     "X14"     "X15"     "X18"     "X26"     "X27"     "X47"    
[12] "X50"     "X56"     "X78"     "X138"    "X163"    "X169"    "X237"    "X282"  '
names(TRY.all.mean.3) <- c("StandSpeciesName","LeafArea.mean", "StemDens.mean", "SLA.mean", "LeafC.perdrymass.mean",
                           "LeafN.mean","LeafP.mean", "PlantHeight.mean", "SeedMass.mean", "Seed.length.mean",
                           "LDMC.mean","LeafNperArea.mean", "LeafNPratio.mean", "Leaf.delta.15N.mean", 
                           "Seed.num.rep.unit.mean", "Leaffreshmass.mean", "Stem.cond.dens.mean", 
                           "Disp.unit.leng.mean", "Wood.vessel.length.mean")
any(is.na(TRY.all.mean.3$SLA.mean)) #F

TRY.all.sd.3 <- aggregate(TRY3.0[,c(2:19)], by=list(TRY3.0$Species), FUN=sd)
str(TRY.all.sd.3) #59319 obs. of  19 variables:
names(TRY.all.sd.3)
names(TRY.all.sd.3) <- c("StandSpeciesName","LeafArea.sd", "StemDens.sd", "SLA.sd", "LeafC.perdrymass.sd",
                           "LeafN.sd","LeafP.sd", "PlantHeight.sd", "SeedMass.sd", "Seed.length.sd",
                           "LDMC.sd","LeafNperArea.sd", "LeafNPratio.sd", "Leaf.delta.15N.sd", 
                           "Seed.num.rep.unit.sd", "Leaffreshmass.sd", "Stem.cond.dens.sd", 
                           "Disp.unit.leng.sd", "Wood.vessel.length.sd")
any(is.na(TRY.all.sd.3$SLA.sd)) #T
TRY.all.mean.sd.3 <- data.frame(TRY.all.n.3,TRY.all.mean.3[,c(2:19)],TRY.all.sd.3[,c(2:19)])
str(TRY.all.mean.sd.3) #59319 obs. of  38 variables

plot(SLA.mean~LDMC.mean, data=TRY.all.mean.sd.3)
#summary(lm(SLA.mean~LDMC.mean, data=TRY.all.mean.sd.2))
#abline(lm(SLA.mean~LDMC.mean, data=TRY.all.mean.sd.2),col="blue", lwd=2)
#sqrt(0.3415)

###### Backbone #####
load("data/backbone.v.2.splot.try3.Rdata")
# alternatively:
#sPlot3 <- read.csv(paste("data/", "backbone.v.2.splot.try3.csv", 
#                         sep = ""), sep=",", quote = "\"'")
str(backbone.splot.try3) 
# data.frame':	122901 obs. of  33 variables:

###### Harmonized plot data ######
DT <- read.csv("data/plots_without_unified_cover_scales_20160120a.csv")
library(data.table)
DT <- data.table(DT)
colnames(DT)
colnames(DT)[9] <- "Cover"
setkey(DT,PlotObservationID)

###### Header data #######
splot.header <- read.csv("data/sPlot_14_04_2015_coord.csv")
str(splot.header)
'data.frame:	1117940 obs. of  3 variables:
$ PlotObservationID: int  1 2 3 4 5 6 7 8 9 10 ...
$ Longitude        : num  -154 -154 -154 -154 -154 ...
$ Latitude         : num  62.4 62.4 62.4 62.4 62.4 ...'

##### Bioclim data ####
splot.bioclim <- read.csv(paste("data/","sPlot.csv", sep = ""), sep = ";", na.strings=c("","NA"))
str(splot.bioclim)
#data.frame':	1117382 obs. of  52 variables:

####################
##### Matching #####
#index6 <- match(TRY.all.mean.sd.2$StandSpeciesName,backbone.splot.try3$names.sPlot.TRY)
index6 <- match(TRY.all.mean.sd.3$StandSpeciesName,backbone.splot.try3$names.sPlot.TRY)
any(is.na(index6)) #T
# not all species in the trait list are also in the backbone
TRY.all.mean.sd.3$StandSpeciesName[is.na(index6)]
# 16 species in TRY are not in the backbone
'[1] Albizia greveanaÊ              AlnusÊgymnothyrsus             Carapanaœba sp                
[4] ChionanthusÊsp                 Clerodendron nudiflorumÊ       Copa’ba sp                    
[7] Copa’fera sp                   Corynocarpus laevigatusÊ       Enterospermum madagascarienseÊ
[10] Hierochlo‘ odorata             Ing‡ sp                        Norrisia maiorÊ               
[13] NothofagusÊbrassospora         QuercusÊsp                     ShoreaÊanthoshorea            
[16] ShoreaÊsp  '
TRY.all.mean.sd.3[TRY.all.mean.sd.3$StandSpeciesName=="Albizia greveanaÊ",] # n=10
TRY.all.mean.sd.3[TRY.all.mean.sd.3$StandSpeciesName=="Albizia greveana",] # 0
backbone.splot.try3$names.sPlot.TRY[backbone.splot.try3$names.sPlot.TRY=="Albizia greveanaÊ"] #0
backbone.splot.try3$names.sPlot.TRY[backbone.splot.try3$names.sPlot.TRY=="Albizia greveana"] #0
backbone.splot.try3$names.sPlot.TRY[backbone.splot.try3$names.sPlot.TRY=="AlnusÊgymnothyrsus"] #0
backbone.splot.try3$names.sPlot.TRY[backbone.splot.try3$names.sPlot.TRY=="Alnus gymnothyrsus"] #0
backbone.splot.try3$names.sPlot.TRY[backbone.splot.try3$names.sPlot.TRY=="Carapanaœba sp"] #0
backbone.splot.try3$names.sPlot.TRY[backbone.splot.try3$names.sPlot.TRY=="Carapanauba sp"] #0
backbone.splot.try3$names.sPlot.TRY[backbone.splot.try3$names.sPlot.TRY=="Carapanaúba sp"] #0
backbone.splot.try3$names.sPlot.TRY[backbone.splot.try3$names.sPlot.TRY=="ChionanthusÊsp"] #0
backbone.splot.try3$names.sPlot.TRY[backbone.splot.try3$names.sPlot.TRY=="Chionanthus sp"] #1
TRY.all.mean.sd.3[TRY.all.mean.sd.3$StandSpeciesName=="ChionanthusÊsp",] # n=10
TRY.all.mean.sd.3[TRY.all.mean.sd.3$StandSpeciesName=="Chionanthus sp",] # n=15
DT$species[DT$species=="Chionanthus sp" & !is.na(DT$species)] #0
backbone.splot.try3$names.sPlot.TRY[backbone.splot.try3$names.sPlot.TRY=="Clerodendron nudiflorumÊ"] #0
backbone.splot.try3$names.sPlot.TRY[backbone.splot.try3$names.sPlot.TRY=="Clerodendron nudiflorum"] #0
backbone.splot.try3$names.sPlot.TRY[backbone.splot.try3$names.sPlot.TRY=="Copa’ba sp"] #0
backbone.splot.try3$names.sPlot.TRY[backbone.splot.try3$names.sPlot.TRY=="Copaiba sp"] #1
TRY.all.mean.sd.3[TRY.all.mean.sd.3$StandSpeciesName=="Copa’ba sp",] # n=0, strange, there should be at least one
TRY.all.mean.sd.3[TRY.all.mean.sd.3$StandSpeciesName=="Copaiba sp",] # n=1
DT$species[DT$species=="Copaiba sp" & !is.na(DT$species)] #0
backbone.splot.try3$names.sPlot.TRY[backbone.splot.try3$names.sPlot.TRY=="Copa’fera sp"] #0
backbone.splot.try3$names.sPlot.TRY[backbone.splot.try3$names.sPlot.TRY=="Copaifera sp"] #1
DT$species[DT$species=="Copaifera sp" & !is.na(DT$species)] #0
backbone.splot.try3$names.sPlot.TRY[backbone.splot.try3$names.sPlot.TRY=="Corynocarpus laevigatusÊ"] #0
backbone.splot.try3$names.sPlot.TRY[backbone.splot.try3$names.sPlot.TRY=="Corynocarpus laevigatus"] #0
backbone.splot.try3$names.sPlot.TRY[backbone.splot.try3$names.sPlot.TRY=="Enterospermum madagascarienseÊ"] #0
backbone.splot.try3$names.sPlot.TRY[backbone.splot.try3$names.sPlot.TRY=="Enterospermum madagascariense"] #1
DT$species[DT$species=="Enterospermum madagascariense" & !is.na(DT$species)] #0
backbone.splot.try3$names.sPlot.TRY[backbone.splot.try3$names.sPlot.TRY=="Hierochlo‘ odorata"] #0
backbone.splot.try3$names.sPlot.TRY[backbone.splot.try3$names.sPlot.TRY=="Hierochloe odorata"] #1
TRY.all.mean.sd.3[TRY.all.mean.sd.3$StandSpeciesName=="Hierochlo‘ odorata",] # n=0, strange, there should be at least one
TRY.all.mean.sd.3[TRY.all.mean.sd.3$StandSpeciesName=="Hierochloe odorata",] # n=63
DT$species[DT$species=="Hierochloe odorata" & !is.na(DT$species)] #545
backbone.splot.try3$names.sPlot.TRY[backbone.splot.try3$names.sPlot.TRY=="Ing‡ sp"] #0
backbone.splot.try3$names.sPlot.TRY[backbone.splot.try3$names.sPlot.TRY=="Inga sp"] #1
DT$species[DT$species=="Inga sp" & !is.na(DT$species)] #0
backbone.splot.try3$names.sPlot.TRY[backbone.splot.try3$names.sPlot.TRY=="Norrisia maiorÊ"] #0
backbone.splot.try3$names.sPlot.TRY[backbone.splot.try3$names.sPlot.TRY=="Norrisia maior"] #1
backbone.splot.try3$names.sPlot.TRY[backbone.splot.try3$names.sPlot.TRY=="NothofagusÊbrassospora"] #0
backbone.splot.try3$names.sPlot.TRY[backbone.splot.try3$names.sPlot.TRY=="Nothofagus brassospora"] #1
backbone.splot.try3$names.sPlot.TRY[backbone.splot.try3$names.sPlot.TRY=="QuercusÊsp"] #0
backbone.splot.try3$names.sPlot.TRY[backbone.splot.try3$names.sPlot.TRY=="Quercus sp"] #1
DT$species[DT$species=="Quercus sp" & !is.na(DT$species)] #0
backbone.splot.try3$names.sPlot.TRY[backbone.splot.try3$names.sPlot.TRY=="ShoreaÊanthoshorea"] #0
backbone.splot.try3$names.sPlot.TRY[backbone.splot.try3$names.sPlot.TRY=="Shorea anthoshorea"] #1
backbone.splot.try3$names.sPlot.TRY[backbone.splot.try3$names.sPlot.TRY=="ShoreaÊsp"] #0
backbone.splot.try3$names.sPlot.TRY[backbone.splot.try3$names.sPlot.TRY=="Shorea sp"] #1
DT$species[DT$species=="Shorea sp" & !is.na(DT$species)] #0

### data cleaning ###
length(TRY.all.mean.sd.2$StandSpeciesName[is.na(TRY.all.mean.sd.2$StandSpeciesName)])
# 1 record is NA
TRY.all.mean.sd.2[which(is.na(TRY.all.mean.sd.2$StandSpeciesName)),]
# record no. 7947 has StandSpeciesName = NA
TRY.all.mean.sd.2 <- TRY.all.mean.sd.2[-which(is.na(TRY.all.mean.sd.2$StandSpeciesName)),]
str(TRY.all.mean.sd.2) #40790 obs. of  38 variables
# 1 record less
'data.frame:	40790 obs. of  38 variables:
$ StandSpeciesName       : Factor w/ 40790 levels "Abarema adenophora",..: 965 3066 3128 3445 4239 7449 7467 7829 8665 12419 ...
$ n                      : int  23 32 15 8 21 32 20 31 7 8 ...
$ SLA.mean               : num  1.42 1.83 1.92 2.7 2.31 ...
$ PlantHeight.mean       : num  1.282 2.92 0.585 0.488 1.1 ...
$ SeedMass.mean          : num  0.0656 2.7757 2.6427 -2.6794 -2.4111 ...
$ LDMC.mean              : num  -0.765 -1.367 -1.009 -1.228 -0.963 ...
$ StemDens.mean          : num  -0.298 -0.524 -0.322 -0.69 -0.592 ...
$ LeafArea.mean          : num  3.39 8.36 5.26 3.28 5.16 ...
$ LeafN.mean             : num  2.33 2.5 2.21 2.95 3.17 ...
$ LeafP.mean             : num  0.118 0.074 -0.127 0.792 0.421 ...
$ LeafNperArea.mean      : num  0.692 0.485 0.381 0.452 0.709 ...
$ Leaffreshmass.mean     : num  -2.325 0.112 -1.916 -3.777 -2.714 ...
$ LeafNPratio.mean       : num  2.23 2.32 2.24 2.14 2.81 ...
$ LeafC.perdrymass.mean  : num  6.14 6.25 6.31 6.1 6.12 ...
$ Leaf.delta.15N.mean    : num  2.68 2.44 2.32 2.78 2.72 ...
$ Stem.cond.dens.mean    : num  5.44 4.82 5.44 6.41 6.69 ...
$ Seed.num.rep.unit.mean : num  4.57 4.34 1.48 9.87 8.48 ...
$ Wood.vessel.length.mean: num  6.32 6.17 6.18 6.13 6.16 ...
$ Seed.length.mean       : num  1.18 1.45 1.78 0.16 1.21 ...
$ Disp.unit.leng.mean    : num  1.173 2.119 2.064 0.039 1.287 ...
$ SLA.sd                 : num  0.1105 0.1754 0.3161 0.0694 0.0596 ...
$ PlantHeight.sd         : num  0.185 0.347 1.296 0.274 0.137 ...
$ SeedMass.sd            : num  0.288 1.016 0.234 0.201 0.317 ...
$ LDMC.sd                : num  0.038 0.0469 0.0449 0.0433 0.0321 ...
$ StemDens.sd            : num  0.0629 0.0582 0.0511 0.0787 0.0635 ...
$ LeafArea.sd            : num  0.876 0.235 1.89 0.135 0.196 ...
$ LeafN.sd               : num  0.0857 0.0902 0.1229 0.0493 0.0443 ...
$ LeafP.sd               : num  0.2173 0.0974 0.142 0.0356 0.0611 ...
$ LeafNperArea.sd        : num  0.0983 0.0916 0.065 0.0425 0.053 ...
$ Leaffreshmass.sd       : num  0.454 0.189 0.776 0.116 0.144 ...
$ LeafNPratio.sd         : num  0.1921 0.0522 0.1184 0.032 0.0477 ...
$ LeafC.perdrymass.sd    : num  0.00741 0.00767 0.00706 0.00634 0.00469 ...
$ Leaf.delta.15N.sd      : num  0.0363 0.0376 0.0382 0.0296 0.0198 ...
$ Stem.cond.dens.sd      : num  0.0996 0.3641 0.2232 0.1064 0.1064 ...
$ Seed.num.rep.unit.sd   : num  0.319 0.51 0.684 0.295 0.408 ...
$ Wood.vessel.length.sd  : num  0.0435 0.0518 0.0673 0.0392 0.0291 ...
$ Seed.length.sd         : num  0.0508 0.0744 0.0992 0.0817 0.0651 ...
$ Disp.unit.leng.sd      : num  0.0721 0.1367 0.1409 0.0423 0.0754 ...
'

index6 <- match(TRY.all.mean.sd.2$StandSpeciesName,backbone.splot.try3$names.sPlot.TRY)
any(is.na(index6)) #T
TRY.all.mean.sd.2$StandSpeciesName[is.na(index6)]
# 3958 species
TRY.all.mean.sd.2$species <- backbone.splot.try3$name.short.correct[index6]
dim(TRY.all.mean.sd.2) # 40790    39


index1 <- match(DT$PlotObservationID, splot.header$PlotObservationID)
any(is.na(index1)) #F
length(index1) #  24241941
length(unique(index1[!is.na(index1)])) #1117940

index2 <- match(splot.header$PlotObservationID,DT$PlotObservationID)
any(is.na(index2)) #F
length(index2) #   1117940
length(unique(index2[!is.na(index2)])) #  1117940
dim(splot.header)
# 1117940      3
identical(as.character(splot.header$PlotObservationID),as.character(DT$PlotObservationID[index2]))
# T

any(is.na(DT$species)) #T
# that should not be the case!
length(DT$species[!is.na(DT$species)]) #24221565
length(DT$species) #24241941
24241941-24221565 # 20376 NA names!!!
# it gives nonsense to match them with traits
index7 <- match(DT$species,TRY.all.mean.sd.2$species)
length(index7) #24241941
length(index7[!is.na(index7)]) #21040927, with previous TRY version that was: 19841429
24241941 - 21040927 # 3201014 entries have no traits
(24241941 - 3201014)/24241941*100 
# which are   86.79555% of plots that have traits,
# previously: 18.15247 %

# reduce DT, and splot.header
#splot.header <- splot.header[!is.na(DT$species[index2]),]
#dim(splot.header) #1117476       3
DT <- DT[!is.na(DT$species),]
index7 <- match(DT$species,TRY.all.mean.sd.2$species)
length(index7) #24221565


### CWM ###
mean(TRY.all.mean.sd.2$SLA.mean) # 2.611657
min(TRY.all.mean.sd.2$SLA.mean) # -1.297031
max(TRY.all.mean.sd.2$SLA.mean) # 5.893029
# example
DT$trait <- NA
DT$trait <- TRY.all.mean.sd.2$SLA.mean[index7]
length(DT$trait[!is.na(DT$trait)]) # 21020551
length(DT$trait[is.na(DT$trait)]) #   3201014
str(DT)

colnames(TRY.all.mean.sd.2)
CWM2 <-  DT[,list(CWM.SLA = weighted.mean(trait,Cover,na.rm = T)),by=PlotObservationID]
# I have checked that we do not need to prefilter only those entries that have a trait value
# this works fine
str(CWM2)
dim(CWM2) #1117898
which(colnames(TRY.all.mean.sd.2)=="SLA.mean") # 3
which(colnames(TRY.all.mean.sd.2)=="Disp.unit.leng.mean") # 20
CWM <- array(NA,c(dim(CWM2)[1],18),dimnames=list(CWM2$PlotObservationID,
                                              colnames(TRY.all.mean.sd.2)[3:20]))
rm(CWM2)
for (i in 1:18){
  DT$trait <- NA
  DT$trait <- TRY.all.mean.sd.2[index7,i+2]
  CWM[,i] <-  DT[,list(CWM.trait= weighted.mean(trait,Relative.cover,na.rm = T)),by=PlotObservationID]$CWM.trait
  
}
str(CWM) #num [1:1117898, 1:18]
CWM[1:20,]
tail(CWM)
write.csv(CWM,file = "CWM.csv", row.names = T)

CWM <- read.csv("C://Daten//iDiv2//splot2//CWM.csv")
str(CWM) #1117898 obs. of  19 variables
dimnames(CWM)[[1]] <- CWM$X
CWM <- CWM[,-1]
tail(CWM)

##### MAP ####
index3 <- match(dimnames(CWM)[[1]],splot.header$PlotObservationID)
length(index3) #1117898
any(is.na(index3)) # F
#length(index3[is.na(index3)]) #422
splot.header2 <- splot.header[index3,]
'CWM1$Longitude <- splot.header$Longitude[index10]
CWM1$Latitude <- splot.header$Latitude[index10]
'
str(CWM) # num [1:1117898, 1:18]
CWM <- CWM[!(is.na(splot.header2$Longitude)|is.na(splot.header2$Latitude)|is.na(CWM[,"SLA.mean"])),]
str(CWM) # num [1:1111307, 1:18]
1111307/1117898*100 # 99.41041% of plots have traits, Long and Lat
any(is.na(CWM)) #F
# match header again
index4 <- match(dimnames(CWM)[[1]],splot.header$PlotObservationID)
length(index4) #1111307
any(is.na(index4)) # F
splot.header2 <- splot.header[index4,]
# now in the same sequence as CWM

library(raster)
CRSlonlat <- CRS("+proj=longlat +datum=WGS84")
coords <- cbind(splot.header2$Longitude, splot.header2$Latitude)
coords <- SpatialPoints(coords, proj4string=CRSlonlat)
map.CWM <- SpatialPointsDataFrame(coords, as.data.frame(CWM), proj4string=CRSlonlat)
str(map.CWM)

library(maptools)
data(wrld_simpl)
plot(wrld_simpl)
points(coordinates(map.CWM), cex=0.1, col="red", pch=16)
# sPlot2withSLA.png
max(coordinates(map.CWM)[,1]) # 179.5901
max(coordinates(map.CWM)[,2]) # 80.14912
min(coordinates(map.CWM)[,1]) # -162.7414
min(coordinates(map.CWM)[,2]) # -64.78


r <- raster(nrows=180, ncols=360, xmn=-180, xmx=180, ymn=-90, ymx=90)
str(r)
raster.map.SLA <- rasterize(map.CWM,r,map.CWM$SLA.mean, run="mean")
str(raster.map.SLA)
plot(wrld_simpl)
plot(raster.map.SLA, add=T)
# SLAgridded1.png

library(rworldmap)
raster.map.SLA.grid <- as(raster.map.SLA, 'SpatialGridDataFrame')
library(RColorBrewer)
# Set a color ramp
rf <- brewer.pal(8, 'Spectral')
mapParams <- mapGriddedData(raster.map.SLA.grid,addLegend=F, 
                            numCats = 8, colourPalette=rf)
str(mapParams)
mapParams$cutVector
# addMapLegend(plottedData=raster.map.SLA.grid,legendShrink=0.5)
#do.call(addMapLegend, c( mapParams, legendLabels="all", legendWidth=0.5,
#                legendIntervals="data", legendMar = 2 ) )
do.call(addMapLegend, c( mapParams, legendLabels="all", legendWidth=0.3,
                         legendIntervals="page", legendMar = 4, legendShrink=1.7,horizontal=F))
#round(as.numeric(exp(mapParams$cutVector)),1)
mtext(round(as.numeric(exp(mapParams$cutVector)),1),side=4,line=-2, 
      at=seq(-150,170, by=39), las=2)
#mtext("ln(SLA)",side=1,line=0, at=230, las=1)
#mtext("ln(SLA)",side=4,line=-2,at=-170, las=2)
# SLAgridded2.png

############################################################
# Regression with Bioclim
############################################################
'BIO1 = Annual Mean Temperature
BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
BIO3 = Isothermality (BIO2/BIO7) (* 100)
BIO4 = Temperature Seasonality (standard deviation *100)
BIO5 = Max Temperature of Warmest Month
BIO6 = Min Temperature of Coldest Month
BIO7 = Temperature Annual Range (BIO5-BIO6)
BIO8 = Mean Temperature of Wettest Quarter
BIO9 = Mean Temperature of Driest Quarter
BIO10 = Mean Temperature of Warmest Quarter
BIO11 = Mean Temperature of Coldest Quarter
BIO12 = Annual Precipitation
BIO13 = Precipitation of Wettest Month
BIO14 = Precipitation of Driest Month
BIO15 = Precipitation Seasonality (Coefficient of Variation)
BIO16 = Precipitation of Wettest Quarter
BIO17 = Precipitation of Driest Quarter
BIO18 = Precipitation of Warmest Quarter
BIO19 = Precipitation of Coldest Quarter'

index11 <- match(splot.header2$PlotObservationID,splot.bioclim$PLOT_ID)
length(index11) # 1111307
length(index11[!is.na(index11)]) # 1111307

#index12 <- match(CWM2$PlotObservationID, splot.header$PlotObservationID)
#any(is.na(index12)) #F
str(splot.bioclim$BIO_01)
plot(CWM[,"SLA.mean"]~splot.bioclim$BIO_01[index11],
     cex=0.05, xlab="BIO_1", ylab="CWM ln(SLA)",cex.lab=1.5, cex.axis=1.5)
model1 <- lm(CWM[,"SLA.mean"]~splot.bioclim$BIO_01[index11])
model2 <- lm(CWM[,"SLA.mean"]~splot.bioclim$BIO_01[index11]+I(splot.bioclim$BIO_01[index11]^2))
summary(model1)
'Coefficients:
                                Estimate Std. Error t value Pr(>|t|)    
(Intercept)                    2.945e+00  9.167e-04  3212.9   <2e-16 ***
splot.bioclim$BIO_01[index11] -1.204e-03  9.067e-06  -132.8   <2e-16 ***
Multiple R-squared:  0.01562,	Adjusted R-squared:  0.01562 
F-statistic: 1.764e+04 on 1 and 1111305 DF,  p-value: < 2.2e-16
'
summary(model2)
'                                     Estimate Std. Error t value Pr(>|t|)    
(Intercept)                         2.806e+00  1.113e-03  2520.3   <2e-16 ***
splot.bioclim$BIO_01[index11]       2.188e-03  1.857e-05   117.8   <2e-16 ***
I(splot.bioclim$BIO_01[index11]^2) -1.657e-05  7.985e-08  -207.6   <2e-16 ***
Multiple R-squared:  0.05241
'
abline(model1, lwd=3, col="blue")
model2$coefficients
x.new <- seq(-100,300,1)
y.new <- model2$coefficients[1]+model2$coefficients[2]*x.new+model2$coefficients[3]*x.new^2
lines(x.new,y.new, lwd=3, col="blue")
# SLA_BIO1.png

# devide in quantiles
quantile(splot.bioclim$BIO_01[index11],probs = seq(0, 1, 1/3))
'       0% 33.33333% 66.66667%      100% 
     -227        79        99       303 '
# -> most points (i.e. one third) are in the temperate zone between 7.9 and 9.9°C
x1 <- splot.bioclim$BIO_01[index11][splot.bioclim$BIO_01[index11]<=79]
model1a <- lm(CWM[,"SLA.mean"][splot.bioclim$BIO_01[index11]<=79]~splot.bioclim$BIO_01[index11][splot.bioclim$BIO_01[index11]<=79])
summary(model1a)
'Coefficients:
                                                                    Estimate Std. Error t value Pr(>|t|)    
(Intercept)                                                        2.697e+00  1.244e-03  2167.2   <2e-16 ***
splot.bioclim$BIO_01[index11][splot.bioclim$BIO_01[index11] <= 79] 2.466e-03  2.117e-05   116.5   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.4027 on 371873 degrees of freedom
Multiple R-squared:  0.03519,	Adjusted R-squared:  0.03519 
F-statistic: 1.356e+04 on 1 and 371873 DF,  p-value: < 2.2e-16'
model1b <- lm(CWM[,"SLA.mean"][splot.bioclim$BIO_01[index11]<=99]~splot.bioclim$BIO_01[index11][splot.bioclim$BIO_01[index11]<=99])
summary(model1b)
'                                                                    Estimate Std. Error t value Pr(>|t|)    
(Intercept)                                                        2.692e+00  1.161e-03  2319.8   <2e-16 ***
splot.bioclim$BIO_01[index11][splot.bioclim$BIO_01[index11] <= 99] 2.777e-03  1.527e-05   181.9   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3954 on 750010 degrees of freedom
Multiple R-squared:  0.04224,	Adjusted R-squared:  0.04224 
F-statistic: 3.308e+04 on 1 and 750010 DF,  p-value: < 2.2e-16'
model1c <- lm(CWM[,"SLA.mean"][splot.bioclim$BIO_01[index11]>99]~splot.bioclim$BIO_01[index11][splot.bioclim$BIO_01[index11]>99])
summary(model1c)
'                                                                    Estimate Std. Error t value Pr(>|t|)    
(Intercept)                                                        3.236e+00  2.587e-03  1251.1   <2e-16 ***
splot.bioclim$BIO_01[index11][splot.bioclim$BIO_01[index11] > 99] -3.774e-03  1.855e-05  -203.4   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.443 on 361293 degrees of freedom
Multiple R-squared:  0.1028,	Adjusted R-squared:  0.1028 
F-statistic: 4.138e+04 on 1 and 361293 DF,  p-value: < 2.2e-16
'

plot(CWM[,"SLA.mean"]~splot.bioclim$BIO_01[index11],
     cex=0.05, xlab="BIO_1", ylab="CWM ln(SLA)",cex.lab=1.5, cex.axis=1.5)
x.new <- seq(-100,79,1)
y.new <- model1a$coefficients[1]+model1a$coefficients[2]*x.new
lines(x.new,y.new, lwd=3, col="blue")
x.new <- seq(79,99,1)
y.new <- model1b$coefficients[1]+model1b$coefficients[2]*x.new
lines(x.new,y.new, lwd=3, col="green")
x.new <- seq(99,300,1)
y.new <- model1c$coefficients[1]+model1c$coefficients[2]*x.new
lines(x.new,y.new, lwd=3, col="red")

### calculate a GAM
library(gam)
x <- splot.bioclim$BIO_01[index11]
model3 <- gam(CWM[,"SLA.mean"]~lo(x, span=0.5))
summary(model3)
'    Null Deviance: 207269.5 on 1111306 degrees of freedom
Residual Deviance: 187452.7 on 1111302 degrees of freedom
AIC: 1175898
Anova for Parametric Effects
                       Df Sum Sq Mean Sq F value    Pr(>F)    
lo(x, span = 0.5)       1   3238  3238.0   19197 < 2.2e-16 ***
Residuals         1111302 187453     0.2
Anova for Nonparametric Effects
                  Npar Df Npar F     Pr(F)    
(Intercept)                                   
lo(x, span = 0.5)     2.7  37028 < 2.2e-16 ***
'

x.new <- seq(-100,300,1)
y.new <- predict(model3, data.frame(x=x.new), se = F)
# se=T does not work
plot(CWM[,"SLA.mean"]~splot.bioclim$BIO_01[index11],
     cex=0.05, xlab="BIO_1", ylab="CWM ln(SLA)",cex.lab=1.5, cex.axis=1.5)
lines(x.new,y.new, lwd=3, col="blue")

### N to P ratio
model4 <- lm(CWM[,"LeafNPratio.mean"]~splot.bioclim$BIO_01[index11])
summary(model4)
'                               Estimate Std. Error t value Pr(>|t|)    
(Intercept)                   2.290e+00  4.334e-04  5284.0   <2e-16 ***
splot.bioclim$BIO_01[index11] 1.531e-03  4.287e-06   357.2   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.2026 on 1111305 degrees of freedom
Multiple R-squared:  0.103,	Adjusted R-squared:  0.103  
'
plot(CWM[,"LeafNPratio.mean"]~splot.bioclim$BIO_01[index11],
     cex=0.05, xlab="BIO_01", ylab="CWM ln(Leaf N to P ratio)",cex.lab=1.5, cex.axis=1.5)
abline(model4, lwd=3, col="blue")

### turn the regression around
# does not make sense
model5 <- lm(splot.bioclim$BIO_01[index11]~CWM[,"SLA.mean"])
summary(model5)
'                   Estimate Std. Error t value Pr(>|t|)    
(Intercept)       127.41676    0.28026   454.6   <2e-16 ***
CWM[, "SLA.mean"] -12.97407    0.09769  -132.8   <2e-16 ***
Multiple R-squared:  0.01562,	Adjusted R-squared:  0.01562 '

model6 <- lm(splot.bioclim$BIO_01[index11]~CWM[,"LeafNPratio.mean"])
summary(model6)
'Coefficients:
                          Estimate Std. Error t value Pr(>|t|)    
(Intercept)               -72.7479     0.4591  -158.5   <2e-16 ***
CWM[, "LeafNPratio.mean"]  67.2535     0.1883   357.2   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 42.46 on 1111305 degrees of freedom
Multiple R-squared:  0.103,	Adjusted R-squared:  0.103 '

model7 <- lm(splot.bioclim$BIO_01[index11]~CWM[,"SLA.mean"]+CWM[,"LeafNPratio.mean"])
summary(model7)
'                           Estimate Std. Error t value Pr(>|t|)    
(Intercept)               -41.59378    0.55839  -74.49   <2e-16 ***
CWM[, "SLA.mean"]          -9.08978    0.09355  -97.17   <2e-16 ***
CWM[, "LeafNPratio.mean"]  65.04103    0.18885  344.40   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 42.28 on 1111304 degrees of freedom
Multiple R-squared:  0.1106,	Adjusted R-squared:  0.1106 '
x <- splot.bioclim$BIO_01[index11]
model8 <- lm(x~CWM[,"LeafNPratio.mean"]*CWM[,"SLA.mean"])
summary(model8)
'Coefficients:
                                             Estimate Std. Error t value Pr(>|t|)    
(Intercept)                                 -432.2500     2.2645  -190.9   <2e-16 ***
CWM[, "LeafNPratio.mean"]                    227.1681     0.9304   244.2   <2e-16 ***
CWM[, "SLA.mean"]                            135.8572     0.8202   165.6   <2e-16 ***
CWM[, "LeafNPratio.mean"]:CWM[, "SLA.mean"]  -60.2280     0.3386  -177.9   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 41.69 on 1111303 degrees of freedom
Multiple R-squared:  0.1352,	Adjusted R-squared:  0.1352 '

library(scatterplot3d) # für 3D-Plots
s3d1 <- scatterplot3d(CWM[,"SLA.mean"],CWM[,"LeafNPratio.mean"], x, 
                      xlab="CWM ln(Leaf N to P ratio)", ylab="CWM ln(SLA)", zlab="BIO_1",
                              pch=1,cex.symbols=0.05,highlight.3d=T,type="p")
#fit <- lm(AnzF ~ Prop.n+Prop.ba,data=data6[data6$Sp=="Qupe",])
s3d1$plane3d(model8)
# does not work
### producing the plane with points
min(CWM[,"SLA.mean"])
max(CWM[,"SLA.mean"])
x2 <- sort(rep(seq(0.1, 5, by=0.1),35))
min(CWM[,"LeafNPratio.mean"])
max(CWM[,"LeafNPratio.mean"])
y2 <- rep(seq(1, 4.4, by=0.1),50)
func <- function(x,y) { -432.2500 + 227.1681 * x + 135.8572*y -60.2280*x*y}
z2 <- func(x2,y2)
s3d1 <- scatterplot3d(CWM[,"SLA.mean"],CWM[,"LeafNPratio.mean"], x, 
                      xlab="CWM ln(Leaf N to P ratio)", ylab="CWM ln(SLA)", zlab="BIO_1",
                      pch=1,cex.symbols=0.05,highlight.3d=T,type="p",angle=70,scale.y=0.5)
s3d1$points3d(x2,y2,z2,col="blue", type="p", pch=20, cex=0.5)
# 
x <- splot.bioclim$BIO_01[index11]
model7 <- lm(CWM[,"SLA.mean"]~x*CWM[,"LeafNPratio.mean"])
summary(model7)
'                              Estimate Std. Error t value Pr(>|t|)    
(Intercept)                  1.948e+00  9.377e-03   207.8   <2e-16 ***
x                            1.278e-02  7.985e-05   160.0   <2e-16 ***
CWM[, "LeafNPratio.mean"]    3.916e-01  3.856e-03   101.6   <2e-16 ***
x:CWM[, "LeafNPratio.mean"] -5.472e-03  3.166e-05  -172.8   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.4213 on 1111303 degrees of freedom
Multiple R-squared:  0.04842,	Adjusted R-squared:  0.04842 '
min(x)
max(x)
x2 <- sort(rep(seq(-100, 290, by=10),35))
min(CWM[,"LeafNPratio.mean"])
max(CWM[,"LeafNPratio.mean"])
y2 <- rep(seq(1, 4.4, by=0.1),40)
func <- function(x,y) {  1.948e+00  +  1.278e-02 * x + 3.916e-01*y -5.472e-03*x*y}
z2 <- func(x2,y2)
s3d1 <- scatterplot3d(x,CWM[,"LeafNPratio.mean"], CWM[,"SLA.mean"], 
                      xlab="BIO_1", ylab="CWM ln(Leaf N to P ratio)", zlab="CWM ln(SLA)", 
                      pch=1,cex.symbols=0.05,highlight.3d=T,type="p",angle=50,scale.y=1)
s3d1$points3d(x2,y2,z2,col="blue", type="p", pch=20, cex=0.5)
# final plot
# some BIO_01 and LeafNPratio combinations do not exist
?cut
x.cut <- cut(x, breaks=seq(-110, 290, by=10))
y.cut <- cut(CWM[,"LeafNPratio.mean"],breaks=seq(0.9, 4.4, by=0.1))
z.cut <- table(x.cut,y.cut)
head(z.cut)
dim(z.cut) #40 35
z2.cut <- table(x2,y2)
head(z2.cut)
dim(z2.cut) #40 35

#library(reshape2)
z.cut <- melt(z.cut)
str(z.cut) # 1400 obs. of  3 variables:
head(z.cut)
z2.cut <- melt(z2.cut)
str(z2.cut) #1400 obs. of  3 variables:
head(z2.cut)
z2.cut$value <- 0
z2.cut$value <- z.cut$value
z2.cut$xy <- paste(z2.cut$x2,z2.cut$y2,sep="_")
#z3.cut <- acast(z2.cut, x2~y2)
#head(z3.cut)
xy <- paste(x2,y2,sep="_")
index20 <- match(xy,z2.cut$xy)
values.present <- vector(mode="logical", length=length(x2))
values.present <- z2.cut$value[index20] > 0
table(values.present)
'z.cut$value[z.cut$x.cut==x.cut[1] & z.cut$y.cut==y.cut[1]]
values.present <- function(z.cut,x,y){
  z.cut$value[z.cut$x.cut==x & z.cut$y.cut==y]
}
x1 <- data.frame(x,x.cut, y=CWM[,"LeafNPratio.mean"],y.cut)
head(x1)
z1 <- apply(x1,1,FUN=values.present(z.cut, x1$x.cut, x1$y.cut))
values.present(z.cut, x1$x.cut[1], x1$y.cut[1])
'

s3d1 <- scatterplot3d(x,CWM[,"LeafNPratio.mean"], CWM[,"SLA.mean"], 
                      xlab="BIO_1", ylab="CWM ln(Leaf N to P ratio)", zlab="CWM ln(SLA)", 
                      pch=1,cex.symbols=0.05,highlight.3d=T,type="p",angle=-10,scale.y=0.8)
s3d1$points3d(x2[values.present],y2[values.present],z2[values.present],col="blue", type="p", pch=20, cex=0.5)


#### Collect all CWM regressions ####
names(splot.bioclim) # from 4 (BIO_01 to 49 T_MAR
CWM.bioclim <- array(NA,c(46,18,5),dimnames=list(names(splot.bioclim)[4:49],dimnames(CWM)[[2]],
                      c("p_lin","r2_lin","p_qua1", "p_qua2","r2_qua")))
for (i in 1:46){
  for (j in 1:18){
    model1 <- lm(CWM[,j]~splot.bioclim[index11,i+3])
    CWM.bioclim[i,j,1] <- summary(model1)$coefficients[2,4]
    CWM.bioclim[i,j,2] <- summary(model1)$r.squared
    model2 <- lm(CWM[,j]~splot.bioclim[index11,i+3]+I(splot.bioclim[index11,i+3]^2))
    CWM.bioclim[i,j,3] <- summary(model2)$coefficients[2,4]
    CWM.bioclim[i,j,4] <- summary(model2)$coefficients[3,4]
    CWM.bioclim[i,j,5] <- summary(model2)$r.squared
  }
}
str(CWM.bioclim) #num [1:46, 1:18, 1:5]
write.csv(data.frame(names(splot.bioclim)[4:49],CWM.bioclim[,,2]),file = "CWM_bioclim_r2_lin.csv", row.names = FALSE)
write.csv(data.frame(names(splot.bioclim)[4:49],CWM.bioclim[,,5]),file = "CWM_bioclim_r2_qua.csv", row.names = FALSE)


which(CWM.bioclim[,1,2]==max(CWM.bioclim[,1,2])) # BIO_02 
which(CWM.bioclim[,1,5]==max(CWM.bioclim[,1,5])) # BIO_02 
which(CWM.bioclim[,,2]==max(CWM.bioclim[,,2]),arr.ind = TRUE) 
#       row col
#BIO_02   2   9
CWM.bioclim[2,9,2] #0.1297723
which(CWM.bioclim[,,5]==max(CWM.bioclim[,,5]),arr.ind = TRUE) 
#       row col
#BIO_02   2   9
CWM.bioclim[2,9,5] # 0.1563736
names(splot.bioclim)[2+3] #"BIO_02"
dimnames(CWM)[[2]][9] #"LeafNperArea.mean"
plot(CWM[,"LeafNperArea.mean"]~splot.bioclim$BIO_02[index11],
     cex=0.05, xlab="BIO_02", ylab="CWM ln(Leaf N per Area)",cex.lab=1.5, cex.axis=1.5)
model1 <- lm(CWM[,9]~splot.bioclim[index11,2+3])
abline(model1, lwd=3, col="blue")
# MeanNperArea_BIO_02.png
png(filename="MeanNperArea_BIO_02.png", width=800, height=800, units = "px", bg="white")
  plot(CWM[,"LeafNperArea.mean"]~splot.bioclim$BIO_02[index11],
       cex=0.05, xlab="BIO_02", ylab="CWM ln(Leaf N per Area)",cex.lab=1.5, cex.axis=1.5)
  #model1 <- lm(CWM[,9]~splot.bioclim[index11,2+3])
  abline(model1, lwd=3, col="blue")
dev.off() # creates a file
png(filename="SLA_BIO_02.png", width=800, height=800, units = "px", bg="white")
  plot(CWM[,"SLA.mean"]~splot.bioclim$BIO_02[index11],
    cex=0.05, xlab="BIO_02", ylab="CWM ln(SLA)",cex.lab=1.5, cex.axis=1.5)
  model1 <- lm(CWM[,1]~splot.bioclim[index11,2+3])
abline(model1, lwd=3, col="blue")
dev.off() # creates a file

#### PCA of all CWM and all Bioclim variables ####
library(vegan)
pca1 <- rda(CWM,scale=T)
plot(pca1)
str(summary(pca1))
#text(summary(pca1)$species[,c(1,2)],dimnames(summary(pca1)$species)[[1]])
traitnames <- dimnames(summary(pca1)$species)[[1]]
traitnames <- substr(traitnames,1,nchar(traitnames)-5)
traitcoord <- summary(pca1)$species[,c(1,2)]
which(traitnames=="SeedMass") #3
traitcoord[3,2] <- traitcoord[3,2]-0.5
ordiplot(pca1, type="n", ylim=c(-10,10))
text(traitcoord,traitnames, col="red", cex=1)
points(plotcoord*30, cex=0.05)
#env1 <- envfit(pca1,splot.bioclim[index11,c(4:49)],permutations = 99)
#with permutations = 999:
#Error: cannot allocate vector of size 4.1 Gb
plot(env1)
png(filename="PCA1.png", width=1000, height=1000, units = "px", bg="white")
  ordiplot(pca1, type="n")
  text(traitcoord,traitnames)
  plot(env1)
dev.off() # creates a file
'rm(env1)
rm(backbone.splot.try3)
rm(model1)
rm(model2)
rm(pca1)
rm(splot.header)
rm(splot.header2)
rm(traitdist)
rm(coords)
rm(TRY.all.mean.sd.2)
rm(index7)
rm(raster.map.SLA)
rm(trait.vec)
rm(FD)
rm(splot.bioclim)
'
gc()

### produce a PCA with kernel density
# From Díaz et al. 2016, Nature
library(ks)
plotcoord <- summary(pca1)$sites[,c(1,2)]
str(plotcoord) #num [1:1111307, 1:2]
H <- Hpi(x=plotcoord)      # optimal bandwidth estimation
# does not work
est<- kde(x=plotcoord, H=H, compute.cont=TRUE)     # kernel density estimation

library(raster)
library(fBasics)
# Color palette
#plotcoord <- plotcoord*30
min(plotcoord[,1]) #-0.2639705
max(plotcoord[,1]) # 0.2415964
min(plotcoord[,2]) #-0.2595053
max(plotcoord[,2]) # 0.3560549
x.cut <- cut(plotcoord[,1], breaks=seq(min(plotcoord[,1]), max(plotcoord[,1]), by=0.01))
y.cut <- cut(plotcoord[,2], breaks=seq(min(plotcoord[,2]), max(plotcoord[,2]), by=0.01))
z.cut <- table(x.cut,y.cut)
head(z.cut)
dim(z.cut) #50 61
z.cut <- melt(z.cut)
str(z.cut) # 3050 obs. of  3 variables:
head(z.cut)
z.cut$xy <- paste(z.cut$x.cut,z.cut$y.cut,sep="_")
str(x.cut)
length(x.cut) # 1111307
xy <- paste(x.cut,y.cut,sep="_")
index30 <- match(xy,z.cut$xy)
length(index30)
any(is.na(index30))
head(log10(z.cut$value[index30]))
values.present <- vector(mode="numeric", length=length(xy))
min(log10(z.cut$value[index30]), na.rm=T) #0
max(log10(z.cut$value[index30]), na.rm=T) # 3.9878
values.cut <- cut(log10(z.cut$value[index30]), breaks=seq(min(log10(z.cut$value[index30]), na.rm=T),
                        4, by=0.4))
#values.present <- z.cut$value[index30]
ordiplot(pca1, type="n")
#points(plotcoord*30, cex=0.05, col=rainbowPalette(n=10,  
#        s = 1, v = 1, start = 3/6, end = 5/6, alpha = 1)[values.cut])
points(plotcoord*30, cex=0.05, col=rev(greyPalette(n=10,  start = 0, end = 0.8, 
                                    gamma = 2.2, alpha = NULL))[values.cut])
text(traitcoord,traitnames, col="red", cex=1)


ext.factor <- 30
r.pca <- raster(nrows=300, ncols=300, xmn=min(plotcoord[,1])*ext.factor, xmx=max(plotcoord[,2])*ext.factor, 
                ymn=min(plotcoord[,1])*ext.factor, ymx=max(plotcoord[,2]*ext.factor))
# attention, xmn and ymn as well as xmx and ymx have to be the same, otherwise the raster is shifted
r.pca.raster <- rasterize(plotcoord*ext.factor, r.pca, fun="count")
str(r.pca.raster)
plot(log10(r.pca.raster), asp=0, col=seqPalette(n=10, name=c("Blues")), 
     main="", cex.main=0.7, xlab="",  ylab="", add=T)
plot(log10(r.pca.raster), asp=0, col=rev(greyPalette(n=10,  start = 0, end = 0.8, 
    gamma = 2.2, alpha = NULL)), 
     main="", cex.main=0.7, xlab="",  ylab="", add=T)
plot(log10(r.pca.raster), asp=0, col=rainbowPalette(n=10,   s = 1, v = 1, start = 3/6, end = 5/6, alpha = 1), 
     main="", cex.main=0.7, xlab="",  ylab="", add=T, legend=F)

'## graph for the PCA ##
ordiplot(pca1, type="n", xlim=c(-10,13), ylim=c(-10,10))
rasterImage(as.raster(r.pca.raster), -10, -10, 20, 10, interpolate = F,
            col=rainbowPalette(n=10,   s = 1, v = 1, start = 3/6, end = 5/6, alpha = 1))
plot(log10(r.pca.raster), asp=0, col=rainbowPalette(n=10,   s = 1, v = 1, start = 3/6, end = 5/6, alpha = 1), 
     main="", cex.main=0.7, xlab="",  ylab="", add=T, legend=F)
text(traitcoord,traitnames, col="red", cex=1)
'


rda1 <- rda(CWM~.,splot.bioclim[index11,c(4:49)],scale=T)
plot(rda1)
summary(rda1)
rda1
'              Inertia Proportion Rank
Total         18.0000     1.0000     
Constrained    2.4811     0.1378   15
Unconstrained 15.5189     0.8622   15
Inertia is correlations 
Eigenvalues for constrained axes:
  RDA1   RDA2   RDA3   RDA4   RDA5   RDA6   RDA7   RDA8   RDA9  RDA10  RDA11  RDA12  RDA13  RDA14  RDA15 
1.2230 0.6020 0.3275 0.1972 0.0653 0.0226 0.0156 0.0107 0.0059 0.0050 0.0031 0.0012 0.0010 0.0007 0.0003 

Eigenvalues for unconstrained axes:
PC1   PC2   PC3   PC4   PC5   PC6   PC7   PC8   PC9  PC10  PC11  PC12  PC13  PC14  PC15 
4.594 3.453 1.486 1.426 1.095 0.798 0.648 0.603 0.483 0.343 0.261 0.122 0.084 0.074 0.047 
'
#anova(rda1, perm=99)
#anova(rda1, by="terms", permutations = how(nperm=9))
library(parallel) 
detectCores() #32
# Set up the cluster
#clusterType <- if(length(find.package("snow", quiet = TRUE))) "SOCK" else "PSOCK"
#clust <- try(makeCluster(getOption("cl.cores", 2), type = clusterType))
cl <- makeCluster(10)
#registerDoParallel(cl) 
options(na.action = "na.pass")
#clusterExport(cl, "results") 

#print(system.time(anova(rda1, by="terms", perm=99, parallel = getOption("mc.cores"))))
print(system.time(out1 <- anova(rda1, by="margin", permutations =99, parallel = cl)))
'user    system   elapsed 
5304.802  1311.088 14522.410
'
out1

traitnames <- dimnames(summary(rda1)$species)[[1]]
traitnames <- substr(traitnames,1,nchar(traitnames)-5)
traitcoord <- summary(rda1)$species[,c(1,2)]
bioclimcoord <- summary(rda1)$biplot[,c(1,2)]
which(traitnames=="LeafNperArea") #9
traitcoord[9,1] <- traitcoord[9,1]+0.5
which(traitnames=="StemDens") #5
traitcoord[5,2] <- traitcoord[5,2]+0.3
which(traitnames=="Disp.unit.leng") #18
traitcoord[18,2] <- traitcoord[18,2]-0.3
ordiplot(rda1, type="n", xlim=c(-7,7))
arrows(rep(0,45),rep(0,45),bioclimcoord[,1]*8,bioclimcoord[,2]*8, col="cadetblue4", length=0.1)
text(bioclimcoord*8,dimnames(bioclimcoord)[[1]], col="cadetblue4", cex=0.8)
text(traitcoord,traitnames, col="red")
# copied as first RDA graph into ppt

#rda1 <- rda(CWM ~ ., splot.bioclim[index11,c(4:49)],scale=T)

### Stepwise RDA ###
rda2 <- rda(CWM ~ 1,splot.bioclim[index11,c(4:49)],scale=T)
#rda3 <- step(rda2, scope = formula(rda1), test = "perm")
rda3 <- ordiR2step(rda2, scope=formula(rda1), direction = "both", Pin = 0.05, R2scope = F, permutations = how(nperm = 99), trace = TRUE)
#CWM ~ GDD5 + BIO_02 + BIO_12 + T_MAY + P_MAY + T_AUG ....
#rm(rda1)

# now rebuild the RDA sequentially
pca1 <- rda(CWM,scale=T)
pca1
5.527+ 3.906+ 1.953+ 1.507+ 1.375+ 0.886+ 0.726+ 0.623+ 0.499 +0.360+ 0.295+ 0.131+ 0.087+ 0.077+ 0.048 
(5.527+ 3.906)/18 # 52.40556%

traitnames <- dimnames(summary(pca1)$species)[[1]]
traitnames <- substr(traitnames,1,nchar(traitnames)-5)
traitcoord <- summary(pca1)$species[,c(1,2)]
which(traitnames=="SeedMass") #3
traitcoord[3,2] <- traitcoord[3,2]-0.5
ordiplot(pca1, type="n")
text(traitcoord,traitnames, col="red")
# copied as second RDA graph into ppt


rda4 <- rda(CWM ~ GDD5,splot.bioclim[index11,c(4:49)],scale=T)
rda4
traitnames <- dimnames(summary(rda4)$species)[[1]]
traitnames <- substr(traitnames,1,nchar(traitnames)-5)
traitcoord <- summary(rda4)$species[,c(1,2)]
plotcoord <- summary(rda4)$sites[,c(1,2)]
str(plotcoord) #num [1:1111307, 1:2]
bioclimcoord <- summary(rda4)$biplot[,c(1,2)]

min(plotcoord[,1]) #-0.5340595
max(plotcoord[,1]) # 0.7760822
min(plotcoord[,2]) #-0.2688765
max(plotcoord[,2]) # 0.2326464
x.cut <- cut(plotcoord[,1], breaks=seq(min(plotcoord[,1]), max(plotcoord[,1]), by=0.01))
y.cut <- cut(plotcoord[,2], breaks=seq(min(plotcoord[,2]), max(plotcoord[,2]), by=0.01))
z.cut <- table(x.cut,y.cut)
head(z.cut)
dim(z.cut) #131 50
z.cut <- melt(z.cut)
str(z.cut) # 6550 obs. of  3 variables:
head(z.cut)
z.cut$xy <- paste(z.cut$x.cut,z.cut$y.cut,sep="_")
str(x.cut)
length(x.cut) # 1111307
xy <- paste(x.cut,y.cut,sep="_")
index30 <- match(xy,z.cut$xy)
length(index30)
any(is.na(index30))
head(log10(z.cut$value[index30]))
values.present <- vector(mode="numeric", length=length(xy))
min(log10(z.cut$value[index30]), na.rm=T) #0
max(log10(z.cut$value[index30]), na.rm=T) # 3.956168
values.cut <- cut(log10(z.cut$value[index30]), breaks=seq(min(log10(z.cut$value[index30]), na.rm=T),
                                                         4, by=0.4))
     

ordiplot(rda4, type="n")
points(plotcoord*30, cex=0.05, col=rev(greyPalette(n=10,  start = 0, end = 0.8, 
                                                   gamma = 2.2, alpha = NULL))[values.cut])

arrows(rep(0,1),rep(0,1),bioclimcoord[1]*8,bioclimcoord[2]*8, col="blue", length=0.1)
text(bioclimcoord[1]*8+0.8,bioclimcoord[2]*8-0.2,"GDD5", col="blue", cex=1)
text(traitcoord,traitnames, col="red")
# copied as third RDA graph into ppt

rda5 <- rda(CWM ~ GDD5+BIO_02,splot.bioclim[index11,c(4:49)],scale=T)
rda5
traitnames <- dimnames(summary(rda5)$species)[[1]]
traitnames <- substr(traitnames,1,nchar(traitnames)-5)
traitcoord <- summary(rda5)$species[,c(1,2)]
bioclimcoord <- summary(rda5)$biplot[,c(1,2)]
plotcoord <- summary(rda5)$sites[,c(1,2)]
str(plotcoord) #num [1:1111307, 1:2]
x.cut <- cut(plotcoord[,1], breaks=seq(min(plotcoord[,1]), max(plotcoord[,1]), by=0.01))
y.cut <- cut(plotcoord[,2], breaks=seq(min(plotcoord[,2]), max(plotcoord[,2]), by=0.01))
z.cut <- table(x.cut,y.cut)
head(z.cut)
dim(z.cut) #131 50
z.cut <- melt(z.cut)
str(z.cut) # 6550 obs. of  3 variables:
head(z.cut)
z.cut$xy <- paste(z.cut$x.cut,z.cut$y.cut,sep="_")
str(x.cut)
length(x.cut) # 1111307
xy <- paste(x.cut,y.cut,sep="_")
index30 <- match(xy,z.cut$xy)
length(index30)
any(is.na(index30))
head(log10(z.cut$value[index30]))
values.present <- vector(mode="numeric", length=length(xy))
min(log10(z.cut$value[index30]), na.rm=T) #0
max(log10(z.cut$value[index30]), na.rm=T) # 3.399154
values.cut <- cut(log10(z.cut$value[index30]), breaks=seq(min(log10(z.cut$value[index30]), na.rm=T),
                                                          4, by=0.4))



ordiplot(rda5, type="n", xlim=c(-10,10), ylim=c(-10,10))
points(plotcoord*30, cex=0.05, col=rev(greyPalette(n=10,  start = 0, end = 0.8, 
                                                   gamma = 2.2, alpha = NULL))[values.cut])
arrows(rep(0,2),rep(0,2),bioclimcoord[,1]*8,bioclimcoord[,2]*8, col="blue", length=0.1)
text(bioclimcoord*8,dimnames(bioclimcoord)[[1]], col="blue", cex=1)
text(traitcoord,traitnames, col="red")
# copied as fourth RDA graph into ppt


plot(CWM[,"LeafNPratio.mean"]~splot.bioclim$GDD5[index11],
     cex=0.05, xlab="GDD5", ylab="CWM ln(leaf N to P ratio)",cex.lab=1.5, cex.axis=1.5)
model1 <- lm(CWM[,"LeafNPratio.mean"]~splot.bioclim$GDD5[index11])
summary(model1)
'                             Estimate Std. Error t value Pr(>|t|)    
(Intercept)                 2.297e+00  3.891e-04  5905.2   <2e-16 ***
splot.bioclim$GDD5[index11] 6.349e-06  1.636e-08   388.1   <2e-16 ***
Multiple R-squared:  0.1193,	Adjusted R-squared:  0.1193 
'
abline(model1, lwd=3, col="blue")
# LeafNPRatio_GDD5

plot(CWM[,"SLA.mean"]~jitter(splot.bioclim$BIO_02[index11]),
     cex=0.05, xlab="BIO_02", ylab="CWM ln(SLA)",cex.lab=1.5, cex.axis=1.5)
model1 <- lm(CWM[,"SLA.mean"]~splot.bioclim$BIO_02[index11])
summary(model1)
'                                Estimate Std. Error t value Pr(>|t|)    
(Intercept)                    3.399e+00  1.448e-03  2348.2   <2e-16 ***
splot.bioclim$BIO_02[index11] -6.253e-03  1.551e-05  -403.3   <2e-16 ***

Residual standard error: 0.4034 on 1111305 degrees of freedom
Multiple R-squared:  0.1277,	Adjusted R-squared:  0.1277 
'
abline(model1, lwd=3, col="blue")
# SLA_BIO_02

plot(CWM[,"LeafNperArea.mean"]~jitter(splot.bioclim$BIO_02[index11]),
     cex=0.05, xlab="BIO_02", ylab="CWM ln(Leaf N per area)",cex.lab=1.5, cex.axis=1.5)
model1 <- lm(CWM[,"LeafNperArea.mean"]~splot.bioclim$BIO_02[index11])
summary(model1)
'                                Estimate Std. Error t value Pr(>|t|)    
(Intercept)                   -5.989e-02  8.475e-04  -70.67   <2e-16 ***
splot.bioclim$BIO_02[index11]  3.696e-03  9.079e-06  407.09   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.2362 on 1111305 degrees of freedom
Multiple R-squared:  0.1298,	Adjusted R-squared:  0.1298 
'
abline(model1, lwd=3, col="blue")
# LeafNper_BIO_02

##### FD calculations ####
j <- 3
nam <- DT$species[DT$PlotObservationID==j]
abu <- DT$Relative.cover[DT$PlotObservationID==j]
trait <- DT$trait[DT$PlotObservationID==j]
abundance.weighted=T

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

mpd.fun <- function(nam, trait, abu,abundance.weighted){
  #res <- numeric(1) # carries result from the function
  res <- as.double(NA)
  if (length(trait[!is.na(trait)])>0){
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

# from mpd source code
'N <- dim(samp)[1]
mpd <- numeric(N)
for (i in 1:N) {
  sppInSample <- names(samp[i, samp[i, ] > 0])
  if (length(sppInSample) > 1) {
    sample.dis <- dis[sppInSample, sppInSample]
    if (abundance.weighted) {
      sample.weights <- t(as.matrix(samp[i, sppInSample, 
                                         drop = FALSE])) %*% as.matrix(samp[i, sppInSample, 
                                                                            drop = FALSE])
      mpd[i] <- weighted.mean(sample.dis, sample.weights)
    }
    else {
      mpd[i] <- mean(sample.dis[lower.tri(sample.dis)])
    }
  }
  else {
    mpd[i] <- NA
  }
}
mpd
'
# from Oliver, who used picante
# does not accept missing values in the function
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
species.list <- unique(DT$species)
any(is.na(species.list)) #T, there is one NA
length(species.list) #60909
species.list <- species.list[!is.na(species.list)]
length(species.list) #60908
index8 <- match(species.list,TRY.all.mean.sd.2$species)
str(index8) # 60908
length(index8[!is.na(index8)]) #21001
#species.list2 <- TRY.all.mean.sd.2$species[index8]
#species.list2 <- species.list2[!is.na(species.list2)]
#length(species.list2) # 21001
TRY.all.mean.sd.2.na <- TRY.all.mean.sd.2[index8,]
str(TRY.all.mean.sd.2.na)
TRY.all.mean.sd.2.na$species2 <- species.list
rm(index9)
'
FD <- array(NA,c(dim(CWM)[1],18),dimnames=list(dimnames(CWM)[[1]],
                                                 colnames(TRY.all.mean.sd.2)[3:20]))
str(FD) #logi [1:1117898, 1:18] 
'for (i in 1:18){
  # create distance matrix
  #trait.vec <- TRY.all.mean.sd.2.na[,i+2]
  #names(trait.vec) <- TRY.all.mean.sd.2.na$species2
  #trait.dist.mat <- as.matrix(dist(trait.vec))
  # too large for my notebook
  #mpd[,i] <-  DT[,list(mpd.trait= mpd.fun.dt(Relative.cover, trait.dist.mat, species, abundance.weighted=TRUE)),by=PlotObservationID]$mpd.trait
  print(i)  
  DT$trait <- NA
  DT$trait <- TRY.all.mean.sd.2[index7,i+2]
  FD[,i] <-  DT[,list(mpd.trait= mpd.fun(species, trait, Relative.cover, abundance.weighted=TRUE)),by=PlotObservationID]$mpd.trait
}
write.csv(FD,file = "FD2.csv", row.names = T)
'
for (i in 1:18){
  # create distance matrix
  #trait.vec <- TRY.all.mean.sd.2.na[,i+2]
  #names(trait.vec) <- TRY.all.mean.sd.2.na$species2
  #trait.dist.mat <- as.matrix(dist(trait.vec))
  # too large for my notebook
  #mpd[,i] <-  DT[,list(mpd.trait= mpd.fun.dt(Relative.cover, trait.dist.mat, species, abundance.weighted=TRUE)),by=PlotObservationID]$mpd.trait
  print(i)  
  DT$trait <- NA
  DT$trait <- TRY.all.mean.sd.2[index7,i+2]
  # recalculate relative cover for this trait
  FD[,i] <-  DT[,list(FD.trait= FD.fun(trait, Relative.cover)),by=PlotObservationID]$FD.trait
}
write.csv(FD,file = "FD3.csv", row.names = T)

FD <- read.csv("C://Daten//iDiv2//splot2//FD3.csv")
str(FD)
dimnames(FD)[[1]] <- FD$X
FD <- FD[,-1]
tail(FD)

#### match FD to splot.header ####
index12 <- match(dimnames(FD)[[1]],splot.header$PlotObservationID)
length(index12) #1117898
any(is.na(index12)) # F
splot.header3 <- splot.header[index12,]
str(FD) # num [1:1117898, 1:18]
FD <- FD[!(is.na(splot.header3$Longitude)|is.na(splot.header3$Latitude)|is.na(FD[,"SLA.mean"])),]
str(FD) # num [1:1111307 , 1:18]
1111307 /1117898*100 # 99.41041 % of plots have traits, Long and Lat
any(is.na(FD)) #F
# match header again
index13 <- match(dimnames(FD)[[1]],splot.header$PlotObservationID)
length(index13) #1111307
any(is.na(index13)) # F
splot.header3 <- splot.header[index13,]
# now in the same sequence as FD


#### Collect all FD regressions ####
names(splot.bioclim) # from 4 BIO_01 to 49 T_MAR
index14 <- match(splot.header3$PlotObservationID,splot.bioclim$PLOT_ID)
length(index14) # 1111307
length(index14[!is.na(index14)]) # 1111307

FD.bioclim <- array(NA,c(46,18,5),dimnames=list(names(splot.bioclim)[4:49],dimnames(FD)[[2]],
                                      c("p_lin","r2_lin","p_qua1", "p_qua2","r2_qua")))
for (i in 1:46){
  for (j in 1:18){
    model1 <- lm(FD[,j]~splot.bioclim[index14,i+3])
    FD.bioclim[i,j,1] <- summary(model1)$coefficients[2,4]
    FD.bioclim[i,j,2] <- summary(model1)$r.squared
    model2 <- lm(FD[,j]~splot.bioclim[index14,i+3]+I(splot.bioclim[index14,i+3]^2))
    FD.bioclim[i,j,3] <- summary(model2)$coefficients[2,4]
    FD.bioclim[i,j,4] <- summary(model2)$coefficients[3,4]
    FD.bioclim[i,j,5] <- summary(model2)$r.squared
  }
}
str(FD.bioclim) #num [1:46, 1:18, 1:5]
write.csv(data.frame(names(splot.bioclim)[4:49],FD.bioclim[,,2]),file = "FD2_bioclim_r2_lin.csv", row.names = FALSE)
write.csv(data.frame(names(splot.bioclim)[4:49],FD.bioclim[,,5]),file = "FD2_bioclim_r2_qua.csv", row.names = FALSE)

library(vegan)
FD.pca1 <- rda(FD,scale=T)
FD.pca1
'Eigenvalues for unconstrained axes:
PC1   PC2   PC3   PC4   PC5   PC6   PC7   PC8 
8.597 1.876 1.378 0.886 0.790 0.780 0.583 0.551 
(Showed only 8 of all 18 unconstrained eigenvalues)
'
plot(FD.pca1)
summary(FD.pca1)
#text(summary(FD.pca1)$species[,c(1,2)],dimnames(summary(FD.pca1)$species)[[1]])
traitnames <- dimnames(summary(FD.pca1)$species)[[1]]
traitnames <- substr(traitnames,1,nchar(traitnames)-5)
traitcoord <- summary(FD.pca1)$species[,c(1,2)]
which(traitnames=="Seed.length") #17
traitcoord[17,2] <- traitcoord[17,2]+0.3
which(traitnames=="LeafC.perdrymass") #12
traitcoord[12,2] <- traitcoord[12,2]-0.4
which(traitnames=="Leaf.delta.15N") #13
traitcoord[13,2] <- traitcoord[13,2]+0.4
which(traitnames=="LeafNPratio") #11
traitcoord[11,2] <- traitcoord[11,2]-0.3

plotcoord <- summary(FD.pca1)$sites[,c(1,2)]
str(plotcoord) #num [1:1111307, 1:2]
x.cut <- cut(plotcoord[,1], breaks=seq(min(plotcoord[,1]), max(plotcoord[,1]), by=0.01))
y.cut <- cut(plotcoord[,2], breaks=seq(min(plotcoord[,2]), max(plotcoord[,2]), by=0.01))
z.cut <- table(x.cut,y.cut)
head(z.cut)
dim(z.cut) #131 50
z.cut <- melt(z.cut)
str(z.cut) # 6550 obs. of  3 variables:
head(z.cut)
z.cut$xy <- paste(z.cut$x.cut,z.cut$y.cut,sep="_")
str(x.cut)
length(x.cut) # 1111307
xy <- paste(x.cut,y.cut,sep="_")
index30 <- match(xy,z.cut$xy)
length(index30)
any(is.na(index30))
head(log10(z.cut$value[index30]))
values.present <- vector(mode="numeric", length=length(xy))
min(log10(z.cut$value[index30]), na.rm=T) #0
max(log10(z.cut$value[index30]), na.rm=T) # 3.399154
values.cut <- cut(log10(z.cut$value[index30]), breaks=seq(min(log10(z.cut$value[index30]), na.rm=T),
                                                          4, by=0.4))

ordiplot(FD.pca1, type="n")
#text(traitcoord,traitnames)
points(plotcoord*30, cex=0.05, col=rev(greyPalette(n=10,  start = 0, end = 0.8, 
                                                   gamma = 2.2, alpha = NULL))[values.cut])

#arrows(rep(0,45),rep(0,45),bioclimcoord[,1]*8,bioclimcoord[,2]*8, col="cadetblue4", length=0.1)
text(traitcoord,traitnames, col="red")
# copied as first RDA graph into ppt

env1 <- envfit(FD.pca1,splot.bioclim[index14,c(4:49)],permutations = 0)
env1
#with permutations = 999:
#Error: cannot allocate vector of size 4.1 Gb
plot(env1)
png(filename="PCA1.png", width=1000, height=1000, units = "px", bg="white")
ordiplot(pca1, type="n")
text(traitcoord,traitnames)
plot(env1)
dev.off() # creates a file

### Stepwise RDA ###
### RDA ###
FD.rda1 <- rda(FD~.,splot.bioclim[index14,c(4:49)],scale=T)
FD.rda1
FD.rda2 <- rda(FD ~ 1,splot.bioclim[index14,c(4:49)],scale=T)
FD.rda3 <- ordistep(FD.rda2, scope=formula(FD.rda1), direction = "both", Pin = 0.1, 
            permutations = how(nperm = 10), trace = TRUE)
'Start: FD ~ 1 

Df     AIC       F  Pr(>F)  
+ BIO_14  1 3191679 20602.8 0.09091 .
+ BIO_17  1 3191898 20379.5 0.09091 .
+ BIO_12  1 3197070 15125.2 0.09091 .
+ P_MAY   1 3198071 14110.8 0.09091 .
+ P_APR   1 3198194 13986.4 0.09091 .
+ BIO_18  1 3198767 13406.3 0.09091 .
+ P_MAR   1 3199448 12717.1 0.09091 .
+ P_FEB   1 3199451 12714.5 0.09091 .
+ P_JUN   1 3199643 12519.9 0.09091 .
+ BIO_05  1 3199804 12357.7 0.09091 .
+ BIO_02  1 3200062 12096.5 0.09091 .
+ P_NOV   1 3200602 11551.1 0.09091 .
+ P_DEC   1 3200823 11328.1 0.09091 .
+ P_JAN   1 3201335 10810.2 0.09091 .
+ BIO_10  1 3202194  9943.1 0.09091 .
+ T_JUL   1 3202226  9911.4 0.09091 .
+ T_AUG   1 3202685  9448.0 0.09091 .
+ GDD5    1 3202914  9217.7 0.09091 .
+ T_JUN   1 3202934  9196.7 0.09091 .
+ BIO_16  1 3203459  8668.3 0.09091 .
+ BIO_19  1 3203685  8440.1 0.09091 .
+ P_AUG   1 3204079  8043.8 0.09091 .
+ BIO_13  1 3204383  7737.1 0.09091 .
+ T_MAY   1 3204450  7669.7 0.09091 .
+ P_OCT   1 3204755  7362.4 0.09091 .
+ T_SEP   1 3204986  7130.0 0.09091 .
+ P_SEP   1 3205174  6941.4 0.09091 .
+ BIO_15  1 3205378  6735.8 0.09091 .
+ P_JUL   1 3205600  6512.4 0.09091 .
+ T_APR   1 3205746  6365.1 0.09091 .
+ BIO_01  1 3205841  6270.3 0.09091 .
+ BIO_03  1 3205882  6228.8 0.09091 .
+ T_OCT   1 3206048  6062.0 0.09091 .
+ AR      1 3206562  5545.3 0.09091 .
+ BIO_07  1 3207063  5042.2 0.09091 .
+ T_NOV   1 3207547  4555.5 0.09091 .
+ T_MAR   1 3207659  4443.2 0.09091 .
+ T_FEB   1 3208177  3923.3 0.09091 .
+ T_DEC   1 3208260  3840.3 0.09091 .
+ BIO_11  1 3208334  3765.7 0.09091 .
+ T_JAN   1 3208465  3634.3 0.09091 .
+ BIO_08  1 3208844  3254.4 0.09091 .
+ BIO_06  1 3209374  2723.0 0.09091 .
+ BIO_09  1 3209543  2553.3 0.09091 .
+ PET     1 3209642  2454.5 0.09091 .
+ BIO_04  1 3210074  2021.3 0.09091 .
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Step: FD ~ BIO_14 

Df     AIC     F  Pr(>F)  
- BIO_14  1 3212091 20603 0.09091 .
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Df     AIC       F  Pr(>F)  
+ BIO_02  1 3184224 7481.80 0.09091 .
+ BIO_03  1 3184435 7269.37 0.09091 .
+ GDD5    1 3187255 4435.29 0.09091 .
+ BIO_05  1 3187772 3915.70 0.09091 .
+ T_JAN   1 3188026 3661.00 0.09091 .
+ BIO_01  1 3188040 3647.18 0.09091 .
+ T_FEB   1 3188048 3638.72 0.09091 .
+ T_MAR   1 3188075 3612.29 0.09091 .
+ T_APR   1 3188082 3605.14 0.09091 .
+ BIO_11  1 3188088 3598.96 0.09091 .
+ T_DEC   1 3188123 3563.70 0.09091 .
+ T_NOV   1 3188208 3478.25 0.09091 .
+ T_OCT   1 3188225 3461.84 0.09091 .
+ T_MAY   1 3188357 3329.42 0.09091 .
+ BIO_06  1 3188491 3194.91 0.09091 .
+ BIO_10  1 3188609 3076.07 0.09091 .
+ T_SEP   1 3188617 3068.49 0.09091 .
+ BIO_13  1 3188664 3021.74 0.09091 .
+ T_JUN   1 3188669 3016.26 0.09091 .
+ BIO_16  1 3188709 2976.11 0.09091 .
+ P_SEP   1 3188724 2961.09 0.09091 .
+ BIO_07  1 3188728 2956.84 0.09091 .
+ BIO_12  1 3188782 2902.48 0.09091 .
+ T_AUG   1 3188788 2897.30 0.09091 .
+ T_JUL   1 3188879 2805.50 0.09091 .
+ P_AUG   1 3189181 2503.15 0.09091 .
+ BIO_18  1 3189256 2427.84 0.09091 .
+ BIO_04  1 3189277 2406.93 0.09091 .
+ P_JUL   1 3189398 2285.76 0.09091 .
+ BIO_15  1 3189605 2078.03 0.09091 .
+ P_APR   1 3189650 2032.61 0.09091 .
+ P_JUN   1 3189703 1979.86 0.09091 .
+ BIO_09  1 3189912 1770.71 0.09091 .
+ P_MAR   1 3189965 1717.19 0.09091 .
+ PET     1 3189984 1698.34 0.09091 .
+ AR      1 3190003 1678.99 0.09091 .
+ P_MAY   1 3190005 1677.55 0.09091 .
+ P_OCT   1 3190063 1619.65 0.09091 .
+ P_FEB   1 3190490 1192.28 0.09091 .
+ BIO_17  1 3190497 1184.39 0.09091 .
+ BIO_19  1 3190555 1126.59 0.09091 .
+ P_NOV   1 3190570 1111.68 0.09091 .
+ BIO_08  1 3190582 1100.05 0.09091 .
+ P_JAN   1 3190893  787.98 0.09091 .
+ P_DEC   1 3190955  726.52 0.09091 .
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Step: FD ~ BIO_14 + BIO_02 

Df     AIC       F  Pr(>F)  
- BIO_02  1 3191679  7481.8 0.09091 .
- BIO_14  1 3200062 15953.2 0.09091 .
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1'

FD.rda4 <- rda(FD ~ BIO_14,splot.bioclim[index14,c(4:49)],scale=T)
FD.rda4
'              Inertia Proportion Rank
Total         18.0000     1.0000     
Constrained    0.3276     0.0182    1
Unconstrained 17.6724     0.9818   18
Inertia is correlations 

Eigenvalues for constrained axes:
RDA1 
0.3276 

Eigenvalues for unconstrained axes:
PC1   PC2   PC3   PC4   PC5   PC6   PC7   PC8 
8.307 1.850 1.378 0.884 0.789 0.780 0.578 0.551 
'
traitnames <- dimnames(summary(FD.rda4)$species)[[1]]
traitnames <- substr(traitnames,1,nchar(traitnames)-5)
traitcoord <- summary(FD.rda4)$species[,c(1,2)]
bioclimcoord <- summary(FD.rda4)$biplot[,c(1,2)]
plotcoord <- summary(FD.rda4)$sites[,c(1,2)]
str(plotcoord) #num [1:1111307, 1:2]
x.cut <- cut(plotcoord[,1], breaks=seq(min(plotcoord[,1]), max(plotcoord[,1]), by=0.01))
y.cut <- cut(plotcoord[,2], breaks=seq(min(plotcoord[,2]), max(plotcoord[,2]), by=0.01))
z.cut <- table(x.cut,y.cut)
head(z.cut)
dim(z.cut) #131 50
z.cut <- melt(z.cut)
str(z.cut) # 6550 obs. of  3 variables:
head(z.cut)
z.cut$xy <- paste(z.cut$x.cut,z.cut$y.cut,sep="_")
str(x.cut)
length(x.cut) # 1111307
xy <- paste(x.cut,y.cut,sep="_")
index30 <- match(xy,z.cut$xy)
length(index30)
any(is.na(index30))
head(log10(z.cut$value[index30]))
values.present <- vector(mode="numeric", length=length(xy))
min(log10(z.cut$value[index30]), na.rm=T) #0
max(log10(z.cut$value[index30]), na.rm=T) # 3.399154
values.cut <- cut(log10(z.cut$value[index30]), breaks=seq(min(log10(z.cut$value[index30]), na.rm=T),
                                                          4, by=0.4))

ordiplot(FD.rda4, type="n", xlim=c(-10,10),ylim=c(-10,12))
points(plotcoord*15, cex=0.05, col=rev(greyPalette(n=10,  start = 0, end = 0.8, 
                                                   gamma = 2.2, alpha = NULL))[values.cut])
arrows(rep(0,1),rep(0,1),bioclimcoord[1]*8,bioclimcoord[2]*8, col="blue", length=0.1)
text(bioclimcoord[1]*8,bioclimcoord[2]*8+0.5,"BIO_14", col="blue", cex=1)
text(traitcoord,traitnames, col="red")
# copied as third RDA graph into ppt

FD.rda5 <- rda(FD ~ BIO_14+BIO_02,splot.bioclim[index14,c(4:49)],scale=T)
FD.rda5
'
Inertia Proportion Rank
Total         18.00000    1.00000     
Constrained    0.44582    0.02477    2
Unconstrained 17.55418    0.97523   18
Inertia is correlations 

Eigenvalues for constrained axes:
RDA1   RDA2 
0.3641 0.0817 

Eigenvalues for unconstrained axes:
PC1   PC2   PC3   PC4   PC5   PC6   PC7   PC8 
8.267 1.816 1.377 0.880 0.788 0.780 0.568 0.530 '
traitnames <- dimnames(summary(FD.rda5)$species)[[1]]
traitnames <- substr(traitnames,1,nchar(traitnames)-5)
traitcoord <- summary(FD.rda5)$species[,c(1,2)]
bioclimcoord <- summary(FD.rda5)$biplot[,c(1,2)]

plotcoord <- summary(FD.rda5)$sites[,c(1,2)]
str(plotcoord) #num [1:1111307, 1:2]
x.cut <- cut(plotcoord[,1], breaks=seq(min(plotcoord[,1]), max(plotcoord[,1]), by=0.01))
y.cut <- cut(plotcoord[,2], breaks=seq(min(plotcoord[,2]), max(plotcoord[,2]), by=0.01))
z.cut <- table(x.cut,y.cut)
head(z.cut)
dim(z.cut) #131 50
z.cut <- melt(z.cut)
str(z.cut) # 6550 obs. of  3 variables:
head(z.cut)
z.cut$xy <- paste(z.cut$x.cut,z.cut$y.cut,sep="_")
str(x.cut)
length(x.cut) # 1111307
xy <- paste(x.cut,y.cut,sep="_")
index30 <- match(xy,z.cut$xy)
length(index30)
any(is.na(index30))
head(log10(z.cut$value[index30]))
values.present <- vector(mode="numeric", length=length(xy))
min(log10(z.cut$value[index30]), na.rm=T) #0
max(log10(z.cut$value[index30]), na.rm=T) # 3.399154
values.cut <- cut(log10(z.cut$value[index30]), breaks=seq(min(log10(z.cut$value[index30]), na.rm=T),
                                                          4, by=0.4))

ordiplot(FD.rda5, type="n",  xlim=c(-10,10),ylim=c(-10,12))
points(plotcoord*15, cex=0.05, col=rev(greyPalette(n=10,  start = 0, end = 0.8, 
                                                   gamma = 2.2, alpha = NULL))[values.cut])
arrows(rep(0,2),rep(0,2),bioclimcoord[,1]*4,bioclimcoord[,2]*4, col="blue", length=0.1)
text(bioclimcoord*4,dimnames(bioclimcoord)[[1]], col="blue", cex=1)
text(traitcoord,traitnames, col="red")
# copied as fourth RDA graph into ppt

plot(FD[,"Seed.length.mean"]~splot.bioclim$BIO_14[index14],
     cex=0.05, xlab="BIO_14", ylab="FD ln(seed length)",cex.lab=1.5, cex.axis=1.5)
model1 <- lm(FD[,"Seed.length.mean"]~splot.bioclim$BIO_14[index14])
summary(model1)
'                               Estimate Std. Error t value Pr(>|t|)    
(Intercept)                   2.584e-01  3.193e-04   809.1   <2e-16 ***
splot.bioclim$BIO_14[index14] 1.407e-04  6.813e-07   206.5   <2e-16 ***
Multiple R-squared:  0.03697,	Adjusted R-squared:  0.03697 
'
abline(model1, lwd=3, col="blue")
# 

plot(FD[,"Seed.num.rep.unit.mean"]~splot.bioclim$BIO_02[index14],
     cex=0.05, xlab="BIO_02", ylab="FD ln(Seed number of reproductive unit)",cex.lab=1.5, cex.axis=1.5)
model1 <- lm(FD[,"Seed.num.rep.unit.mean"]~splot.bioclim$BIO_02[index14])
summary(model1)
'                                 Estimate Std. Error t value Pr(>|t|)    
(Intercept)                    1.251e+00  1.799e-03   695.3   <2e-16 ***
splot.bioclim$BIO_02[index14] -3.751e-03  1.927e-05  -194.6   <2e-16 ***
Multiple R-squared:  0.03296,	Adjusted R-squared:  0.03296  '
abline(model1, lwd=3, col="blue")












# mpd presence-absence
system.time(
  mpd.sla.no.abu <-  DT[,list(mpd.sla = mntd.fun.dt(Cover.., SLA.dist.mat, StandSpeciesName, abundance.weighted=FALSE)),by=PlotObservationID]
)


###########
levels(splot.header2$COUNTRY_CO)
levels(splot.header$Country)
CWM2$Country <- splot.header$Country[index12]
data(countryExData)
str(countryExData)
sPDF <- joinCountryData2Map(splot.header, joinCode = "NAME",nameJoinColumn = "Country")
?mapCountryData
#mapCountryData(sPDF, nameColumnToPlot="BIODIVERSITY" )

#### Climatic space, from Jonathon Lenoir ####
library(fBasics)
# Color palette

# Import bio1 (MAT) of the world
bio <- read.table(paste("/home/helge/sPlot/Bioclim/", "BIO.txt", sep = ""),sep=";", header=T)
str(bio) #9033453 obs. of  22 variables
r <- raster(nrows=33, ncols=33, xmn=min(bio$BIO_01), xmx=max(bio$BIO_01), ymn=min(bio$BIO_12), ymx=max(bio$BIO_12))
mattar <- data.frame(bio$BIO_01,bio$BIO_12)
str(mattar)
names(mattar) <- c("bio1","bio12")
mattar_r <- rasterize(mattar, r, fun="count")
str(mattar_r)
plot(log10(mattar_r), asp=0, col=rev(divPalette(n=100, name=c("RdYlBu"))), 
     main="N 2.5 arc-minutes cells per bin (log10 scale)", cex.main=0.7,
     xlab="MAT (°C*10)",  ylab="TAR (mm)")
# Bio1_Bio12_world.png

#plot(mattar_r/max(getValues(mattar_r), na.rm=TRUE), asp=0, col=rev(divPalette(n=100, name=c("RdYlBu"))), main="Relative proportion of 10 arc-minutes cells per bin", xlab="MAR (°C*10)", ylab="TAR (mm)")
# Plot the sampling effort of sPlot across the MAT-TAR climatic space
str(splot.bioclim)
# Make it a spatial data frame
CRSlonlat <- CRS("+proj=longlat +datum=WGS84")
splot.bioclim.coords <- cbind(splot.bioclim$X, splot.bioclim$Y)
splot.bioclim.coords <- SpatialPoints(splot.bioclim.coords, proj4string=CRSlonlat)
splot.bioclim2 <- data.frame(splot.bioclim$BIO_01,splot.bioclim$BIO_12)
names(splot.bioclim2) <- c("bio1","bio12")
#splot.bioclim2 <- SpatialPointsDataFrame(splot.bioclim.coords, splot.bioclim2, proj4string=CRSlonlat)
#str(splot.bioclim2)
#sPlot_mattar <- extract(bio1, coordinates(sPlot))
#sPlot_mattar <- as.data.frame(sPlot_mattar)
#names(sPlot_mattar) <- c("bio1")
#sPlot_mattar$bio12 <- extract(bio12, coordinates(sPlot))
sPlot_mattar_r <- rasterize(splot.bioclim2, r, fun="count")
str(sPlot_mattar_r)
plot(mattar_r, asp=0, col="grey", legend=FALSE, main="N vegetation plots per bin (log10 scale)",
     xlab="MAT (°C)", ylab="TAR (mm)")
# Bio1_Bio12_world_grey.png
plot(log10(sPlot_mattar_r), asp=0, col=rev(divPalette(n=100, name=c("RdYlBu"))), 
     add=TRUE)
# Bio1_Bio12_sPlot_SLA.png

# Build the PC1-PC2 climatic space
load("/home/helge/sPlot/Bioclim/pca_data.RData")
str(pca.data)
r <- raster(nrows=33, ncols=33, xmn=min(pca.data$PC1), xmx=max(pca.data$PC1), ymn=min(pca.data$PC2), ymx=max(pca.data$PC2))
pca_r <- rasterize(pca.data[, c("PC1", "PC2")], r, fun="count")
plot(log10(pca_r), asp=0, col=rev(divPalette(n=100, name=c("RdYlBu"))), 
     main="N 2.5 arc-minutes cells per bin (log10 scale)", 
     xlab="PC1 (cold-to-warm)", ylab="PC2 (dry-to-wet)")
plot(pca_r/max(getValues(pca_r), na.rm=TRUE), asp=0, 
     col=rev(divPalette(n=100, name=c("RdYlBu"))), 
     main="Relative proportion of 2.5 arc-minutes cells per bin", 
     xlab="PC1 (cold-to-warm)", ylab="PC2 (dry-to-wet)")
# Plot the sampling effort of sPlot across the PC1-PC2 climatic space
index13 <- match(splot.bioclim$RAST_ID,pca.data$RAST_ID)
sPlot_pca <- pca.data[index13, c("PC1", "PC2")]
sPlot_pca_r <- rasterize(sPlot_pca, r, fun="count")
plot(pca_r, asp=0, col="grey", legend=FALSE, 
     main="N vegetation plots per bin (log10 scale)", 
     xlab="PC1 (cold-to-warm)", ylab="PC2 (dry-to-wet)")
plot(log10(sPlot_pca_r), asp=0, col=rev(divPalette(n=100, name=c("RdYlBu"))), 
     add=TRUE)

#### For Marten Winter ####
# species x country Matrix
species.country <- table(DT$species,splot.header$Country[index30])
str(species.country) # int [1:60908, 1:130]
names(dimnames(species.country))
names(dimnames(species.country)) <- c("species","country")

library(reshape2)
?melt.table
species.country2 <- melt(species.country)
str(species.country2) # 	188470 obs. of  3 variables:
names(species.country2)
names(species.country2)[3] <- "p_a"
table(species.country2$p_a)

species.country2$p_a[species.country2$p_a>0] <- 1
species.country2 <- species.country2[species.country2$p_a==1,]
str(species.country2) # 	188470 obs. of  3 variables:
species.country2 <- species.country2[,-3]
str(species.country2) # 188470
write.csv(species.country2,file = 
            paste("/home/oliver/shared/", "species.country2.csv", sep = ""), row.names = FALSE)
levels(species.country2$country) # 130
length(species.country2$country[species.country2$country=="Algeria"])
# 97
sum(species.country[,2]) 
# 799, this is the amount of records for DZA = Algeria
sum(species.country[,126]) 
# 1467200, sum of records for United States

TaxaNat <- read.csv(paste("/home/oliver/shared/Glonaf/", "TaxaNat.csv", sep = ""))
str(TaxaNat) #178779 obs. of  2

GlonafRegion <- read.csv(paste("/home/oliver/shared/Glonaf/", "GlonafRegion.csv", sep = ""))
str(GlonafRegion) #843 obs. of  10
trim.trailing <- function (x) sub("\\s+$", "", x)
# returns string w/o leading or trailing whitespace
GlonafRegion$Country_sPlot <- as.factor(trim.trailing(as.character(GlonafRegion$Country_sPlot)))
index14 <- match(levels(species.country2$country),levels(GlonafRegion$Country_sPlot))
index14
levels(GlonafRegion$Country)

data.frame(levels(species.country2$country),levels(GlonafRegion$Country_sPlot)[index14])
## all correct

index15 <- match(levels(GlonafRegion$Country_sPlot),GlonafRegion$Country_sPlot)
index15
index16 <- match(species.country2$country,levels(species.country2$country))
any(is.na(index16)) # F
length(levels(species.country2$species)) #60908
'species.country2$species.country <- paste(species.country2$species,
levels(species.country2$country)[index16],sep="_")
species.country2$species.country <- paste(species.country2$species,
levels(GlonafRegion$Country_sPlot)[index14][index16],sep="_")
species.country2$species.country <- paste(species.country2$species,
GlonafRegion$Country_sPlot[index15][index14][index16],sep="_")
'
species.country2$species.country <- paste(species.country2$species,
                                          GlonafRegion$ISO.country[index15][index14][index16],sep="_")
str(species.country2) # 188470 obs. of  3 variables:
head(species.country2)
species.country2[c(160000:160100),]
## fits!

index17 <- match(TaxaNat$Region,GlonafRegion$ID.region)
any(is.na(index17)) # F

TaxaNat$species.country <-paste(TaxaNat$Taxon2,GlonafRegion$ISO.country[index17], sep="_")

index18 <- match(species.country2$species.country,TaxaNat$species.country)
length(index18) #188470
length(index18[!is.na(index18)]) # 9283 out of 188470 species x country occurrences
# are alien occurrences
9283/188470*100 # 4.925452 % species x country occurrences
species.country2$alien <- F
species.country2$alien[!is.na(index18)] <- T
species.country2$ISO.country <- GlonafRegion$ISO.country[index15][index14][index16]
str(species.country2)
head(species.country2)
length(species.country2$ISO.country[species.country2$ISO.country=="DZA"])
# 12939, this cannot be
#species.country2$ISO.country[species.country2$ISO.country=="DZA"]
sum(species.country[,2]) 
# 799, this is the amount of records for DZA = Algeria
length(species.country2$ISO.country[species.country2$ISO.country=="DZA"
                                    & !is.na(species.country2$ISO.country)])
# 97!

#table(species.country2$ISO.country)
#species.country3 <- acast(species.country2, species~ISO.country, length)
length(species.country2$alien[species.country2$alien])
# 9283 alien records in sPlot
length(species.country2$alien[species.country2$alien & species.country2$ISO.country=="DZA"])
# 3 alien records in DZA = Algeria
length(species.country[species.country[,2]>0,2])
# 97, however there are only 97 records in total
length(species.country2$alien[species.country2$alien & species.country2$ISO.country=="USA"])
# 1911 alien species in USA
length(species.country[species.country[,126]>0,2])
# 9933 species in total in USA
alien.richness <- as.matrix(table(species.country2$ISO.country,species.country2$alien))[,2]

species.country3 <- species.country
species.country3[species.country3>0] <- 1
country.richness <- colSums(species.country3)
country.richness
names(country.richness)
data.frame(names(country.richness),GlonafRegion$ISO.country[index15][index14])
country.richness2 <- data.frame(GlonafRegion$ISO.country[index15][index14],as.numeric(country.richness))
names(country.richness2) <- c("ISO.country","richness")
country.richness2$ISO.country
country.richness2

index19 <- match(country.richness2$ISO.country,names(alien.richness))
index19
alien.richness
country.richness2$alien.richness <- alien.richness[index19]
country.richness2
country.richness2$alien.prop <- country.richness2$alien.richness/country.richness2$richness
country.richness2$alien.prop

sPDF <- joinCountryData2Map(country.richness2,joinCode = "ISO3",
                            nameJoinColumn = "ISO.country")
'112 codes from your data successfully matched countries in the map
18 codes from your data failed to match with a country code in the map
132 codes from the map werent represented in your data
'
mapParams <- mapCountryData(sPDF, nameColumnToPlot="richness",addLegend=FALSE)
do.call(addMapLegend, c(mapParams, legendWidth=0.5, legendMar = 2,legendLabels="all"))
mapParams <- mapCountryData(sPDF, nameColumnToPlot="richness",
                            catMethod=seq(0,10000,by=1000),addLegend=FALSE)
do.call(addMapLegend, c(mapParams, legendWidth=0.5, legendMar = 2,legendLabels="all"))

mapParams <- mapCountryData(sPDF, nameColumnToPlot="alien.richness",
                            catMethod=seq(0,2000,by=200),addLegend=FALSE)
do.call(addMapLegend, c(mapParams, legendWidth=0.5, legendMar = 2,legendLabels="all"))

max(country.richness2$alien.prop, na.rm=T) #0.2823779
mapParams <- mapCountryData(sPDF, nameColumnToPlot="alien.prop",addLegend=FALSE)
mapParams <- mapCountryData(sPDF, nameColumnToPlot="alien.prop",
                            catMethod=seq(0,0.3,by=0.02),addLegend=FALSE)
do.call(addMapLegend, c(mapParams, legendWidth=0.5, legendMar = 2,legendLabels="all"))




##################### OLD ###################
library(spdep)
library(maptools)
library(hexbin)
library(RColorBrewer)
# Use a lighter version of sPlot focusing on mean annual temperature MAT (BIO1)
# and total annual rainfall TAR (BIO12)
sPlot <- splot.header[, c("LONGITUDE", "LATITUDE", "BIO_1", "BIO_12")]
sPlot$LONGITUDE <- as.numeric(as.character(sPlot$LONGITUDE))
sPlot$LATITUDE <- as.numeric(as.character(sPlot$LATITUDE))
sPlot$BIO_1 <- as.numeric(sPlot$BIO_1)/10
sPlot$BIO_12 <- as.numeric(sPlot$BIO_12)
str(sPlot)
# Make it a spatial data frame
CRSlonlat <- CRS("+proj=longlat +datum=WGS84")
coords <- cbind(sPlot$LONGITUDE, sPlot$LATITUDE)
coords <- SpatialPoints(coords, proj4string=CRSlonlat)
sPlot <- SpatialPointsDataFrame(coords, sPlot, proj4string=CRSlonlat)
str(sPlot)
# Plot the data on the world map to check the coordinates and projection
data(wrld_simpl)
plot(wrld_simpl)
points(coordinates(sPlot), cex=0.1, col="red", pch=16)
# Set a color ramp
rf <- colorRampPalette(rev(brewer.pal(11, 'Spectral')))
# Plot the plot distribution within the MAT-TAR bidimentionnal climatic space
hexbinplot(BIO_12~BIO_1, data=sPlot@data, colramp=rf, trans=log, inv=exp,
           border="white", xlab="MAT", ylab="TAR", xbins=30)
# Display only bins with a minimum of 10 plots and a maximum of 1000 plots
hexbinplot(BIO_12~BIO_1, data=sPlot@data, colramp=rf, trans=log, inv=exp,
           border="white", xlab="MAT", ylab="TAR", xbins=30, mincnt=10, maxcnt=1000)
#








################ OLD ###################

###
load("C:\\Daten\\iDiv\\sPlot\\pca_data.Rdata")
str(pca.data)
any(is.na(pca.data$PC1)) #F

load("C:\\Daten\\iDiv\\sPlot\\sPlot_header_pca.RData")
str(splot.header2)
any(is.na(pca.data$PC1)) #F

library(raster)
r <- 
  splot_pca_r <- rasterize(splot_pca, r, run="count")
plot(splot_pca_r, asp=0, col=rev(divPalette(n=100, name="RdY1Bu"))))
plot(splot_r, asp=0, col=rev(divPalette(n=100, name="RdY1Bu"))))

index <- which(getValues(splot_pca_r)>0)
which(getValues(pca_r)>0)
poinra
getValues(splot_pca_r)[index] # 136 values
?sample
splo_pca$binid <- cellFromXY(pca_r, splot_pca)


###########################
load("C:\\Daten\\iDiv\\sPlot\\splot_pca2.Rdata")
str(splot_pca)
splot_pca <- data.table(splot_pca)
setkey(splot_pca,binid)
inter <- splot_pca[,list(count=length(PCA1)), by=binid]
splot_pca2 <- splot_pca[,list(PCA1, PCA2,count=length(PCA1)), by=binid]

str(splot_pca2)
hist(splot_pca2$binid)
inter2 <- splot_pca2[count>100,list(count=sum(PCA1)), by=binid]
str(inter2)
[,list(total.cover=sum(Cover..)), by=PlotObservationID]
<- DT[,list(total.cover=sum(Cover..)), by=PlotObservationID]

sample(splot_pca$PCA1,100)
splot_pca2 <-  splot_pca[table(splot_pca$binid)>100,sample(PCA1,100),by=binid]
str(splot_pca2)

resample <- function(PCA1_,PCA2_,binid_){
  if (length(PCA1)>100) {
    
  }
  else {
    1
  }
  ]index10 <- match(species,labels(d))
  d3 <- as.dist(dist[index10,index10])
  if (sum(d3)==0){
    NA
  }
  divc(as.data.frame(rel.cover),d3)$diversity
}
splot_pca2 <-  splot_pca[length(PCA1)<=100,res_ID := 1]
head(splot_pca2,100)
table(splot_pca2$binid,splot_pca2$count, exclude=NULL)
table(splot_pca$binid)
splot_pca2 <-  splot_pca[,res_ID := 0]
splot_pca2 <-  splot_pca[table(splot_pca2$binid)<=100,res_ID := 1]
splot.selection <-  splot_pca[,list(res = resample(PCA1,PCA2,binid)),by=binid]
#length(splot_pca[binid>100])
hist(splot_pca$binid)
splot.selection <-  splot_pca[,list(length(binid)),by=binid]
splot.selection <-  splot_pca[,list(count=length(PCA1)),by=binid]
splot.selection <-  splot_pca[,,by=binid]
hist(splot.selection$count)
str(splot.selection)
str(splot_pca2)
splot.selection <-  splot_pca[,list(nrow()),by=binid]
splot.selection <-  splot_pca[,count:=length(binid),by=binid]
length(splot_pca,by=binid)
########################################################################################

### CWM ###
#CWM1 <-  DT[!is.na(plot.species.trait.value),list(CWM = weighted.mean(plot.species.trait.value,Relative.cover)),by=PlotObservationID]
CWM1 <-  DT[,list(CWM = weighted.mean(plot.species.trait.value,Relative.cover)),by=PlotObservationID]
# see http://stackoverflow.com/questions/3367190/aggregate-and-weighted-mean-in-r
CWM1[1:100,]
hist(CWM1$CWM)
write.csv((x, file = paste(path.sPlot, "CWM_SLA_all.csv", sep = ""), row.names = FALSE))

### FD ###
library(ade4)    # f?r divc (Rao's Q)
# this does not work: ~50 GB RAM required for 40k x 40k species matrix:
'ktab1 <- ktab.list.df(list(as.data.frame(species.trait.value[!is.na(species.trait.value)])))
d <- dist.ktab(ktab1, type=c("Q"), option = "scaledBYrange")
#labels(d)
#str(d)
#attr(d,"Labels")
attr(d,"Labels") <- dimnames(species.trait.value)[[1]][!is.na(species.trait.value)]
labels(d)
any(is.na(d)) # F
dist <- as.matrix(d)

#tree.trait.weighted <- t(t(veg.KHG3.prop)  * all.traits.KHG[,j])
#dist.sqrt <- sqrt(dist)
d.sqrt <- sqrt(d)
labels(d.sqrt)
dist <- as.matrix(d.sqrt)
'
'Rao <- function(rel.cover,species){
index10 <- match(species,labels(d))
d3 <- as.dist(dist[index10,index10])
if (sum(d3)==0){
NA
}
divc(as.data.frame(rel.cover),d3)$diversity
}
FD.all <-  DT[!is.na(plot.species.trait.value),list(Rao.Q = Rao(Relative.cover,Matched.concept)),by=PlotObservationID]
'
#species.trait.value.scaled <- species.trait.value/(max(species.trait.value)-min(species.trait.value))
species.trait.value.scaled <- TRY.mean.sd.splot$SLA.mean/(max(TRY.mean.sd.splot$SLA.mean)-min(TRY.mean.sd.splot$SLA.mean))

# scale by range, which saves time for distance calculations

library(reshape2)

package.size = 1000
#number.plots.total <- length(levels(DT[!is.na(plot.species.trait.value)]$PlotObservationID)) # 705272
number.plots.total <- length(levels(DT$PlotObservationID)) # 701611 vs. 705272
FD.all <- array(0,number.plots.total, dimnames=list(levels(DT$PlotObservationID)))
number.packages <-ceiling(number.plots.total/package.size) #702
pb <- winProgressBar(title = "progress bar", min = 0, max =number.packages, width = 300)
for (k in 1:number.packages) {
  setWinProgressBar(pb, k, title=paste("Row ",k,"of ", number.packages, " is processed"))
  ifelse (k==number.packages, package<-number.plots.total%%package.size, package <- package.size)
  #index3 <- match(DT[!is.na(plot.species.trait.value)]$PlotObservationID,levels(DT[!is.na(plot.species.trait.value)]$PlotObservationID)[seq((k-1)*package.size+1,k*package.size,1)])
  index3 <- match(DT$PlotObservationID,levels(DT$PlotObservationID)[seq((k-1)*package.size+1,k*package.size,1)])
  #DT2 <- DT[!is.na(plot.species.trait.value) & !is.na(index3)]
  #DT2 <- DT2[!is.na(plot.species.trait.value)]
  #length(index3[!is.na(index3)])
  DT2 <- DT[!is.na(index3)]
  #str(DT2)
  #veg.matrix1 <- acast(DT2, Matched.concept ~ PlotObservationID, value.var="Relative.cover", fill=0)  
  veg.matrix1 <- acast(DT2, names.stand ~ PlotObservationID, value.var="Relative.cover", fun.aggregate=sum, fill=0)  
  # fun.aggregate = sum -> sums up multiple entries per species!
  # this can happen with the unified species nomenclature!
  #length(unique(dimnames(veg.matrix1)[[2]]))# 1000
  #length(unique(DT2$PlotObservationID)) # 1000
  #veg.matrix1[veg.matrix1 > 0] <- 1
  #any(colSums(veg.matrix1)!=1) #F
  index4 <- match(dimnames(veg.matrix1)[[1]],species.list)
  species.trait.value.scaled2 <- species.trait.value.scaled[index4]
  species.list2 <- species.list[index4]
  ktab1 <- ktab.list.df(list(as.data.frame(species.trait.value.scaled2)))
  d <- dist.ktab(ktab1, type=c("Q"), option = "noscale")
  #labels(d)
  #str(d)
  #attr(d,"Labels")
  attr(d,"Labels") <- species.list2
  #labels(d)
  #any(is.na(d)) # F
  #dist <- as.matrix(d)
  #as.matrix(d)[1:8,1:8]
  d <- sqrt(d)
  #dist <- as.matrix(d)
  #rm(d)
  if (k<number.packages){
    FD.all[seq((k-1)*package.size+1,k*package.size,1)] <- 
      divc(as.data.frame(veg.matrix1),d)$diversity
    #DT2[,list(Rao.Q = Rao(Relative.cover,Matched.concept)),by=PlotObservationID]$Rao.Q
    any(colSums(veg.matrix1)==0) # F o.K.
    any(colSums(veg.matrix1)!=1) # T ups....
    hist(colSums(veg.matrix1))
    any(rowSums(veg.matrix1)==0) # F o.K.
  } else {
    FD.all[seq((k-1)*package.size+1,number.plots.total,1)] <- 
      divc(as.data.frame(veg.matrix1),d)$diversity
    #DT2[,list(Rao.Q = Rao(Relative.cover,Matched.concept)),by=PlotObservationID]
  }
}
close(pb)
#FD.all
write.csv(FD.all, file = paste(path.sPlot, "FDall.csv", sep = ""), row.names = FALSE)

