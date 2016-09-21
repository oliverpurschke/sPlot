##############################
### read in sPlot database ###
##############################
rm(list=ls())    
gc()

#path.sPlot <- "/home/oliver/Dokumente/PhD/PostPhD/IDiv/sDiv/sPlot/Analyses/Data/Species/sPlot/sPlot_20_11_2014/"
#path.sPlot <- "C:\\Daten\\iDiv\\sPlot\\2014_December\\"
#path.sPlot <- "D:\\Helge\\sPlot\\"
getwd()
#path.sPlot <- "/data"
#path.sPlot <- "/home/helge/sPlot2/"

#path.sPlot <- "/home/oliver/shared/Backbone_v.2/"
# type in FileZilla in the server path: /home/oliver/shared
#setwd(path.sPlot)

##### DATA ####
sPlot3 <- read.csv(paste(getwd(),"/data/sPlot_14_04_2015_species.csv", 
                         sep = ""), sep="\t", quote = "\"'")
str(sPlot3) # 24241941 obs. of  11 variables:
# column 11 in sPlot_08_04_2015_species.csv is "x_"
# This numeric variable either holds basal area (BA), individual count (IV)
# importance value (IV), per cen frequency (PF) or stem count (SC)
# Which type of information is given, is shown in the Cover.code variable:
levels(sPlot3$Cover.code)
# .... "x"    "x_BA" "x_IC" "x_IV" "x_PF" "x_SC"
# here, "x" stands for p/a data.
# In the first steps of processing the data, the different types of information 
# is combined with the cover values in Cover..
any(is.na(sPlot3$Cover.code)) # T
length(sPlot3$Cover.code[is.na(sPlot3$Cover.code)]) #  30252

###### Backbone #####
load("backbone.v.2.splot.try3.Rdata")
# alternatively:
#sPlot3 <- read.csv(paste(path.sPlot, "backbone.v.2.splot.try3.csv", 
#                         sep = ""), sep=",", quote = "\"'")
str(backbone.splot.try3) 
#data.frame':	122901 obs. of  33 variables:
'$ Name_number                 : int  1 2 3 4 5 6 7 8 9 10 ...
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
'The first column "names.sPlot.TRY" are the "raw" names from sPlot and TRY.
The last five columns are the final outcome of the whole matching procedure: 
"Status.correct",Â  "name.correct",Â  "rank.correct", "family.correct",Â  "name.short.correct",Â  "rank.short.correct".
I guess "name.short.correct" will be the most important column and should be used for most of the species-trait matching and other analyses.
'
length(unique(backbone.splot.try3$name.short.correct)) #  86529

###### Match sPlot3 (Matched.concept) with backbone.splot.try3 (name.short.correct)
index1 <- match(sPlot3$Matched.concept,backbone.splot.try3$names.sPlot.TRY)
any(is.na(index1)) #F Excellent
head(backbone.splot.try3$name.short.correct[index1])
any(is.na(backbone.splot.try3$name.short.correct[index1])) # T
length(sPlot3$Matched.concept[!is.na(backbone.splot.try3$name.short.correct[index1])])
# 24221565
length(sPlot3$Matched.concept[is.na(backbone.splot.try3$name.short.correct[index1])])
# 20376
sPlot3$Matched.concept[is.na(backbone.splot.try3$name.short.correct[index1])]
backbone.splot.try3$name.short.correct[index1][is.na(backbone.splot.try3$name.short.correct[index1])]
# there is NA in the name.short.correct
sPlot3$species <- backbone.splot.try3$name.short.correct[index1]
length(sPlot3$species[is.na(sPlot3$species)])
# 20376

#remove NAs
#sPlot4 <- sPlot3[!is.na(sPlot3$species),]
#str(sPlot4) # 24221565 x 11 var.
#rm(sPlot4)

###### Header ######
#splot.header <- read.csv(paste(path.sPlot, "sPlot_header.csv", sep = ""), 
#                         sep = "\t", na.strings=c("","NA"), fileEncoding = "UTF-8",quote = "")
#splot.header <- read.csv(paste("/home/helge/sPlot2/", "sPlot_header.csv", sep = ""), 
#                         sep = "\t", na.strings=c("","NA"), fileEncoding = "UTF-8",quote = "")
splot.header <- read.csv("sPlot_header.csv", 
                         sep = "\t", na.strings=c("","NA"), fileEncoding = "UTF-8",quote = "")
# without comments

dim(splot.header)
# 1117940      41
search.tab <- regexpr("\t",splot.header$Dataset)
head(search.tab)
table(search.tab)
# search.tab
# -1 
# 1117940 
# -> no tabulator in Dataset ???

str(splot.header)
#tail(splot.header)
write.csv(splot.header[,c("PlotObservationID","Longitude","Latitude")],file = 
            paste("/home/helge/sPlot2/", "sPlot_14_04_2015_coord.csv", 
                  sep = ""), row.names = FALSE)
index30 <- match(sPlot3$PlotObservationID, splot.header$PlotObservationID)
any(is.na(index30)) #F
length(index30) #  24241941
length(unique(index30[!is.na(index30)])) # 1117940
#sPlot3[head(which(is.na(index30))),]

index31 <- match(splot.header$PlotObservationID,sPlot3$PlotObservationID)
any(is.na(index31)) #F
length(index31) #   1117940
#length(index31[is.na(index31)]) # 3
length(unique(index31[!is.na(index31)])) # 1117940
#splot.header[which(is.na(index31)),]
#splot.header <- splot.header[-which(is.na(index31)),]
dim(splot.header)
# 1117940      40
#index32 <- match(splot.header$PlotObservationID,sPlot3$PlotObservationID)
#any(is.na(index32)) #F
identical(as.character(splot.header$PlotObservationID),as.character(sPlot3$PlotObservationID[index31]))
# T
head(cbind(splot.header$PlotObservationID,sPlot3$PlotObservationID[index31]))
head(index31)
#head(splot.header$PlotObservationID)

library(data.table)
DT <- data.table(sPlot3)
str(DT) # 24241941 obs. of  12 variables:
tables()
length(unique(DT$PlotObservationID)) # 1117940
setkey(DT,PlotObservationID)

### Species number (more precisely data entry number, as layers counted multiple times) ###
'plot.species.number <- DT[,list(species.number=length(Cover..)), by=PlotObservationID]
str(plot.species.number) #  1117940
hist(plot.species.number$species.number)
min(plot.species.number$species.number) # 1
max(plot.species.number$species.number) # 797
which(plot.species.number$species.number==max(plot.species.number$species.number))
# 51010
'
### Relative Cover ###
plot.total.cover <- DT[,list(total.cover=sum(Cover..)), by=PlotObservationID]
any(is.na(DT$Cover..)) # F
str(plot.total.cover)

any(is.na(plot.total.cover$total.cover)) # F
length(plot.total.cover$total.cover[plot.total.cover$total.cover==0]) 
# 47504
# this includes only plots, in which none of the species has a cover != 0
# there are also cases, in which some spcies have a cover > 0, but others have
# Cover.code=="x" or =="X_BA" etc. or =="NA"

index1a <- match(DT$PlotObservationID,plot.total.cover$PlotObservationID)
length(index1a) # 24241941
any(is.na(index1a)) # F

DT$Relative.cover <-  DT$Cover../plot.total.cover$total.cover[index1a]
str(DT)
any(is.na(DT$Relative.cover)) # T
hist(DT$Relative.cover)
na.cases <- DT[,list(na.cases=any(is.na(Cover.code))), by=PlotObservationID]
str(na.cases)
table(na.cases$na.cases)
'
FALSE    TRUE 
1107234   10706'
head(na.cases[na.cases==T,],100)
dim(DT[is.na(Relative.cover)]) # 136479     13

na.cases2 <- DT[,list(na.species=any(is.na(species))), by=PlotObservationID]
str(na.cases2)
table(na.cases2$na.species,na.cases$na.cases)
'      
FALSE    TRUE
FALSE 1090935   10663
TRUE    16299      43'
# most NAs either in cover values or species

na.cases3 <- DT[,list(x=any(Cover.code=="x"|Cover.code=="x_BA"|Cover.code=="x_IC"|
                              Cover.code=="x_IV"|Cover.code=="x_PF"|Cover.code=="x_SC"|
                              is.na(Cover.code))), by=PlotObservationID]
# this includes "x" -> p/a as well as different types of relative abundance information
str(na.cases3)
table(na.cases3$x)
' FALSE    TRUE 
1061602   56338  '

index2 <- match(DT$PlotObservationID,na.cases3$PlotObservationID)
length(index2) #24241941
any(is.na(index2)) # F

na.cases4 <- DT[,list(x=all(Cover.code=="x"|Cover.code=="x_BA"|Cover.code=="x_IC"|
                              Cover.code=="x_IV"|Cover.code=="x_PF"|Cover.code=="x_SC"|
                              is.na(Cover.code))), by=PlotObservationID]
str(na.cases4)
table(na.cases4$x)
' FALSE    TRUE 
1070436   47504  '
# this matches the number of plots with sum of Cover..==0

plot.total.cover2 <- DT[na.cases3$x[index2]==T, list(total.cover=sum(x_,na.rm=T)), 
                        by=PlotObservationID]
any(is.na(DT$x_)) # T
length(DT$PlotObservationID[is.na(DT$x_)]) #  23915391
# 24241941-23915391= 326550 lines with information in the x_ column
str(plot.total.cover2) #56338 obs. of  2 variables
hist(plot.total.cover2$total.cover)
length(plot.total.cover2$total.cover)
# 56338
any(is.na(plot.total.cover2$total.cover)) # F


index3 <- match(plot.total.cover2$PlotObservationID,plot.total.cover$PlotObservationID)
length(index3) #56338
any(is.na(index3)) # F
identical(plot.total.cover2$PlotObservationID,plot.total.cover$PlotObservationID[index3])
# T

table(plot.total.cover2$total.cover>0,plot.total.cover$total.cover[index3]>0)
' 
FALSE  TRUE
FALSE 31459  8674
TRUE  16045   160
# 47504-16045=31459 -> p/a cases for all species in a plot
# all species are assigned 1/species richness as a value for Cover..
16045  -> get value from x_ for all species in a plot
# Cover.. <- x_, but care with assigning NA values
8834 (8674+160) are mixed cover and p/a cases
8674 -> low Cover..=0.01 is assigned
160 -> rescale all values with x_ to 1 and assign that to Cover..
# can be handled together with 11162!
'

index3a <- match(DT$PlotObservationID,plot.total.cover2$PlotObservationID
                 [!plot.total.cover2$total.cover>0 & !plot.total.cover$total.cover[index3]>0])
length(index3a) #   24241941
length(DT$PlotObservationID[!is.na(index3a)])
# 555792
length(unique(DT$PlotObservationID[!is.na(index3a)]))
# 31459
index3a1 <- match(DT$PlotObservationID[!is.na(index3a)],plot.species.number$PlotObservationID)
length(index3a1) #  555792
length(1/plot.species.number$species.number[index3a1])
identical(DT$PlotObservationID[!is.na(index3a)],plot.species.number$PlotObservationID[index3a1])
# T
DT$Cover..[!is.na(index3a)] <- 1/plot.species.number$species.number[index3a1]
# p/a cases for all species in a plot
# all species are assigned 1/species richness as a value for Cover..

index3b <- match(DT$PlotObservationID,plot.total.cover2$PlotObservationID
                 [plot.total.cover2$total.cover>0 & !plot.total.cover$total.cover[index3]>0])
length(index3b) #    24241941
length(DT$PlotObservationID[!is.na(index3b)])
#  355182
length(unique(DT$PlotObservationID[!is.na(index3b)]))
#  16045
index3b1 <- match(DT$PlotObservationID[!is.na(index3b)],plot.total.cover2$PlotObservationID)
length(index3b1) # 355182
identical(DT$PlotObservationID[!is.na(index3b)],plot.total.cover2$PlotObservationID[index3b1])
# T
any(is.na(plot.total.cover2$total.cover[index3b1])) #F
any(is.na(DT$x_[!is.na(index3b)])) #T
length(DT$x_[!is.na(index3b)][!is.na(DT$x_[!is.na(index3b)])]) #  324922
length(DT$x_[!is.na(index3b)][is.na(DT$x_[!is.na(index3b)])]) # 30260
# -> handle !is.na and is.na separately
# !is.na:
DT$Cover..[!is.na(index3b)][!is.na(DT$x_[!is.na(index3b)])] <- 
  DT$x_[!is.na(index3b)][!is.na(DT$x_[!is.na(index3b)])]
# is.na:
'index3b2 <- match(DT$PlotObservationID[!is.na(index3b)]
[is.na(DT$x_[!is.na(index3b)])],DT$PlotObservationID)
# does not work
length(index3b2) # 1939'
plot.species.number2 <- DT[!is.na(index3b)][is.na(DT$x_[!is.na(index3b)])][,list(species.number=length(Cover..)), by=PlotObservationID]
str(plot.species.number2) # 1944
hist(plot.species.number2$species.number)
min(plot.species.number2$species.number) # 1
max(plot.species.number2$species.number) # 612
which(plot.species.number2$species.number==max(plot.species.number2$species.number))
# 1879
plot.species.number2$PlotObservationID[which(plot.species.number2$species.number==max(plot.species.number2$species.number))]
# 51071
#length(DT$Cover..[index3b2]) # 7505
length(DT$Cover..[!is.na(index3b)][is.na(DT$x_[!is.na(index3b)])]) # 30260
index3b3 <- match(DT$PlotObservationID[!is.na(index3b)][is.na(DT$x_[!is.na(index3b)])],
                  plot.species.number2$PlotObservationID)
length(index3b3) #30260
DT$Cover..[!is.na(index3b)][is.na(DT$x_[!is.na(index3b)])] <- 
  1/plot.species.number2$species.number[index3b3]
# p/a cases for all those species in a plot that do not have x_ values
# all those species are assigned 1/species richness as a value for Cover..


index3c <- match(DT$PlotObservationID,plot.total.cover2$PlotObservationID
                 [!plot.total.cover2$total.cover>0 & plot.total.cover$total.cover[index3]>0])
length(index3c) #    24241941
length(DT$PlotObservationID[!is.na(index3c)])
# 115351
length(unique(DT$PlotObservationID[!is.na(index3c)]))
# 8674
# all non-zero cases:
index3c2 <- which(DT$Cover..[!is.na(index3c)]>0)
length(index3c2) #   92103
length(DT$PlotObservationID[!is.na(index3c)][index3c2]) #  92103
min(DT$Cover..[!is.na(index3c)][index3c2]) # 0.01
hist(DT$Cover..[!is.na(index3c)][index3c2]) 
length(DT$Cover..[!is.na(index3c)][index3c2]) #  92103
plot.total.cover3 <- DT[!is.na(index3c)][index3c2][,list(min.cover=min(Cover..)), by=PlotObservationID]
str(plot.total.cover3) # 8674
hist(plot.total.cover3$min.cover)
min(plot.total.cover3$min.cover) # 0.01
max(plot.total.cover3$min.cover) # 100
# all zero cases:
index3c1 <- which(!DT$Cover..[!is.na(index3c)]>0)
length(index3c1) #  23248
length(DT$PlotObservationID[!is.na(index3c)][index3c1]) # 23248
'index3c11 <- match(DT$PlotObservationID[!is.na(index3c)][index3c1],DT$PlotObservationID)
length(index3c11) # 26111
does not work, does mess up the sequence'
plot.species.number3 <- DT[!is.na(index3c)][index3c1][,list(species.number=length(Cover..)), by=PlotObservationID]
str(plot.species.number3) # 8535
hist(plot.species.number3$species.number)
min(plot.species.number3$species.number) # 1
max(plot.species.number3$species.number) # 52
index3c11 <- match(DT$PlotObservationID[!is.na(index3c)][index3c1],
                   plot.total.cover3$PlotObservationID)
length(index3c11) # 23248
DT$Cover..[!is.na(index3c)][index3c1] <- plot.total.cover3$min.cover[index3c11]
# the smallest cover value is assigned that occurred in the plot

index3d <- match(DT$PlotObservationID,plot.total.cover2$PlotObservationID
                 [plot.total.cover2$total.cover>0 & plot.total.cover$total.cover[index3]>0])
length(index3d) #   24241941
length(DT$PlotObservationID[!is.na(index3d)])
# 6472
length(unique(DT$PlotObservationID[!is.na(index3d)]))
# 160
index3d1 <- match(DT$PlotObservationID[!is.na(index3d)],plot.total.cover2$PlotObservationID)
length(index3d1) # 6472
identical(DT$PlotObservationID[!is.na(index3d)],plot.total.cover2$PlotObservationID[index3d1])
# T
any(is.na(plot.total.cover2$total.cover[index3d1])) #F
any(is.na(DT$x_[!is.na(index3d)])) #T
length(DT$x_[!is.na(index3d)][!is.na(DT$x_[!is.na(index3d)])]) #  1628
length(DT$x_[!is.na(index3d)][is.na(DT$x_[!is.na(index3d)])]) # 4844
# -> handle !is.na and is.na separately
# !is.na:
DT$Cover..[!is.na(index3d)][!is.na(DT$x_[!is.na(index3d)])] <- 
  DT$x_[!is.na(index3d)][!is.na(DT$x_[!is.na(index3d)])]
# is.na:
index3d2 <- match(DT$PlotObservationID[!is.na(index3d)]
                  [is.na(DT$x_[!is.na(index3d)])],DT$PlotObservationID)
length(index3d2) # 4844
plot.species.number4 <- DT[index3d2,list(species.number=length(Cover..)), by=PlotObservationID]
str(plot.species.number4) # 160
hist(plot.species.number4$species.number)
min(plot.species.number4$species.number) # 10
max(plot.species.number4$species.number) # 73
length(DT$Cover..[index3d2]) # 4844
index3d3 <- match(DT$PlotObservationID[index3d2],plot.species.number4$PlotObservationID)
length(index3d3) #4844
DT$Cover..[index3d2] <- 1/plot.species.number4$species.number[index3d3]
# p/a cases for all those species in a plot that do not have x_ values
# all those species are assigned 1/species richness as a value for Cover..

### Test for Cover.. ###
hist(DT$Cover..)
any(is.na(DT$Cover..)) #F
#### VERY GOOD !!!! ####
any(DT$Cover..==0) #F
#### VERY GOOD !!!! ####
length(DT$Cover..[DT$Cover..==0]) # 0

min(DT$Cover..) # 0.001
max(DT$Cover..) # 104692
# check
which(DT$Cover..==max(DT$Cover..)) # 1980770
DT[1980770,]
length(DT$PlotObservationID[DT$Cover..==0]) # 0

plot.total.cover4 <- DT[, list(total.cover=sum(Cover..)), by=PlotObservationID]
str(plot.total.cover4) #1117940 
min(plot.total.cover4$total.cover) #0.001
max(plot.total.cover4$total.cover) #104704.1
any(is.na(plot.total.cover4$total.cover)) # F
length(plot.total.cover4$total.cover[plot.total.cover4$total.cover==0]) 
# 0 !!!
# all plots have cover now

index4 <- match(DT$PlotObservationID,plot.total.cover4$PlotObservationID)
length(index4) #24241941
any(is.na(index4)) # F

# recalculate Relative cover
### CAREFUL! This has to be done by layer now ###
DT$Relative.cover <-  DT$Cover../plot.total.cover4$total.cover[index4]
str(DT)
any(is.na(DT$Relative.cover)) # F
hist(DT$Relative.cover)
# between 0 and 1
#### VERY GOOD !!!! ####

write.csv(DT,file = paste("/home/oliver/shared/", 
                          "plots_without_unified_cover_scales_20160120a.csv", sep = ""), row.names = FALSE)


