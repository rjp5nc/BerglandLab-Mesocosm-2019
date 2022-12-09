### libraries
library(data.table)
library(ggplot2)
library(foreach)
library(lattice)
library(tidyr)
library(tidyverse)
library(gridExtra)
library(dplyr)
library(reshape2)
library(patchwork)
library(knitr)
library(lme4)
library(cowplot)
library(ggsignif)

#Read in all csv files.

#List of all hatchlings
hatch <- fread("Hatch20191115.csv")
# Added Vern- date into hatch file 20191115

# All of the eggs/epp put into 96 well plates
eggs <- fread("Eggs20190928.csv")

#When the plates were underwent cold shock
Vern_Dates <- fread("Vern_Dates.csv")

#Combined in how many days after vern did individuals emerge
DaysafterVern <- fread("DaysfromVern.csv")

#fixed a few recording issues in m.
mfixed20191202 <- fread("mfixed20191202.csv")

#Long format
firstfiveweekslong <- read.csv("First5weekslong.csv")

#Long format
HatchlingParseLong <- fread("HatchlingParseLongNoUnScored.csv")

#CSV with survival over the first five weeks w/ remarks. 
Firstfive <- read.csv("First5weeksMomdeadremark.csv")
names(Firstfive)[1] <- "Clone"

eggs2<- eggs
eggs2$one <- 1
eppsclones <- eggs2[, lapply(.SD, sum, na.rm=TRUE), by=c("Clone"), .SDcols=c("one")]

#Replace mistakes
hatch$Clone <- str_replace(hatch$Clone, "AXB", "AxB")
hatch$FloatSink <- str_replace(hatch$FloatSink, "SInk", "Sink")
hatch$FloatSink <- str_replace(hatch$FloatSink, "Float ", "Float")

hatchagdaystohatch <- hatch[, .N, by=list(Clone, DaystoHatch, Vernalized)]
hatchagdaystohatchsubset<- subset(hatchagdaystohatch, Clone=="D8515" | Clone=="D8222")

magfixed <- mfixed20191202[, lapply(.SD, sum, na.rm=TRUE), by=c("Clone", "SampleWeek", "Vernalized"), .SDcols=c("NHatch", "TotalEmb")]
magfixed$prophatch <- magfixed$NHatch/magfixed$TotalEmb

magfixedb <- subset(magfixed, Clone=="D8515" | Clone=="D8222")

magfixedbvern <- magfixedb[!is.na(Vernalized),]
magfixedbvernfew <- magfixedbvern[,list(NHatch, TotalEmb),  list(Clone, Vernalized)]
magfixedbnovern <- magfixedbvernfew[, lapply(.SD, sum, na.rm=TRUE), by=c("Clone", "Vernalized"), .SDcols=c("NHatch", "TotalEmb")]
magfixedbvernfew$prophatch <- magfixedbvernfew$NHatch/magfixedbvernfew$TotalEmb

### Plot with fixed ind, and sum(vern + no vern) in second half

magfixedbnovern <- magfixedbvern[, lapply(.SD, sum, na.rm=TRUE), by=c("Clone", "SampleWeek"), .SDcols=c("NHatch", "TotalEmb")]
magfixedbnovern$SampleWeek <- magfixedbnovern$SampleWeek
magfixedbnovern$TotalEmb <- magfixedbnovern$TotalEmb/2
magfixedbnovern$prophatch <- magfixedbnovern$NHatch/magfixedbnovern$TotalEmb

magfixedprevernb<-magfixedbvern[!(magfixedbvern$Vernalized=="1"),]

magfixedcumul <- rbind(magfixedbnovern,magfixedprevernb, fill=TRUE)
magfixedcumul[is.na(Vernalized),Vernalized:=1]
magfixedcumul2 <- magfixedcumul

magfixedcumul2$Vernalized <- sub("1", "Vernalized", magfixedcumul2$Vernalized)
magfixedcumul2$Vernalized <- sub("0", "Non-Vernalized", magfixedcumul2$Vernalized)

# total hatching

## Mark which ones that had only 1 hatch out, had another embryo in their well
hatchsibswells <- hatch[, .N, by=list(Clone, Plate, Well, EppHatch, Replicate, ID, HatchDate, DaystoHatch, Vernalized)]
eggswells <- eggs[, .N, by=list(Clone, Plate, Well, NumHatch, NumLeft, TotalEmb)]

setDT(hatchsibswells)
setDT(eggswells)
setkey(eggswells, Clone, Plate, Well)
setkey(hatchsibswells, Clone, Plate, Well)
hatchsibseggs <- merge(eggswells, hatchsibswells)

Dissected <- subset(hatchsibseggs, EppHatch=="Hatch")
Dissectednums <- Dissected[, .N, by=list(Clone, EppHatch, Vernalized)]

Nondissected <-subset(hatchsibseggs, EppHatch=="Epp")
Nondissectednums <- Nondissected[, .N, by=list(Clone, EppHatch, Vernalized, NumHatch)]

hatchsibseggs<-separate(hatchsibseggs, ID, into= c("ID", "IDRep"))
##For Dissected?

hatchsibseggsA <- subset(hatchsibseggs, IDRep=="A")
hatchsibseggsB <- subset(hatchsibseggs, IDRep=="B")
hatchsibseggsAb <- subset(hatchsibseggsA, EppHatch=="Hatch")
hatchsibseggsAc <- subset(hatchsibseggsAb, NumLeft=="0")
hatchsibseggsAs <- subset(hatchsibseggsAc, NumHatch=="1")
hatchsibseggsBb <- subset(hatchsibseggsB, EppHatch=="Hatch")
hatchsibseggsBc <- subset(hatchsibseggsBb, NumLeft=="0")
hatchsibseggsBs <- subset(hatchsibseggsBc, NumHatch=="1")
setDT(hatchsibseggsAs)
setDT(hatchsibseggsBs)
setkey(hatchsibseggsAs, Plate, Clone, EppHatch,Replicate,ID)
setkey(hatchsibseggsBs, Plate, Clone, EppHatch,Replicate,ID)
hatchsibseggsmerges <- merge(hatchsibseggsAs, hatchsibseggsBs, all.x=TRUE, all.y=TRUE)

hatchsibseggsmerges$Diff <- hatchsibseggsmerges$DaystoHatch.y-hatchsibseggsmerges$DaystoHatch.x
hatchsibseggsmerges$Verndiff <- hatchsibseggsmerges$Vernalized.y+hatchsibseggsmerges$Vernalized.x

hatchsibseggsmerges$Diff <- abs(hatchsibseggsmerges$Diff)
hatchsibseggsmergesvern <- hatchsibseggsmerges[order(Diff, Verndiff),] 
hatchsibseggsmergesvern <- cbind(hatchsibseggsmergesvern, "observation3"=1:nrow(hatchsibseggsmergesvern)) 
hatchsibseggsmergesvern[is.na(Diff),Diff:=-100]

hatchsibseggsmergesvern[is.na(Verndiff),Verndiff:=1000]

#plot for hatching 
hatch2eggs <- subset(hatchsibseggsmergesvern, Verndiff==0|Verndiff==1|Verndiff==2)
hatch2eggs$Verndiff <- str_replace(hatch2eggs$Verndiff, "0", "Both Early")
hatch2eggs$Verndiff <- str_replace(hatch2eggs$Verndiff, "1", "One Early, One Late")
hatch2eggs$Verndiff <- str_replace(hatch2eggs$Verndiff, "2", "Both Later")

hatch2eggssimp <- hatch2eggs[, .N, by=list(Clone, Verndiff)]

hatch2eggssimp515 <- subset(hatch2eggssimp, Clone=="D8515")
hatch2eggssimp515$Total <- sum(hatch2eggssimp515$N)
hatch2eggssimp222 <- subset(hatch2eggssimp, Clone=="D8222")
hatch2eggssimp222$Total <- sum(hatch2eggssimp222$N)

hatch2eggssimpall <- rbind(hatch2eggssimp515,hatch2eggssimp222)

hatch2eggssimpall$prop <- hatch2eggssimpall$N/hatch2eggssimpall$Total

setkey(hatchsibseggsA, Clone, Plate, Well,EppHatch,Replicate,ID)
setkey(hatchsibseggsB, Clone, Plate, Well,EppHatch,Replicate,ID)
hatchsibseggsmerge <- merge(hatchsibseggsA, hatchsibseggsB, all.x=TRUE, all.y=TRUE)

hatchsibseggsmerge$Diff <- hatchsibseggsmerge$DaystoHatch.y-hatchsibseggsmerge$DaystoHatch.x
hatchsibseggsmerge$Verndiff <- hatchsibseggsmerge$Vernalized.y+hatchsibseggsmerge$Vernalized.x

hatchsibseggsmerge2emb <- subset(hatchsibseggsmerge, TotalEmb.x == 2)
hatchsibseggsmerge2embEpp <- subset(hatchsibseggsmerge2emb, EppHatch == "Epp")
hatchsibseggsmerge2embEppvern <- hatchsibseggsmerge2embEpp[order(Diff, Verndiff),] 
hatchsibseggsmerge2embEppvern <- cbind(hatchsibseggsmerge2embEppvern, "observation3"=1:nrow(hatchsibseggsmerge2embEppvern)) 

hatchsibseggsmerge2embEppvernmin <- hatchsibseggsmerge2embEppvern[, .N, by=list(Clone, EppHatch, Replicate, ID, IDRep.x, observation3, Diff, Verndiff, observation3)]

hatchsibseggsmerge2embEppvernmin[is.na(Verndiff),Verndiff:=-1]

d2 <- hatchsibseggsmerge2embEppvern#[is.na(DaystoHatch.x),DaystoHatch.x:=-100]
e <- subset(d2, d2$Daysaftervern.y > 0)
e <- subset(d2, d2$DaystoHatch.x > 0)
e <- e[order(Clone, Diff, Verndiff),] 
e2 <- cbind(e, "observation4"=1:nrow(e)) 
e2[is.na(Diff),Diff:=-99]
e2[is.na(Verndiff),Verndiff:=-99]

#Replace Replicate Here
e2 <- e2[, .N, by=list(Clone, Verndiff)]

e3 <- e[, .N, by=list(Clone)]

evern1 <- subset(e2, e2$Verndiff == 1)
evern2 <- subset(e2, e2$Verndiff == 2)
evern0 <- subset(e2, e2$Verndiff == 0)

setkey(e3, Clone)
setkey(evern1, Clone)
x21 <- merge(e3, evern1)
x21$prop <- x21$N.y/x21$N.x

setkey(e3, Clone)
setkey(evern2, Clone)
x31 <- merge(e3, evern2)
x31$prop <- x31$N.y/x31$N.x

setkey(e3, Clone)
setkey(evern0, Clone)
x41 <- merge(e3, evern0)
x41$prop <- x41$N.y/x41$N.x

twoepp <- rbind(x21,x31,x41)
twoepp515 <- subset(twoepp, Clone=="D8515")
twoepp515$N.x <- sum(twoepp515$N.y)
twoepp222 <- subset(twoepp, Clone=="D8222")
twoepp222$N.x <- sum(twoepp222$N.y)

twoeppnoA <- rbind(twoepp515,twoepp222)
twoeppnoA$prop <- twoeppnoA$N.y/twoeppnoA$N.x

twoeppnoA<- twoeppnoA %>%
  rename(Totalhatch=N.x,hatched =N.y)
twoeppnoA$Verndiff <- str_replace(twoeppnoA$Verndiff, "0", "Both Early")
twoeppnoA$Verndiff <- str_replace(twoeppnoA$Verndiff, "1", "One Early, One Late")
twoeppnoA$Verndiff <- str_replace(twoeppnoA$Verndiff, "2", "Both Later")

twoeppnoA
hatch2eggssimpall
#For Chi2
# Total number of individuals that 2 embryos hatched out from C clones
# In Ephippia: twoeppnoA
# Dissected Siblings: hatch2eggssimpall


HatchlingParseLong[is.na(Reproduced),Reproduced:=0]

HatchlingParseLongRepro <- HatchlingParseLong[, lapply(.SD, sum, na.rm=TRUE), by=c("Week", "Clone"), .SDcols="Reproduced"]


HatchlingParseLongSibs <- HatchlingParseLong[, lapply(.SD, sum, na.rm=TRUE), by=c("EppHatch", "ID", "DaystoHatch", "Clone"), .SDcols="Reproduced"]

# HatchlingParseLong222222<-HatchlingParseLong
# HatchlingParseLong222222$one <-1
# HatchlingParseLong2222222<- HatchlingParseLong222222[, lapply(.SD, sum, na.rm=TRUE), by=c("Clone", "UnqiueID"), .SDcols=c("one")]
# HatchlingParseLong2222222 <- subset(HatchlingParseLong2222222, Clone== "D8515" | Clone=="D8222")
# 

#Pre and Post vern hatching between Epp and hatching chi^2
hatchsibseggsmerge2embEppvernmin231 <- hatchsibseggsmerge2embEppvernmin[, lapply(.SD, sum, na.rm=TRUE), by=c("Verndiff"), .SDcols="N"]
hatchsibseggsmergesvern$N <- 1
hatchsibseggsmergesvern231 <- hatchsibseggsmergesvern[, lapply(.SD, sum, na.rm=TRUE), by=c("Verndiff"), .SDcols="N"]
hatchsibseggsmergesvern231

#Back to other dataset

HatchlingParseLongEppmales <- HatchlingParseLong[, lapply(.SD, sum, na.rm=TRUE), by=c("Clone", "Week", "EmptyEpp", "1EggEpp", "2EggEpp", "Males"), .SDcols="Reproduced"]

names(HatchlingParseLongEppmales)[4] <- "oneEggEpp"
names(HatchlingParseLongEppmales)[5] <- "twoEggEpp"

HatchlingParseLong515 <- subset(HatchlingParseLong, Clone=="D8515")
HatchlingParseLong222 <- subset(HatchlingParseLong, Clone == "D8222")

HatchlingParseLong515$totalepp <- HatchlingParseLong515$EmptyEpp + HatchlingParseLong515$`1EggEpp` + HatchlingParseLong515$`2EggEpp`
HatchlingParseLong222$totalepp <- HatchlingParseLong222$EmptyEpp + HatchlingParseLong222$`1EggEpp` + HatchlingParseLong222$`2EggEpp`

names(HatchlingParseLong222)[names(HatchlingParseLong222) == "1EggEpp"] <- "oneEggEpp"
names(HatchlingParseLong222)[names(HatchlingParseLong222) == "2EggEpp"] <- "twoEggEpp"

names(HatchlingParseLong515)[names(HatchlingParseLong515) == "1EggEpp"] <- "oneEggEpp"
names(HatchlingParseLong515)[names(HatchlingParseLong515) == "2EggEpp"] <- "twoEggEpp"

HatchlingParseLong222$one <- 1
HatchlingParseLong515$one <- 1


HatchlingParseLong222test <- HatchlingParseLong222[, lapply(.SD, sum, na.rm=TRUE), by=c("Clone","ID", "Plate","Replicate","Week"), .SDcols="one"]
HatchlingParseLong222test$Week <- as.character(HatchlingParseLong222test$Week)

HatchlingParseLong515test <- HatchlingParseLong515[, lapply(.SD, sum, na.rm=TRUE), by=c("Clone","ID", "Plate","Replicate","Week"), .SDcols="one"]
HatchlingParseLong515test$Week <- as.character(HatchlingParseLong515test$Week)

summary(HatchlingParseLong222test)
summary(HatchlingParseLong515test)

#Any lineages accidentally been duplicated when sampling.

notone222 <- subset(HatchlingParseLong222test, one == "2")
notone515 <- subset(HatchlingParseLong515test, one == "2")

HatchlingParseLong222$Vernalized
HatchlingParseLong515$Vernalized
HatchlingParseLong222Vern <- HatchlingParseLong222[, lapply(.SD, sum, na.rm=TRUE), by=c("Clone","Vernalized"), .SDcols="one"]
HatchlingParseLong515Vern <- HatchlingParseLong515[, lapply(.SD, sum, na.rm=TRUE), by=c("Clone","Vernalized"), .SDcols="one"]

HatchlingParseLong222fillrate1<- HatchlingParseLong222[, lapply(.SD, sum, na.rm=TRUE), by=c("Clone","Vernalized", "SampleWeek", "EmptyEpp", "oneEggEpp", "twoEggEpp", "one"), .SDcols=c("totalepp")]
HatchlingParseLong222fillrate1$totalembs <- HatchlingParseLong222fillrate1$oneEggEpp+2*HatchlingParseLong222fillrate1$twoEggEpp
HatchlingParseLong222fillrate1$availablespaces <- HatchlingParseLong222fillrate1$totalepp*2
HatchlingParseLong222fillrate1$fillrate <- HatchlingParseLong222fillrate1$totalembs/HatchlingParseLong222fillrate1$availablespaces

names(HatchlingParseLong222fillrate1)[7] <- "NumberofOccurrences"

HatchlingParseLong515fillrate1<- HatchlingParseLong515[, lapply(.SD, sum, na.rm=TRUE), by=c("Clone","Vernalized", "SampleWeek", "EmptyEpp", "oneEggEpp", "twoEggEpp", "one"), .SDcols=c("totalepp")]
HatchlingParseLong515fillrate1$totalembs <- HatchlingParseLong515fillrate1$oneEggEpp+2*HatchlingParseLong515fillrate1$twoEggEpp
HatchlingParseLong515fillrate1$availablespaces <- HatchlingParseLong515fillrate1$totalepp*2
HatchlingParseLong515fillrate1$fillrate <- HatchlingParseLong515fillrate1$totalembs/HatchlingParseLong515fillrate1$availablespaces

names(HatchlingParseLong515fillrate1)[7] <- "NumberofOccurrences"

HatchlingParseLong222fillrate1$averagetest <- mean(HatchlingParseLong222fillrate1$fillrate)
HatchlingParseLong515fillrate1$averagetest <- mean(HatchlingParseLong515fillrate1$fillrate)

HatchlingParseLong222Epps <- HatchlingParseLong222[, lapply(.SD, sum, na.rm=TRUE), by=c("Clone","Vernalized", "SampleWeek"), .SDcols=c("totalepp", "one")]

#Survival over first five weeks


Firstfive <- setDT(Firstfive)

#Fix mistakes
Firstfive$Epp.Hatch <- sub("Hatch ", "Hatch", Firstfive$Epp.Hatch)
Firstfive$X1WeekAlive <- sub("^$", "N", Firstfive$X1WeekAlive)
Firstfive$X2WeekAlive <- sub("^$", "N", Firstfive$X2WeekAlive)
Firstfive$X2WeekAlive <- sub("N ", "N", Firstfive$X2WeekAlive)

Firstfive$X3WeekAlive <- sub("^$", "N", Firstfive$X3WeekAlive)
Firstfive$X3WeekAlive <- sub("N ", "N", Firstfive$X3WeekAlive)
Firstfive$X3WeekAlive <- sub("R ", "R", Firstfive$X3WeekAlive)
Firstfive$X3WeekAlive <- sub("Y ", "Y", Firstfive$X3WeekAlive)

Firstfive$X4WeekAlive <- sub("^$", "N", Firstfive$X4WeekAlive)
Firstfive$X4WeekAlive <- sub("N ", "N", Firstfive$X4WeekAlive)
Firstfive$X4WeekAlive <- sub("R ", "R", Firstfive$X4WeekAlive)
Firstfive$X4WeekAlive <- sub("Y ", "Y", Firstfive$X4WeekAlive)

Firstfive$X5WeekAlive <- sub("^$", "N", Firstfive$X5WeekAlive)
Firstfive$X5WeekAlive <- sub("N ", "N", Firstfive$X5WeekAlive)
Firstfive$X5WeekAlive <- sub("R ", "R", Firstfive$X5WeekAlive)

Firstfive$sum <- 1

Firstfive$Dead[is.na(Firstfive$Dead)] <- 0

FirstfiveCsonly<- subset(Firstfive, Clone=="D8515" | Clone=="D8222")

FirstfiveDead <- Firstfive[, lapply(.SD, sum, na.rm=TRUE), by=c("Clone", "Vernalized", "Dead", "Epp.Hatch"), .SDcols="sum"]
FirstfiveDead <- FirstfiveDead[!is.na(FirstfiveDead$Vernalized)]

FirstfiveDeadtotal <- FirstfiveDead[, lapply(.SD, sum, na.rm=TRUE), by=c("Clone", "Vernalized", "Epp.Hatch"), .SDcols="sum"]

setkey(FirstfiveDeadtotal, Clone, Vernalized, Epp.Hatch)
setkey(FirstfiveDead, Clone, Vernalized, Epp.Hatch)
Firstfivedeadsum <- merge(FirstfiveDead, FirstfiveDeadtotal)
Firstfivedeadsum$propalive <- Firstfivedeadsum$sum.x/Firstfivedeadsum$sum.y

FirstfivedeadsumB <- subset(Firstfivedeadsum, Clone=="D8222"|Clone=="D8515")
FirstfivedeadsumBalive <- subset(FirstfivedeadsumB, Dead=="0")
FirstfivedeadsumBalive2 <- FirstfivedeadsumBalive
FirstfivedeadsumBalive2$Epp.Hatch <- sub("Epp", "Non-Dissected", FirstfivedeadsumBalive2$Epp.Hatch)
FirstfivedeadsumBalive2$Epp.Hatch <- sub("Hatch", "Dissected", FirstfivedeadsumBalive2$Epp.Hatch)
FirstfivedeadsumBalive2$Vernalized <- sub("0", "Early", FirstfivedeadsumBalive2$Vernalized)
FirstfivedeadsumBalive2$Vernalized <- sub("1", "Later", FirstfivedeadsumBalive2$Vernalized)

HatchlingParseLongBothBfillrate1 <- rbind(HatchlingParseLong515fillrate1,HatchlingParseLong222fillrate1)
HatchlingParseLongBothBfillrate1$Vernalized <- sub("0", "Early", HatchlingParseLongBothBfillrate1$Vernalized)
HatchlingParseLongBothBfillrate1$Vernalized <- sub("1", "Later", HatchlingParseLongBothBfillrate1$Vernalized)

twoeppnoA$Epphatch <- "Non-Dissected"
hatch2eggssimpall$Epphatch <- "Dissected"

hatch2eggssimpall2 <- hatch2eggssimpall
names(hatch2eggssimpall2)[4] <- "Totalhatch"
names(hatch2eggssimpall2)[3] <- "hatched"

Prepostverndisectboth <- rbind(twoeppnoA,hatch2eggssimpall2, fill=TRUE)

magfixedcumul22 <- magfixedcumul2[, lapply(.SD, sum, na.rm=TRUE), by=c("Clone", "SampleWeek"), .SDcols="prophatch"]
hatchprevern <- magfixedcumul2[, lapply(.SD, sum, na.rm=TRUE), by=c("Vernalized"), .SDcols="NHatch"]

setkey(magfixedcumul2, Clone, SampleWeek)
setkey(magfixedcumul22, Clone, SampleWeek)
magfixedcumulcombined <- merge(magfixedcumul2, magfixedcumul22)
magfixedcumulcombined$relhatch <- magfixedcumulcombined$prophatch.x/magfixedcumulcombined$prophatch.y

magfixedcumulcombinedpre0 <- subset(magfixedcumulcombined, Vernalized=="Non-Vernalized")
magfixedcumulcombinedpre<- magfixedcumulcombinedpre0[-c(1)]

#Number of Ephippia per jar

HatchlingParseLong
HatchlingParseLong$TotalEpp <- HatchlingParseLong$EmptyEpp + HatchlingParseLong$`1EggEpp`+ HatchlingParseLong$`2EggEpp`
HatchlingParseLongB <- subset(HatchlingParseLong, Clone == "D8515"|Clone == "D8222")
HatchlingParseLongB$Vernalized <- sub("0", "Early", HatchlingParseLongB$Vernalized)
HatchlingParseLongB$Vernalized <- sub("1", "Later", HatchlingParseLongB$Vernalized)
HatchlingParseLongslimB <- HatchlingParseLongB[, lapply(.SD, sum, na.rm=TRUE), by=c("UnqiueID", "Clone", "Vernalized", "EppHatch", "ID"), .SDcols=c("TotalEpp")]
HatchlingParseLonglargeB <- HatchlingParseLongB[, lapply(.SD, sum, na.rm=TRUE), by=c("UnqiueID", "Clone", "Vernalized", "EppHatch", "ID","EmptyEpp", "1EggEpp", "2EggEpp"), .SDcols=c("TotalEpp")]


firstfiveweekslong<- data.table(firstfiveweekslong)
names(firstfiveweekslong)[1] <- "Clone"

#Fix mistakes again
firstfiveweekslong$Alive <- sub("^$", "N", firstfiveweekslong$Alive)
firstfiveweekslong$Alive <- sub("R ", "R", firstfiveweekslong$Alive)
firstfiveweekslong$Alive <- sub("Y ", "Y", firstfiveweekslong$Alive)
firstfiveweekslong$Alive <- sub("Ystr", "Sterile", firstfiveweekslong$Alive)
firstfiveweekslong$Alive <- sub("Ysm str", "Sterile", firstfiveweekslong$Alive)
firstfiveweekslong$Alive <- sub("R_momdead", "N", firstfiveweekslong$Alive)
firstfiveweekslong$Alive <- sub("missing", "N", firstfiveweekslong$Alive)

firstfiveweekslongB <- subset(firstfiveweekslong, Clone=="D8222"|Clone=="D8515")
firstfiveweekslongB$one <-1
firstfiveweekslongB<- data.table(firstfiveweekslongB)
firstfiveweekslongB$Cloneid <- paste(firstfiveweekslongB$Clone,firstfiveweekslongB$Replicate, firstfiveweekslongB$ID)
firstfiveweekslongB2 <- firstfiveweekslongB[, .N, by=list(Clone, Week, Alive, Vernalized, Cloneid)]
firstfiveweekslongB3 <- firstfiveweekslongB[, .N, by=list(Clone, Alive)]
firstfiveweekslongB4 <- subset(firstfiveweekslongB3, firstfiveweekslongB3$N > 6)
ggplot(data=firstfiveweekslongB2, aes(x=Week, y=N, col=Alive))+ geom_col()+ facet_wrap(Clone~Vernalized)+ theme(legend.title = element_blank())

write.csv(firstfiveweekslongB, "C:\\Users\\rjpor\\Desktop\\data.csv")

firstfiveweekslongB3 <- firstfiveweekslongB[, .N, by=list(Clone, Alive)]
firstfiveweekslongB3total <- firstfiveweekslongB[, .N, by=list(Clone)]

firstfiveweekslongB3<- subset(firstfiveweekslongB3, Alive=="R")

setkey(firstfiveweekslongB3, Clone)
setkey(firstfiveweekslongB3total, Clone)
firstfiveweekslongB3merge <- merge(firstfiveweekslongB3, firstfiveweekslongB3total, all.x=TRUE, all.y=TRUE)
firstfiveweekslongB3merge$prop <- firstfiveweekslongB3merge$N.x/firstfiveweekslongB3merge$N.y
firstfiveweekslongB3mergeglm <- glm(prop~Clone, firstfiveweekslongB3merge, family=binomial(), weights=N.y)
anova(firstfiveweekslongB3mergeglm, test = "Chisq")

#Higher rate of reaching reproduction over sample week?
firstfiveweekslongB3 <- firstfiveweekslongB[, .N, by=list(Clone, Alive, Week, Vernalized)]
firstfiveweekslongB3total <- firstfiveweekslongB[, .N, by=list(Clone, Week, Vernalized)]
firstfiveweekslongB3<- subset(firstfiveweekslongB3, Alive=="R")
setkey(firstfiveweekslongB3, Clone, Week, Vernalized)
setkey(firstfiveweekslongB3total, Clone, Week, Vernalized)
firstfiveweekslongB3merge <- merge(firstfiveweekslongB3, firstfiveweekslongB3total, all.x=TRUE, all.y=TRUE)
firstfiveweekslongB3merge$prop <- firstfiveweekslongB3merge$N.x/firstfiveweekslongB3merge$N.y
firstfiveweekslongB3merge[is.na(prop),prop:=0]
firstfiveweekslongB3merge <- na.omit(firstfiveweekslongB3merge)
firstfiveweekslongB3mergeglm <- glm(prop~Clone*as.factor(Week)*Vernalized, firstfiveweekslongB3merge, family=binomial(), weights=N.y)
anova(firstfiveweekslongB3mergeglm, test = "Chisq")

ReproductionB <- predict(firstfiveweekslongB3mergeglm, newdata=data.frame(firstfiveweekslongB3merge), se.fit=T)
ReproductionB.dt <- as.data.table(data.frame(firstfiveweekslongB3merge))
ReproductionB.dt[,pred:=plogis(ReproductionB$fit)]
ReproductionB.dt[,lci:=plogis(ReproductionB$fit-1.96*ReproductionB$se.fit)]
ReproductionB.dt[,uci:=plogis(ReproductionB$fit+1.96*ReproductionB$se.fit)]

ReproductionB.dt$Vernalized <- sub("0", "Early", ReproductionB.dt$Vernalized)
ReproductionB.dt$Vernalized <- sub("1", "Later", ReproductionB.dt$Vernalized)

ReproductionB.dt$Vernalized_f <- factor(ReproductionB.dt$Vernalized, levels=c('Early', 'Later'))
ReproductionB.dt$Vernalized <- sub("1", "Later", ReproductionB.dt$Vernalized)

names(ReproductionB.dt)[3] <- "Condition"

Reproductionplot <- ggplot(data=ReproductionB.dt, aes(x=ReproductionB.dt$Week, y=ReproductionB.dt$pred, group=Clone, col=Clone)) +
  geom_line()+
  geom_point()+ xlim(1,5)+
  geom_errorbar(aes(ymin=uci, ymax=lci), width=.05) + 
  facet_wrap("Vernalized_f") + ylab("Proportion reproduced") + xlab("Weeks after hatching")
Reproductionplot

#Final Graphs and statistics

#Figure 1
prophatchglm <- glm(prop~Verndiff*Clone*Epphatch, Prepostverndisectboth, family=binomial(), weights = Totalhatch)
anything2 <- predict(prophatchglm, newdata=data.frame(Prepostverndisectboth), se.fit=T)
pred.dt2 <- as.data.table(data.frame(Prepostverndisectboth))
pred.dt2[,pred:=plogis(anything2$fit)]
pred.dt2[,lci:=plogis(anything2$fit-1.96*anything2$se.fit)]
pred.dt2[,uci:=plogis(anything2$fit+1.96*anything2$se.fit)]

pred.dt2$Estimated <- c(17.45/80,61.25/80,1.25/80,19.2/40,6.4/40,14.4/40,5.2267/150,99.2267/150,45.5467/150,23.3382/204,89.3382/204,91.3235/204)

pred.dt2$CloneDissection <- paste(pred.dt2$Clone,pred.dt2$Epphatch)


magfixedcumulcombinedglm <- glm((prophatch.x*TotalEmb)/((prophatch.y*TotalEmb))~Clone*as.factor(SampleWeek), magfixedcumulcombinedpre0, family=binomial(), weights=(prophatch.y*TotalEmb))
anova(magfixedcumulcombinedglm, test = "Chisq")

#prophatch.x is proportion hatch pre-vern. Prop.y is proportion hatched prevern + postvern. Relhatch is prop prevern/total hatched

anything3 <- predict(magfixedcumulcombinedglm, newdata=data.frame(magfixedcumulcombinedpre0), se.fit=T)
pred.dt3 <- as.data.table(data.frame(magfixedcumulcombinedpre0))
pred.dt3[,pred:=plogis(anything3$fit)]
pred.dt3[,lci:=plogis(anything3$fit-1.96*anything3$se.fit)]
pred.dt3[,uci:=plogis(anything3$fit+1.96*anything3$se.fit)]
pred.dt3$Vernalized <- sub("Non-Vernalized", "Early", pred.dt3$Vernalized)
names(pred.dt3)[6] <- "Condition"


#Figure 2

FirstfivedeadsumBalive2glm <- glm(propalive~Vernalized*Clone*Epp.Hatch, FirstfivedeadsumBalive2, family=binomial(), weights=sum.y)
predict(FirstfivedeadsumBalive2glm, newdata=FirstfivedeadsumBalive2)
FirstfivedeadsumBalive2predict <- predict(FirstfivedeadsumBalive2glm, Vernalized=FirstfivedeadsumBalive2$Vernalized, Clone=FirstfivedeadsumBalive2$Clone, Epp.Hatch=FirstfivedeadsumBalive2$Epp.Hatch, se.fit=T)
FirstfivedeadsumBalive2pred <- as.data.table(data.frame(Vernalized=FirstfivedeadsumBalive2$Vernalized, Clone=FirstfivedeadsumBalive2$Clone, Epp.Hatch=FirstfivedeadsumBalive2$Epp.Hatch))
FirstfivedeadsumBalive2pred[,pred:=plogis(FirstfivedeadsumBalive2predict$fit)]
FirstfivedeadsumBalive2pred[,lci:=plogis(FirstfivedeadsumBalive2predict$fit-1.96*FirstfivedeadsumBalive2predict$se.fit)]
FirstfivedeadsumBalive2pred[,uci:=plogis(FirstfivedeadsumBalive2predict$fit+1.96*FirstfivedeadsumBalive2predict$se.fit)]

#Figure 3

HatchlingParseLongBothBfillrate1glm <- glm(fillrate~Vernalized*Clone, HatchlingParseLongBothBfillrate1, family=binomial(), weights=availablespaces)
anova(HatchlingParseLongBothBfillrate1glm, test = "Chisq")

HatchlingParseLongBothBfillrate1glmdt <- predict(HatchlingParseLongBothBfillrate1glm, newdata=data.frame(HatchlingParseLongBothBfillrate1), se.fit=T)
pred.dthatchlong <- as.data.table(data.frame(HatchlingParseLongBothBfillrate1))
pred.dthatchlong[,pred:=plogis(HatchlingParseLongBothBfillrate1glmdt$fit)]
pred.dthatchlong[,lci:=plogis(HatchlingParseLongBothBfillrate1glmdt$fit-1.96*HatchlingParseLongBothBfillrate1glmdt$se.fit)]
pred.dthatchlong[,uci:=plogis(HatchlingParseLongBothBfillrate1glmdt$fit+1.96*HatchlingParseLongBothBfillrate1glmdt$se.fit)]

pred.dthatchlong$Vernalized_f <- factor(pred.dthatchlong$Vernalized, levels=c('Early', 'Later'))
HatchlingParseLongBothBfillrate1$Vernalized_f <- factor(HatchlingParseLongBothBfillrate1$Vernalized, levels=c('Early', 'Later'))

Eppplotglm <- glm(TotalEpp~Vernalized*Clone, HatchlingParseLongslimB, family=poisson())

Prepostverndisectbothnoclone <- Prepostverndisectboth[, lapply(.SD, sum, na.rm=TRUE), by=c("Verndiff", "Epphatch"), .SDcols=c("Totalhatch", "hatched")]

Prepostverndisectbothnoclone$prop <- Prepostverndisectbothnoclone$hatched/Prepostverndisectbothnoclone$Totalhatch

prophatchglmnoclone <- glm(prop~Verndiff*Epphatch, Prepostverndisectbothnoclone, family=binomial(), weights = Totalhatch)
anything2noclone <- predict(prophatchglmnoclone, newdata=data.frame(Prepostverndisectbothnoclone), se.fit=T)
pred.dt2noclone <- as.data.table(data.frame(Prepostverndisectbothnoclone))
pred.dt2noclone[,pred:=plogis(anything2noclone$fit)]
pred.dt2noclone[,lci:=plogis(anything2noclone$fit-1.96*anything2noclone$se.fit)]
pred.dt2noclone[,uci:=plogis(anything2noclone$fit+1.96*anything2noclone$se.fit)]


pred.dt2noclone$Estimated = c(2*(249/346)*(97/346), (249/346)^2, (97/346)^2, (26/120)^2, (94/120)^2, 2*(26/120)*(94/120))

hatchsibswells2 <- hatch[, .N, by=list(Clone, FloatSink, Plate, Well, EppHatch, Replicate, ID, HatchDate, DaystoHatch, Vernalized)]

eggswells2 <- eggs[, .N, by=list(Clone, Plate, Well, NumHatch, NumLeft, TotalEmb)]

setDT(hatchsibswells2)
setDT(eggswells2)
setkey(eggswells2, Clone, Plate, Well)
setkey(hatchsibswells2, Clone, Plate, Well)
hatchsibseggs2 <- merge(eggswells2, hatchsibswells2)
hatchsibseggs3 <- subset(hatchsibseggs2, Clone=="D8515" | Clone=="D8222")

hatchsibseggs3floatsinksimp <- hatchsibseggs3[, .N, by=list(Clone, EppHatch, FloatSink)]
hatchsibseggs3floatsinksimp <- hatchsibseggs3[, .N, by=list(FloatSink)]

hatchsibseggs4 <- subset(hatchsibseggs3, FloatSink== "Float" | FloatSink== "Sink")
hatchsibseggs5 <- hatchsibseggs4[, lapply(.SD, sum, na.rm=TRUE), by=c("Clone", "FloatSink", "Vernalized"), .SDcols=c("NumHatch")]