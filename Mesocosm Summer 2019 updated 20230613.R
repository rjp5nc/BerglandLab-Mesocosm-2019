#REVISED
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


Ceggs <- subset(eggs, Clone == "D8515"|Clone == "D8222")

Chatch <- subset(hatch, Clone == "D8515"|Clone == "D8222")

Chatch$Clone <- str_replace(Chatch$Clone, "AXB", "AxB")
Chatch$FloatSink <- str_replace(Chatch$FloatSink, "SInk", "Sink")
Chatch$FloatSink <- str_replace(Chatch$FloatSink, "Float ", "Float")
Chatch$FloatSink <- str_replace(Chatch$FloatSink, "Sink ", "Sink")
Chatch$Plate <- str_replace(Chatch$Plate, "5_29_19KÂ", "5_29_19K")
Chatch$Plate <- str_replace(Chatch$Plate, "5_29_19K ", "5_29_19K")


Chatch$FloatSink[is.na(Chatch$FloatSink)] <- ""

Ceggs$FloatSink <- str_replace(Ceggs$FloatSink, "SinkÂ", "Sink")
Ceggs$FloatSink <- str_replace(Ceggs$FloatSink, "Sink ", "Sink")

Chatch$NumHatch <- "1"
Chatch$NumHatch <- as.numeric(Chatch$NumHatch)
sum(Chatch$NumHatch)

#For Fig 1

hatchsibswells <- Chatch[, .N, by=list(Clone, Plate, Well, EppHatch, Replicate, ID, HatchDate, DaystoHatch, Vernalized)]
eggswells <- Ceggs[, .N, by=list(Clone, Plate, Well, NumHatch, NumLeft, TotalEmb)]

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

hatchsibseggsA <- as.data.table(hatchsibseggsA)
hatchsibseggsB <- as.data.table(hatchsibseggsB)

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

twoeppnoA$Epphatch <- "Non-Dissected"
hatch2eggssimpall$Epphatch <- "Dissected"

hatch2eggssimpall2 <- hatch2eggssimpall
names(hatch2eggssimpall2)[4] <- "Totalhatch"
names(hatch2eggssimpall2)[3] <- "hatched"

Prepostverndisectboth <- rbind(twoeppnoA,hatch2eggssimpall2, fill=TRUE)

Prepostverndisectbothnoclone <- Prepostverndisectboth[, lapply(.SD, sum, na.rm=TRUE), by=c("Verndiff", "Epphatch"), .SDcols=c("Totalhatch", "hatched")]

Prepostverndisectbothnoclone$prop <- Prepostverndisectbothnoclone$hatched/Prepostverndisectbothnoclone$Totalhatch

Prepostverndisectbothnoclone$se <- sqrt(Prepostverndisectbothnoclone$prop * (1-Prepostverndisectbothnoclone$prop)/ (Prepostverndisectbothnoclone$Totalhatch))
Prepostverndisectbothnoclone$lci <- Prepostverndisectbothnoclone$prop - (1.96 * Prepostverndisectbothnoclone$se)
Prepostverndisectbothnoclone$uci <- Prepostverndisectbothnoclone$prop + (1.96 * Prepostverndisectbothnoclone$se)

Prepostverndisectbothnoclone$Estimated = c(2*(249/346)*(97/346), (249/346)^2, (97/346)^2, (26/120)^2, (94/120)^2, 2*(26/120)*(94/120))


zone <-c("Non-Dissected","Dissected")
color.codes<-as.character(c("#a6611a", "#33a02c"))
color.names<-c("Brown", "Green")

Fig1A <- ggplot(data=Prepostverndisectbothnoclone, aes(x=Verndiff, y=prop,col=Epphatch)) +
  geom_point(aes(col=Epphatch), position = position_dodge(width = 0.2), size = 2) +
  geom_point(aes(y=Estimated, col=Epphatch), position = position_dodge(width = 0.2), size = 2, shape = 1) +
  geom_errorbar(aes(ymin=uci, ymax=lci), width=.1, position = position_dodge(width = 0.2)) +
  ylim(0,1)+ 
  ylab("Proportion Hatched")+ 
  xlab("Hatching Time") +
  scale_x_discrete(limit= c("Both Early", "One Early, One Late", "Both Later"),
                   labels= c("Both Early"="EE", "One Early, One Late"= "EL", "Both Later"="LL"))+
  theme(text=element_text(size=12), axis.title=element_text(size=12))+
  scale_colour_manual(values=setNames(color.codes, zone))+ theme_bw()


#Model 1

#Proportion early hatching ~ Clone * Mesocosm Age + Rep
# Clone, Plate, SampleWeek Vernalized NHatch TotalEmb  prophatch

fewchatchnosampweek <- Chatch[, lapply(.SD, sum), by=c("Clone","Replicate","FloatSink","EppHatch"), .SDcols=c("NumHatch")]


#fewchatch <- Chatch[, lapply(.SD, sum), by=c("Clone","Replicate","FloatSink","EppHatch","SampleWeek","Vernalized"), .SDcols=c("NumHatch")]
fewceggs <- Ceggs[, lapply(.SD, sum), by=c("Clone", "Replicate", "EppHatch","FloatSink"), .SDcols=c("TotalEmb")]

fewchatch <- Chatch[, lapply(.SD, sum), by=c("Clone","Replicate","SampleWeek","Vernalized"), .SDcols=c("NumHatch")]
fewchatchnovern <- Chatch[, lapply(.SD, sum), by=c("Clone","Replicate","SampleWeek"), .SDcols=c("NumHatch")]

setkey(fewchatch, Clone, Replicate, SampleWeek)
setkey(fewchatchnovern, Clone, Replicate, SampleWeek)
fewchatchplustotals <- merge(fewchatch, fewchatchnovern, all=T)
fewchatchplustotals$prophatchearly <- fewchatchplustotals$NumHatch.x/fewchatchplustotals$NumHatch.y
fewchatchplustotals <- subset(fewchatchplustotals, Vernalized== "0")

fewchatchplustotals$se <- sqrt(fewchatchplustotals$prophatchearly * (1-fewchatchplustotals$prophatchearly)/ (fewchatchplustotals$NumHatch.y))
fewchatchplustotals$lci <- fewchatchplustotals$prophatchearly - (1.96 * fewchatchplustotals$se)
fewchatchplustotals$uci <- fewchatchplustotals$prophatchearly + (1.96 * fewchatchplustotals$se)

Fig1B <- ggplot(data=fewchatchplustotals, aes(x=fewchatchplustotals$SampleWeek, y=fewchatchplustotals$prophatchearly, group=Replicate, col=Replicate)) +
  geom_line()+
  geom_point(size=2)+ 
  ylab("Proportion Hatched Early") + xlab("Sample Week of Mesocosm")+ 
  ylim(0,1)+ facet_wrap("Clone")+  geom_errorbar(aes(ymin=uci, ymax=lci), width=.1) +
  theme(text=element_text(size=20))+ theme_bw()
Fig1B

names(fewchatchplustotals)[5] <- "NumHatch_early"
names(fewchatchplustotals)[6] <- "NumHatch_total"


fewchatchplustotalsglm <- glm(prophatchearly~SampleWeek+Clone/Replicate, 
                           fewchatchplustotals, family=binomial(),
                                weights=(NumHatch_total))

anova(fewchatchplustotalsglm, test = "Chisq")


#Model 2

names(Firstfive)[1] <- "Clone"
Firstfive <- setDT(Firstfive)
Firstfive$sum <- 1
Firstfive$Dead[is.na(Firstfive$Dead)] <- 0

FirstfiveDead <- Firstfive[, lapply(.SD, sum, na.rm=TRUE), by=c("Clone", "Vernalized", "Dead", "Epp.Hatch", "Replicate"), .SDcols="sum"]
FirstfiveDead <- FirstfiveDead[!is.na(FirstfiveDead$Vernalized)]

FirstfiveDeadtotal <- FirstfiveDead[, lapply(.SD, sum, na.rm=TRUE), by=c("Clone", "Vernalized", "Epp.Hatch", "Replicate"), .SDcols="sum"]

setkey(FirstfiveDeadtotal, Clone, Vernalized, Epp.Hatch, Replicate)
setkey(FirstfiveDead, Clone, Vernalized, Epp.Hatch, Replicate)
Firstfivedeadsum <- merge(FirstfiveDead, FirstfiveDeadtotal)
Firstfivedeadsum$propalive <- Firstfivedeadsum$sum.x/Firstfivedeadsum$sum.y

FirstfivedeadsumB <- subset(Firstfivedeadsum, Clone=="D8222"|Clone=="D8515")

FirstfivedeadsumBalive <- subset(FirstfivedeadsumB, Dead=="0")
FirstfivedeadsumBalive2 <- FirstfivedeadsumBalive
FirstfivedeadsumBalive2$Epp.Hatch <- sub("Epp", "Non-Dissected", FirstfivedeadsumBalive2$Epp.Hatch)
FirstfivedeadsumBalive2$Epp.Hatch <- sub("Hatch", "Dissected", FirstfivedeadsumBalive2$Epp.Hatch)
FirstfivedeadsumBalive2$Vernalized <- sub("0", "Early", FirstfivedeadsumBalive2$Vernalized)
FirstfivedeadsumBalive2$Vernalized <- sub("1", "Later", FirstfivedeadsumBalive2$Vernalized)



FirstfivedeadsumBalive2$se <- sqrt(FirstfivedeadsumBalive2$propalive * (1-FirstfivedeadsumBalive2$propalive)/ (FirstfivedeadsumBalive2$sum.y))
FirstfivedeadsumBalive2$lci <- FirstfivedeadsumBalive2$propalive - (1.96 * FirstfivedeadsumBalive2$se)
FirstfivedeadsumBalive2$uci <- FirstfivedeadsumBalive2$propalive + (1.96 * FirstfivedeadsumBalive2$se)


Fig2A <- ggplot(data=FirstfivedeadsumBalive2, aes(x=Vernalized, y=propalive, col=Epp.Hatch, shape = Clone))+ 
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  #facet_wrap("Vernalized") + 
  xlab("Hatching time") + 
  ylab("Proportion Survived to Reproduction")+ 
  ylim(0,1)+ facet_wrap(Clone~Replicate)+
  geom_errorbar(aes(ymin=uci, ymax=lci), width=.05, position = position_dodge(width = 0.3))+
  scale_colour_manual(values=setNames(color.codes, zone))+ theme_bw()

FirstfivedeadsumBalive2glm <- glm(propalive~Vernalized*Epp.Hatch+Clone/Replicate, FirstfivedeadsumBalive2, family=binomial(), weights=sum.y)
anova(FirstfivedeadsumBalive2glm, test = "Chisq")

sum(FirstfivedeadsumBalive2$sum.y)

#Model 3

D8222R1 <- subset(firstfiveweekslongB3merge, Clone== "D8222" & Replicate =="R1")
D8222R2 <- subset(firstfiveweekslongB3merge, Clone== "D8222" & Replicate =="R2")
D8515R1 <- subset(firstfiveweekslongB3merge, Clone== "D8515" & Replicate =="R1")
D8515R2 <- subset(firstfiveweekslongB3merge, Clone== "D8515" & Replicate =="R2")


#D8222 R1
D8222R1<- c(0,0,2,1, 46, 90, 35, 15, 12, 0)
D8222R1 <- as.data.table(D8222R1)
names(D8222R1) [1] <- "N"
D8222R1$week <- c(1,1,2,2,3,3,4,4,5,5)
D8222R1$vernalized <- c("Early","Late","Early","Late","Early","Late","Early","Late","Early","Late")
D8222R1$clone <- "D8222"
D8222R1$Rep <- "R1"

#D8222 R2
D8222R2<- c(0,0,0,0,11,19, 13, 6, 4, 1)
D8222R2 <- as.data.table(D8222R2)
names(D8222R2) [1] <- "N"
D8222R2$week <- c(1,1,2,2,3,3,4,4,5,5)
D8222R2$vernalized <- c("Early","Late","Early","Late","Early","Late","Early","Late","Early","Late")
D8222R2$clone <- "D8222"
D8222R2$Rep <- "R2"

#D8515R1 
D8515R1<- c(0,0,1,2,16,81, 9, 17, 2, 0)
D8515R1 <- as.data.table(D8515R1)
names(D8515R1) [1] <- "N"
D8515R1$week <- c(1,1,2,2,3,3,4,4,5,5)
D8515R1$vernalized <- c("Early","Late","Early","Late","Early","Late","Early","Late","Early","Late")
D8515R1$clone <- "D8515"
D8515R1$Rep <- "R1"
#D8515R2
D8515R2<- c(0,0,1,3,13,81, 7, 13, 2, 1)
D8515R2 <- as.data.table(D8515R2)
names(D8515R2) [1] <- "N"
D8515R2$week <- c(1,1,2,2,3,3,4,4,5,5)
D8515R2$vernalized <- c("Early","Late","Early","Late","Early","Late","Early","Late","Early","Late")
D8515R2$clone <- "D8515"
D8515R2$Rep <- "R2"
testing <- rbind(D8222R1, D8222R2,D8515R1,D8515R2)

testingsum <- testing[, lapply(.SD, sum, na.rm=TRUE), by=c("clone","Rep", "vernalized"), .SDcols=c("N")]

sum(testingsum$N)
setkey(testing, clone, Rep,vernalized)
setkey(testingsum, clone, Rep,vernalized)
testing <- merge(testing, testingsum)

# testing$N.y <- as.numeric("504")

testing$prop<- testing$N.x/testing$N.y



testingglm <- glm(prop~vernalized*week+clone/Rep, testing, family=binomial(), weights = N.y)
anova(testingglm, test= "Chisq")

testing$se <- sqrt(testing$prop*(1-testing$prop)/(testing$N.y))
testing$lci <- testing$prop  - (1.96 * testing$se)
testing$uci <- testing$prop  + (1.96 * testing$se)

Fig2B <- ggplot(testing, aes(x = week, y=prop, group=vernalized, col=vernalized)) +
  geom_point(size=2) + geom_line() + facet_wrap(clone~Rep)+
  ylab("Proportion Reproduced each Week")+
  xlab("Week")+
  geom_errorbar(aes(ymin=uci, ymax=lci), width=.1) +
  theme_bw()





#Is this needed?

firstfiveweekslongB <- subset(firstfiveweekslong, Clone=="D8222"|Clone=="D8515")
firstfiveweekslongB$one <-1
firstfiveweekslongB<- data.table(firstfiveweekslongB)
firstfiveweekslongB$Cloneid <- paste(firstfiveweekslongB$Clone,firstfiveweekslongB$Replicate, firstfiveweekslongB$ID)
firstfiveweekslongB2 <- firstfiveweekslongB[, .N, by=list(Clone, Week, Alive, Vernalized, Cloneid, Replicate)]
firstfiveweekslongB3 <- firstfiveweekslongB[, .N, by=list(Clone, Alive)]
firstfiveweekslongB4 <- subset(firstfiveweekslongB3, firstfiveweekslongB3$N > 6)

firstfiveweekslongB3 <- firstfiveweekslongB[, .N, by=list(Clone, Alive, Vernalized, SampleWeek, Replicate)]
firstfiveweekslongB3total <- firstfiveweekslongB[, .N, by=list(Clone,Vernalized,SampleWeek, Replicate)]

firstfiveweekslongB3<- subset(firstfiveweekslongB3, Alive=="R")

setkey(firstfiveweekslongB3, Clone,Vernalized,SampleWeek, Replicate)
setkey(firstfiveweekslongB3total, Clone,Vernalized,SampleWeek, Replicate)
firstfiveweekslongB3merge <- merge(firstfiveweekslongB3, firstfiveweekslongB3total, all.x=TRUE, all.y=TRUE)
# firstfiveweekslongB3merge$prop <- firstfiveweekslongB3merge$N.x/firstfiveweekslongB3merge$N.y
# 
# firstfiveweekslong<- data.table(firstfiveweekslong)
# names(firstfiveweekslong)[1] <- "Clone"
# 
# #Fix mistakes again
# firstfiveweekslong$Alive <- sub("^$", "N", firstfiveweekslong$Alive)
# firstfiveweekslong$Alive <- sub("R ", "R", firstfiveweekslong$Alive)
# firstfiveweekslong$Alive <- sub("Y ", "Y", firstfiveweekslong$Alive)
# firstfiveweekslong$Alive <- sub("Ystr", "Sterile", firstfiveweekslong$Alive)
# firstfiveweekslong$Alive <- sub("Ysm str", "Sterile", firstfiveweekslong$Alive)
# firstfiveweekslong$Alive <- sub("R_momdead", "N", firstfiveweekslong$Alive)
# firstfiveweekslong$Alive <- sub("missing", "N", firstfiveweekslong$Alive)
# 
# 
# ggplot(data=firstfiveweekslongB2, aes(x=Week, y=N, col=Alive))+ geom_col()+ facet_wrap(Clone~Vernalized)+ theme(legend.title = element_blank())
# 
# firstfiveweekslongB3 <- firstfiveweekslongB[, .N, by=list(Clone, Alive)]
# firstfiveweekslongB3total <- firstfiveweekslongB[, .N, by=list(Clone)]
# 
# firstfiveweekslongB3<- subset(firstfiveweekslongB3, Alive=="R")
# 
# setkey(firstfiveweekslongB3, Clone)
# setkey(firstfiveweekslongB3total, Clone)
# firstfiveweekslongB3merge <- merge(firstfiveweekslongB3, firstfiveweekslongB3total, all.x=TRUE, all.y=TRUE)
# firstfiveweekslongB3merge$prop <- firstfiveweekslongB3merge$N.x/firstfiveweekslongB3merge$N.y
# firstfiveweekslongB3mergeglm <- glm(prop~Clone, firstfiveweekslongB3merge, family=binomial(), weights=N.y)
# anova(firstfiveweekslongB3mergeglm, test = "Chisq")
# 
# #Higher rate of reaching reproduction over sample week?
# firstfiveweekslongB3 <- firstfiveweekslongB[, .N, by=list(Clone, Alive, Week, Vernalized, Replicate)]
# firstfiveweekslongB3total <- firstfiveweekslongB[, .N, by=list(Clone, Week, Vernalized, Replicate)]
# firstfiveweekslongB3<- subset(firstfiveweekslongB3, Alive=="R")
# setkey(firstfiveweekslongB3, Clone, Week, Vernalized, Replicate)
# setkey(firstfiveweekslongB3total, Clone, Week, Vernalized, Replicate)
# firstfiveweekslongB3merge <- merge(firstfiveweekslongB3, firstfiveweekslongB3total, all.x=TRUE, all.y=TRUE)
# firstfiveweekslongB3merge$prop <- firstfiveweekslongB3merge$N.x/firstfiveweekslongB3merge$N.y
# firstfiveweekslongB3merge[is.na(prop),prop:=0]
# firstfiveweekslongB3merge <- na.omit(firstfiveweekslongB3merge)
# 
# 
# firstfiveweekslongB3merge$se <- sqrt(firstfiveweekslongB3merge$prop * (1-firstfiveweekslongB3merge$prop)/ (firstfiveweekslongB3merge$N.y))
# firstfiveweekslongB3merge$lci <- firstfiveweekslongB3merge$prop - (1.96 * firstfiveweekslongB3merge$se)
# firstfiveweekslongB3merge$uci <- firstfiveweekslongB3merge$prop + (1.96 * firstfiveweekslongB3merge$se)

firstfiveweekslongB3merge$Vernalized <- sub("0", "Early", firstfiveweekslongB3merge$Vernalized)
firstfiveweekslongB3merge$Vernalized <- sub("1", "Late", firstfiveweekslongB3merge$Vernalized)

# Fig2B <- ggplot(data=firstfiveweekslongB3merge, aes(x=firstfiveweekslongB3merge$Week, y=firstfiveweekslongB3merge$prop, color=Vernalized)) +
#   geom_line()+
#   geom_point()+ xlim(1.9,5.1)+
#   geom_errorbar(aes(ymin=uci, ymax=lci), width=.05) + 
#   ylim(0,0.8)+
#   facet_wrap(Clone~Replicate) + ylab("Proportion Reproduced") +
#   xlab("Weeks After Hatching")+ theme_bw()
# 
# firstfiveweekslongB3mergeglm <- glm(prop~Week*Vernalized+Clone/Replicate, firstfiveweekslongB3merge, family=binomial(), weights=N.y)
# anova(firstfiveweekslongB3mergeglm, test = "Chisq")

#Model 4

HatchlingParseLong
HatchlingParseLong$TotalEpp <- HatchlingParseLong$EmptyEpp + HatchlingParseLong$`1EggEpp`+ HatchlingParseLong$`2EggEpp`
HatchlingParseLongB <- subset(HatchlingParseLong, Clone == "D8515"|Clone == "D8222")
HatchlingParseLongB$Vernalized <- sub("0", "Early", HatchlingParseLongB$Vernalized)
HatchlingParseLongB$Vernalized <- sub("1", "Later", HatchlingParseLongB$Vernalized)
HatchlingParseLongslimB <- HatchlingParseLongB[, lapply(.SD, sum, na.rm=TRUE), by=c("UnqiueID", "Clone", "Vernalized", "EppHatch", "ID", "Replicate"), .SDcols=c("TotalEpp")]
HatchlingParseLonglargeB <- HatchlingParseLongB[, lapply(.SD, sum, na.rm=TRUE), by=c("UnqiueID", "Clone", "Vernalized", "EppHatch", "ID", "Replicate", "EmptyEpp", "1EggEpp", "2EggEpp"), .SDcols=c("TotalEpp")]

Eppplotglm <- glm(TotalEpp~Vernalized+ Clone/Replicate, HatchlingParseLongslimB, family=poisson())
anova(Eppplotglm, test = "Chisq")

#Model 5

HatchlingParseLongEppmales <- HatchlingParseLong[, lapply(.SD, sum, na.rm=TRUE), by=c("Clone","Replicate", "Week", "EmptyEpp", "1EggEpp", "2EggEpp", "Males"), .SDcols="Reproduced"]

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
HatchlingParseLong222Vern <- HatchlingParseLong222[, lapply(.SD, sum, na.rm=TRUE), by=c("Clone","Vernalized", "Replicate"), .SDcols="one"]
HatchlingParseLong515Vern <- HatchlingParseLong515[, lapply(.SD, sum, na.rm=TRUE), by=c("Clone","Vernalized", "Replicate"), .SDcols="one"]

HatchlingParseLong222fillrate1<- HatchlingParseLong222[, lapply(.SD, sum, na.rm=TRUE), by=c("Clone","Vernalized", "Replicate", "SampleWeek", "EmptyEpp", "oneEggEpp", "twoEggEpp", "one"), .SDcols=c("totalepp")]
HatchlingParseLong222fillrate1$totalembs <- HatchlingParseLong222fillrate1$oneEggEpp+2*HatchlingParseLong222fillrate1$twoEggEpp
HatchlingParseLong222fillrate1$availablespaces <- HatchlingParseLong222fillrate1$totalepp*2
HatchlingParseLong222fillrate1$fillrate <- HatchlingParseLong222fillrate1$totalembs/HatchlingParseLong222fillrate1$availablespaces

names(HatchlingParseLong222fillrate1)[7] <- "NumberofOccurrences"

HatchlingParseLong515fillrate1<- HatchlingParseLong515[, lapply(.SD, sum, na.rm=TRUE), by=c("Clone","Vernalized", "Replicate", "SampleWeek", "EmptyEpp", "oneEggEpp", "twoEggEpp", "one"), .SDcols=c("totalepp")]
HatchlingParseLong515fillrate1$totalembs <- HatchlingParseLong515fillrate1$oneEggEpp+2*HatchlingParseLong515fillrate1$twoEggEpp
HatchlingParseLong515fillrate1$availablespaces <- HatchlingParseLong515fillrate1$totalepp*2
HatchlingParseLong515fillrate1$fillrate <- HatchlingParseLong515fillrate1$totalembs/HatchlingParseLong515fillrate1$availablespaces

names(HatchlingParseLong515fillrate1)[7] <- "NumberofOccurrences"

HatchlingParseLong222fillrate1$averagetest <- mean(HatchlingParseLong222fillrate1$fillrate)
HatchlingParseLong515fillrate1$averagetest <- mean(HatchlingParseLong515fillrate1$fillrate)

HatchlingParseLong222Epps <- HatchlingParseLong222[, lapply(.SD, sum, na.rm=TRUE), by=c("Clone","Vernalized", "SampleWeek"), .SDcols=c("totalepp", "one")]

HatchlingParseLongBothBfillrate1 <- rbind(HatchlingParseLong515fillrate1,HatchlingParseLong222fillrate1)
HatchlingParseLongBothBfillrate1$Vernalized <- sub("0", "Early", HatchlingParseLongBothBfillrate1$Vernalized)
HatchlingParseLongBothBfillrate1$Vernalized <- sub("1", "Later", HatchlingParseLongBothBfillrate1$Vernalized)

HatchlingParseLongslimBforgraph<- HatchlingParseLongslimB
HatchlingParseLongslimBforgraph$Vernalized <- sub("Later", "Late", HatchlingParseLongslimBforgraph$Vernalized)

HatchlingParseLongBothBfillrate1forgraph <- HatchlingParseLongBothBfillrate1
HatchlingParseLongBothBfillrate1forgraph$Vernalized <- sub("Later", "Late", HatchlingParseLongBothBfillrate1forgraph$Vernalized)
HatchlingParseLongBothBfillrate1forgraph$Vernalized_f <- factor(HatchlingParseLongBothBfillrate1forgraph$Vernalized, levels=c('Early', 'Late'))

Fig3A <- ggplot(data=HatchlingParseLongslimBforgraph, aes(x=Vernalized, y=TotalEpp, group=Vernalized)) +
  geom_boxplot()+ facet_wrap(Clone~Replicate)+ scale_x_discrete(limit= c("Early", "Late"))+
  ylab("Total Ephippia Produced Per Jar")+ xlab("Hatching time") + ylim(0,30)+
  geom_signif(comparisons = list(c("Early", "Late")),map_signif_level = TRUE)+ 
  theme_bw()+theme(text = element_text(size=14))


Fig3B <- ggplot(data=HatchlingParseLongBothBfillrate1forgraph, aes(x=HatchlingParseLongBothBfillrate1forgraph$Vernalized_f, y=HatchlingParseLongBothBfillrate1forgraph$fillrate, group=Vernalized_f)) +
  geom_boxplot()+ ylab("Average Ephippial Fill Rate per Replicate")+ 
  xlab("Hatching time") +facet_wrap(Clone~Replicate) +
  scale_y_continuous(breaks=c(0,0.25,0.5,0.75, 1), limits=c(0, 1.1))+
  geom_signif(comparisons = list(c("Early","Late")),map_signif_level = TRUE)+ 
  theme_bw()+theme(text = element_text(size=14))


HatchlingParseLongBothBfillrate1glm2 <- glm(fillrate~Vernalized+Clone/Replicate, HatchlingParseLongBothBfillrate1, family=binomial(), weights=availablespaces)
anova(HatchlingParseLongBothBfillrate1glm2, test = "Chisq")



#Model 6

hatchsibswells2 <- hatch[, .N, by=list(Clone, Replicate, FloatSink, Plate, Well, EppHatch, Replicate, ID, HatchDate, DaystoHatch, Vernalized)]

eggswells2 <- eggs[, .N, by=list(Clone, Replicate, Plate, Well, NumHatch, NumLeft, TotalEmb)]

setDT(hatchsibswells2)
setDT(eggswells2)
setkey(eggswells2, Replicate, Clone, Plate, Well)
setkey(hatchsibswells2, Replicate, Clone, Plate, Well)
hatchsibseggs2 <- merge(eggswells2, hatchsibswells2)
hatchsibseggs3 <- subset(hatchsibseggs2, Clone=="D8515" | Clone=="D8222")

hatchsibseggs3floatsinksimp <- hatchsibseggs3[, .N, by=list(Clone, EppHatch, FloatSink)]
hatchsibseggs3floatsinksimp <- hatchsibseggs3[, .N, by=list(FloatSink)]

hatchsibseggs4 <- subset(hatchsibseggs3, FloatSink== "Float" | FloatSink== "Sink")
hatchsibseggs5 <- hatchsibseggs4[, lapply(.SD, sum, na.rm=TRUE), by=c("Clone", "Replicate", "FloatSink", "Vernalized"), .SDcols=c("NumHatch")]


hatchsibseggs6 <- hatchsibseggs5[, lapply(.SD, sum, na.rm=TRUE), by=c("Clone", "Replicate", "FloatSink"), .SDcols=c("NumHatch")]
hatchsibseggs7 <- merge(hatchsibseggs5, hatchsibseggs6, by=c("Clone", "Replicate", "FloatSink"), all=TRUE)
hatchsibseggs7$hatchearlyprop <- hatchsibseggs7$NumHatch.x/hatchsibseggs7$NumHatch.y
hatchsibseggs8 <- subset(hatchsibseggs7, Vernalized=="0")

hatchsibseggs8glm <- glm(hatchearlyprop~FloatSink+ Clone/Replicate, hatchsibseggs8, family=binomial(), weights=NumHatch.y)
anova(hatchsibseggs8glm, test = "Chisq")

hatchsibseggs5D8222 <- subset(hatchsibseggs8, Clone== "D8222")
hatchsibseggs5glm222 <- glm(hatchearlyprop~FloatSink+Replicate, hatchsibseggs5D8222, family=binomial(), weights=NumHatch.y)
anova(hatchsibseggs5glm222, test = "Chisq")

hatchsibseggs8$se <- sqrt(hatchsibseggs8$hatchearlyprop  * (1-hatchsibseggs8$hatchearlyprop )/ (hatchsibseggs8$NumHatch.y))
hatchsibseggs8$lci <- hatchsibseggs8$hatchearlyprop  - (1.96 * hatchsibseggs8$se)
hatchsibseggs8$uci <- hatchsibseggs8$hatchearlyprop  + (1.96 * hatchsibseggs8$se)

Fig4 <- ggplot(data=hatchsibseggs8, aes(x=FloatSink, y=hatchearlyprop, col=Replicate)) +
  geom_point(size=2, position = position_dodge(width = 0.2)) + ylim(0,0.6)+
  theme_bw()+ facet_wrap("Clone")+
  geom_errorbar(aes(ymin=uci, ymax=lci), width=.1, position = position_dodge(width = 0.2)) +
  ylab("Proportion Hatched Early")+ xlab("Buoyancy")

#Fig 5

hatch$Plate <- str_replace(hatch$Plate, "6_7_19_D", "6_7_19D")
hatch$Clone <- str_replace(hatch$Clone, "AXB", "AxB")
setkey(hatch, Plate)
setkey(Vern_Dates, Plate)
DaysfromVern <- merge(hatch, Vern_Dates, all.x=TRUE, all.y=TRUE)


DaysafterVern2 <- DaysafterVern[, .N, by=list(Clone, EppHatch, Daysaftervern)]
colnames(DaysafterVern2) <- c("Clone", "EppHatch", "Daysaftervern", "NHatch")

DaysafterVern2cs <- DaysafterVern2 %>% group_by(Clone,EppHatch) %>% arrange(Daysaftervern) %>%  mutate(cs = cumsum(NHatch))

hatchagdaystohatch <- hatch[, .N, by=list(Clone, DaystoHatch, EppHatch, Vernalized)]
eggagonlycloneEpphatch <- eggs[, lapply(.SD, sum, na.rm=TRUE), by=c("Clone", "EppHatch"), .SDcols="TotalEmb"]
colnames(hatchagdaystohatch) <- c("Clone", "DaystoHatch", "EppHatch", "Vernalized", "NHatch")
hatchagdaystohatch$Clone <- str_replace(hatchagdaystohatch$Clone, "AXB", "AxB")

setkey(hatchagdaystohatch, Clone, EppHatch)
setkey(eggagonlycloneEpphatch, Clone, EppHatch)

hatchagtotalHatch <- merge(hatchagdaystohatch, eggagonlycloneEpphatch, all.x=TRUE, all.y=TRUE,allow.cartesian=TRUE)
hatchagtotalHatchcsEppHatch <- hatchagtotalHatch %>% group_by(Clone,EppHatch) %>% arrange(DaystoHatch) %>%  mutate(cs = cumsum(NHatch))

hatchagtotalHatchcsEppHatchC <- subset(hatchagtotalHatchcsEppHatch, Clone== "D8515"| Clone=="D8222")
DaysafterVern2csC <-  subset(DaysafterVern2cs, Clone== "D8515"| Clone=="D8222")


#FigX

load("selfedC_hetsites_byclone.Rdata")
load("Csites.Rdata")

selfedC_hetsites_byclone <- as.data.table(selfedC_hetsites_byclone)

pattern <- "^D8"  
matching_rows <- grep(pattern, selfedC_hetsites_byclone$clone)

# Subset the data frame based on matching rows
c <- selfedC_hetsites_byclone[matching_rows, ]
c <- na.omit(c)

c2 <- c[, lapply(.SD, sum, na.rm=TRUE), by=c("clone"), .SDcols=c("N")]
names(c2)[2] <- "sum"

c3 <- merge(c, c2, by="clone")
c3$prop <-c3$N/c3$sum

geno0 <- mean(subset(c3, genotype=="0")$prop)
geno1 <- mean(subset(c3, genotype=="1")$prop)
geno2 <- mean(subset(c3, genotype=="2")$prop)

genos <- rbind(geno0,geno1,geno2)
genos <- as.data.table(genos)
names(genos)[1] <- "AverageSelfedC"
Csites

1457 + 546562 + 1567

ParentalC <- c("1457" ,"546562","1567" )
ParentalC <- as.data.table(ParentalC)
names(ParentalC)[1]<- "N"
ParentalC$sum <- "549586"
ParentalC$prop <- as.numeric(ParentalC$N)/as.numeric(ParentalC$sum)

genos$ParentalC  <- ParentalC$prop
genos$Genotype <- c("0","1","2")
genos_long <- pivot_longer(genos, cols = c(ParentalC, AverageSelfedC), names_to = "Lineage", values_to = "Proportion")
genos_long <- as.data.table(genos_long)

FigX <- ggplot(genos_long, aes(x = Genotype, y=Proportion, group=Lineage, linetype=Lineage)) +
  geom_line() +
  geom_point(size=2) +  
  scale_x_discrete(limits= c("0", "1", "2"))+ 
  scale_linetype_manual(values = c("dashed", "solid")) +
  ylab("Proportion of sites")+
  xlab("Genotype")+
  theme_bw()

pdf("C:\\Users\\rjpor\\Desktop\\Hydro Figures Updated\\Second Update\\Fig1.pdf", width=8, height=6)
(FigX + Fig1A)/Fig1B + plot_annotation(title = "Fig. 1", tag_levels = "A")
dev.off()

pdf("C:\\Users\\rjpor\\Desktop\\Hydro Figures Updated\\Second Update\\Fig2.pdf", width=8, height=4)
Fig2A + Fig2B+ plot_annotation(title = "Fig. 2", tag_levels = "A")
dev.off()

pdf("C:\\Users\\rjpor\\Desktop\\Hydro Figures Updated\\Second Update\\Fig3.pdf", width=8, height=6)
Fig3A + Fig3B+ plot_annotation(title = "Fig. 3", tag_levels = "A")
dev.off()

pdf("C:\\Users\\rjpor\\Desktop\\Hydro Figures Updated\\Second Update\\Fig4.pdf", width=4, height=4)
Fig4+ plot_annotation(title = "Fig. 4")
dev.off()

DaysafterVern2csCforgraph <- subset(as.data.table(DaysafterVern2csC), Daysaftervern > -25 & EppHatch== "Hatch")
D8222Daysaftervern0 <- c("D8222", "Hatch", "0", "0", "55")
D8222Daysaftervern0<- as.data.table(rbind(missing0))
names(D8222Daysaftervern0) [1:5] <-c("Clone", "EppHatch", "Daysaftervern", "NHatch", "cs")
DaysafterVern2csCforgraph <- rbind(DaysafterVern2csCforgraph,D8222Daysaftervern0)
DaysafterVern2csCforgraph$cs <- as.numeric(DaysafterVern2csCforgraph$cs)
DaysafterVern2csCforgraph$Daysaftervern  <- as.numeric(DaysafterVern2csCforgraph$Daysaftervern )

pdf("C:\\Users\\rjpor\\Desktop\\Hydro Figures Updated\\Second Update\\Fig5.pdf", width=8, height=6)
grid.arrange(ggplot(subset(hatchagtotalHatchcsEppHatchC, DaystoHatch < 41 & EppHatch== "Epp"),aes(x=DaystoHatch, y=cs, col=Clone)) + 
               geom_line(linewidth=1.2)+ ylab("Cumulative Hatch") + 
               facet_wrap("EppHatch", dir="v")+ 
               ggtitle ("Cumulative Number of Hatchlings Early")+ 
               scale_color_manual(values = c("Darkblue", "Red")) +
               theme_bw()+ labs(x = NULL)+
               theme(legend.position = "none") +ylim(0,500),
             ggplot(subset(DaysafterVern2csC, Daysaftervern > -25 & EppHatch== "Epp"),
                    aes(x=Daysaftervern, y=cs, col=Clone))+ 
               geom_line(linewidth=1.2)+ ggtitle("Late") + labs(x = NULL,y = NULL)+
               scale_color_manual(values = c("Darkblue", "Red")) +
               facet_wrap("EppHatch", dir="v") +ylim(0,500)+ 
               theme_bw()+ xlim(0,14)+
               theme(axis.title.y=element_blank(),axis.text.y = element_blank()),
             
             
ggplot(subset(hatchagtotalHatchcsEppHatchC, DaystoHatch < 41 & EppHatch== "Hatch"),aes(x=DaystoHatch, y=cs, col=Clone)) + 
  geom_line(linewidth=1.2)+ ylab("Cumulative Hatch") + 
  facet_wrap("EppHatch", dir="v")+ 
  scale_color_manual(values = c("Darkblue", "Red")) +
  theme_bw()+ xlab("Days After Collection")+
  theme(legend.position = "none") +ylim(0,200),
ggplot(DaysafterVern2csCforgraph,
       aes(x=Daysaftervern, y=cs, col=Clone))+ 
  geom_line(linewidth=1.2) + 
  scale_color_manual(values = c("Darkblue", "Red")) +
  facet_wrap("EppHatch", dir="v") +ylim(0,200)+ 
  theme_bw()+ xlim(0,14)+ xlab("Days After Cold Shock")+
  theme(axis.title.y=element_blank(),axis.text.y = element_blank()), ncol = 2, widths= c(1.5, 1))
dev.off()




hatchagtotalHatchcsEppHatchC <- as.data.table((hatchagtotalHatchcsEppHatchC), DaystoHatch < 41 & EppHatch== "Hatch")
sum(subset(hatchagtotalHatchcsEppHatchC, DaystoHatch > 5 & DaystoHatch < 41 & Vernalized ==0)$NHatch)
sum(subset(hatchagtotalHatchcsEppHatchC, DaystoHatch < 41 & Vernalized ==0)$NHatch)
204/370



AxChybrids <- read.csv("C:\\Users\\rjpor\\Downloads\\AxChybrids.csv")
AxChybrids <- as.data.table(AxChybrids)

setkey(Firstfive, Clone, Replicate, ID)
setkey(AxChybrids, Clone, Replicate, ID)
Firstfive <- merge(AxChybrids, Firstfive)



FirstfiveDead <- Firstfive[, lapply(.SD, sum, na.rm=TRUE), by=c("Clone", "Vernalized", "Dead", "Epp.Hatch", "Replicate"), .SDcols="sum"]
FirstfiveDead <- FirstfiveDead[!is.na(FirstfiveDead$Vernalized)]

FirstfiveDeadtotal <- FirstfiveDead[, lapply(.SD, sum, na.rm=TRUE), by=c("Clone", "Vernalized", "Epp.Hatch", "Replicate"), .SDcols="sum"]

setkey(FirstfiveDeadtotal, Clone, Vernalized, Epp.Hatch, Replicate)
setkey(FirstfiveDead, Clone, Vernalized, Epp.Hatch, Replicate)
Firstfivedeadsum <- merge(FirstfiveDead, FirstfiveDeadtotal)
Firstfivedeadsum$propalive <- Firstfivedeadsum$sum.x/Firstfivedeadsum$sum.y

FirstfivedeadsumB <- subset(Firstfivedeadsum, Clone=="D8222"|Clone=="D8515")

FirstfivedeadsumBalive <- subset(Firstfivedeadsum, Dead=="0")
FirstfivedeadsumBalive2 <- FirstfivedeadsumBalive
FirstfivedeadsumBalive2$Epp.Hatch <- sub("Epp", "Non-Dissected", FirstfivedeadsumBalive2$Epp.Hatch)
FirstfivedeadsumBalive2$Epp.Hatch <- sub("Hatch", "Dissected", FirstfivedeadsumBalive2$Epp.Hatch)
FirstfivedeadsumBalive2$Vernalized <- sub("0", "Early", FirstfivedeadsumBalive2$Vernalized)
FirstfivedeadsumBalive2$Vernalized <- sub("1", "Later", FirstfivedeadsumBalive2$Vernalized)



FirstfivedeadsumBalive2$se <- sqrt(FirstfivedeadsumBalive2$propalive * (1-FirstfivedeadsumBalive2$propalive)/ (FirstfivedeadsumBalive2$sum.y))
FirstfivedeadsumBalive2$lci <- FirstfivedeadsumBalive2$propalive - (1.96 * FirstfivedeadsumBalive2$se)
FirstfivedeadsumBalive2$uci <- FirstfivedeadsumBalive2$propalive + (1.96 * FirstfivedeadsumBalive2$se)


Fig2A <- ggplot(data=FirstfivedeadsumBalive2, aes(x=Vernalized, y=propalive, col=Epp.Hatch, shape = Clone))+ 
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  #facet_wrap("Vernalized") + 
  xlab("Hatching time") + 
  ylab("Proportion Survived to Reproduction")+ 
  ylim(0,1)+ facet_wrap(Clone~Replicate)+
  geom_errorbar(aes(ymin=uci, ymax=lci), width=.05, position = position_dodge(width = 0.3))+
  scale_colour_manual(values=setNames(color.codes, zone))+ theme_bw()

