#Summer 2019 Glm

#Run Mesocosm Summer 2019.R first


#For 1st figure (1B)

#Model 1
magfixedcumulcombinedglm <- glm((prophatch.x*TotalEmb)/((prophatch.y*TotalEmb))~Clone*SampleWeek, 
                           magfixedcumulcombinedpre0, family=binomial(),
                           weights=((prophatch.y*TotalEmb)))

anova(magfixedcumulcombinedglm, test = "Chisq")


#For 2nd figure


#Survival at the end of 5 weeks as a function of clone and treatment status. (2A)

#Model 2
FirstfivedeadsumBalive2glm <- glm(propalive~Vernalized*Epp.Hatch+Clone, FirstfivedeadsumBalive2, family=binomial(), weights=sum.y)
anova(FirstfivedeadsumBalive2glm, test = "Chisq")

#Reproduction over the first 5 weeks as a function of clone and treatment status. (2B)

#Model 3
firstfiveweekslongB3mergeglm <- glm(prop~Vernalized*Week+Clone, firstfiveweekslongB3merge, family=binomial(), weights=N.y)
anova(firstfiveweekslongB3mergeglm, test = "Chisq")

#For 3rd figure


#totalepp

#Model 4


Eppplotglm <- glm(TotalEpp~Vernalized*Clone, HatchlingParseLongslimB, family=poisson())
anova(Eppplotglm, test = "Chisq")


#Fill rate as a function of sample week

#Model 5
HatchlingParseLongBothBfillrate1glm2 <- glm(fillrate~Vernalized*Clone, HatchlingParseLongBothBfillrate1, family=binomial(), weights=availablespaces)
anova(HatchlingParseLongBothBfillrate1glm2, test = "Chisq")

HatchlingParseLongBothBfillrate1515<- subset(HatchlingParseLongBothBfillrate1, Clone=="D8515")
HatchlingParseLongBothBfillrate1glm515 <- glm(fillrate~Vernalized, HatchlingParseLongBothBfillrate1515, family=binomial(), weights=availablespaces)
anova(HatchlingParseLongBothBfillrate1glm515, test = "Chisq")

#Does floating have an effect on hatch rate?
#Model 6
hatchsibseggs6 <- hatchsibseggs5[, lapply(.SD, sum, na.rm=TRUE), by=c("Clone", "FloatSink"), .SDcols=c("NumHatch")]
hatchsibseggs7 <- merge(hatchsibseggs5, hatchsibseggs6, by=c("Clone", "FloatSink"), all=TRUE)
hatchsibseggs7$hatchearlyprop <- hatchsibseggs7$NumHatch.x/hatchsibseggs7$NumHatch.y
hatchsibseggs8 <- subset(hatchsibseggs7, Vernalized=="0")

hatchsibseggs8glm <- glm(hatchearlyprop~FloatSink*Clone, hatchsibseggs8, family=binomial(), weights=NumHatch.y)
anova(hatchsibseggs8glm, test = "Chisq")

hatchsibseggs5D8222 <- subset(hatchsibseggs8, Clone== "D8222")
hatchsibseggs5glm222 <- glm(hatchearlyprop~FloatSink, hatchsibseggs5D8222, family=binomial(), weights=NumHatch.y)
anova(hatchsibseggs5glm222, test = "Chisq")


hatchsibseggs8glmpred <- predict(hatchsibseggs8glm, newdata=data.frame(hatchsibseggs8), se.fit=T)
pred.dthatchsibseggs8 <- as.data.table(data.frame(hatchsibseggs8))
pred.dthatchsibseggs8[,pred:=plogis(hatchsibseggs8glmpred$fit)]
pred.dthatchsibseggs8[,lci:=plogis(hatchsibseggs8glmpred$fit-1.96*hatchsibseggs8glmpred$se.fit)]
pred.dthatchsibseggs8[,uci:=plogis(hatchsibseggs8glmpred$fit+1.96*hatchsibseggs8glmpred$se.fit)]

