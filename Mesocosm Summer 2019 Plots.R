#Summer 2019 Plots

#Run Mesocosm Summer 2019.R first, then Mesocosm Summer 2019 GLM.R, then this

#Figure 1 

pred.dt2$CloneDissection  <- paste(pred.dt2$Clone, pred.dt2$Epphatch)

zone <-c("Non-Dissected","Dissected")
color.codes<-as.character(c("#a6611a", "#33a02c"))
color.names<-c("Brown", "Green")

#Need to add estimated to legend

pred.dt2nocloneestimated <- pred.dt2noclone %>% select(Verndiff, Epphatch, Estimated)
pred.dt2nocloneestimated$data <- pred.dt2nocloneestimated$Estimated
pred.dt2nocloneestimated$shape <- as.character("Estimated")
pred.dt2nocloneactual <- pred.dt2noclone %>% select(Verndiff, Epphatch, pred, lci, uci)
pred.dt2nocloneactual$data <- pred.dt2nocloneactual$pred
pred.dt2nocloneactual$shape <- as.character("Actual")

pred.dt2nocloneboth <- rbind(pred.dt2nocloneactual, pred.dt2nocloneestimated, fill=TRUE)
  

plot1simp <- ggplot(data=pred.dt2noclone, aes(x=Verndiff, y=pred,col=Epphatch)) +
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
plot1simp

plot1simp

#Redo Plot 2 no clone

plot2 <- ggplot(data=pred.dt3, aes(x=pred.dt3$SampleWeek, y=pred.dt3$pred, group=Clone, col=Clone)) +
  geom_line()+
  geom_point(size=2)+ 
  geom_errorbar(aes(ymin=uci, ymax=lci), width=.05) + 
   ylab("Proportion Hatched Early") + xlab("Sample Week of Mesocosm")+ 
  ylim(0,0.6)+
  theme(text=element_text(size=20))+ theme_bw()

plot1simp/plot2+ plot_annotation(title = "Fig. 1", tag_levels = "A")

# Figure 2
FirstfivedeadsumBalive2predforgraph<- FirstfivedeadsumBalive2pred
FirstfivedeadsumBalive2predforgraph$Vernalized <- sub("Later", "Late", FirstfivedeadsumBalive2predforgraph$Vernalized)
names(FirstfivedeadsumBalive2predforgraph)[1] <- "Hatching_time"
names(FirstfivedeadsumBalive2predforgraph)[3] <- "Dissection_Status"

Dissectedvsnonclonegraph <- ggplot(data=FirstfivedeadsumBalive2predforgraph, aes(x=Hatching_time, y=pred, col=Dissection_Status, shape = Clone))+ 
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  #facet_wrap("Vernalized") + 
  xlab("Hatching time") + 
  ylab("Proportion Alive")+ 
  ylim(0.3,0.8)+
  geom_errorbar(aes(ymin=uci, ymax=lci), width=.05, position = position_dodge(width = 0.3))+
  scale_colour_manual(values=setNames(color.codes, zone))+ theme_bw()

Dissectedvsnonclonegraph

Reproductionplot <- ggplot(data=ReproductionB.dt, aes(x=ReproductionB.dt$Week, y=ReproductionB.dt$pred)) +
  geom_line(aes(linetype=Condition), position = position_dodge(width = 0.1))+
  geom_point(position = position_dodge(width = 0.1))+ xlim(1.9,5.1)+
  geom_errorbar(aes(ymin=uci, ymax=lci), width=.05, position = position_dodge(width = 0.1)) + 
  ylim(0,0.65)+
  facet_wrap("Clone") + ylab("Proportion Reproduced") +
  xlab("Weeks After Hatching")+ theme_bw()

Reproductionplot

# Dissectedvsnonclonegraphall+Reproductionplot + plot_annotation(title = "Fig. 2", tag_levels = "A")
Dissectedvsnonclonegraph+Reproductionplot + plot_annotation(title = "Fig. 2", tag_levels = "A")

#Figure 3

pred.dthatchlongforgraph<- pred.dthatchlong
pred.dthatchlongforgraph$Vernalized_f <- sub("Later", "Late", pred.dthatchlongforgraph$Vernalized_f)


a213432 <- ggplot(data=pred.dthatchlongforgraph, aes(x=pred.dthatchlongforgraph$Vernalized_f, y=pred.dthatchlongforgraph$fillrate, group=Vernalized_f)) +
  geom_boxplot()+ ylab("Average Ephippial Fill Rate per Replicate")+ 
  xlab("Hatching time") +facet_wrap("Clone") +
  scale_y_continuous(breaks=c(0,0.25,0.5,0.75, 1), limits=c(0, 1.1))+
  geom_signif(comparisons = list(c("Early","Late")),map_signif_level = TRUE)+ 
  theme_bw()+theme(text = element_text(size=14))
a213432
summary(pred.dthatchlongforgraph)
pred.dthatchlongforgraphtest <- pred.dthatchlongforgraph[, lapply(.SD, sum, na.rm=TRUE), by=c("Clone","Vernalized"), .SDcols=c("NumberofOccurrences","EmptyEpp","oneEggEpp","twoEggEpp")]
pred.dthatchlongforgraphtest

HatchlingParseLongBothBfillrate1glm2 <- glm(fillrate~Vernalized*Clone, HatchlingParseLongBothBfillrate1, family=binomial(), weights=availablespaces)
anova(HatchlingParseLongBothBfillrate1glm2, test = "Chisq")
anova(HatchlingParseLongBothBfillrate1glm2, test = "Chisq")


HatchlingParseLongslimBforgraph<- HatchlingParseLongslimB
HatchlingParseLongslimBforgraph$Vernalized <- sub("Later", "Late", HatchlingParseLongslimBforgraph$Vernalized)

Totaleppplot <- ggplot(data=HatchlingParseLongslimBforgraph, aes(x=Vernalized, y=TotalEpp, group=Vernalized)) +
  geom_boxplot()+ facet_wrap("Clone")+ scale_x_discrete(limit= c("Early", "Late"))+
  ylab("Total Ephippia Produce Per Replicate")+ xlab("Hatching time") + ylim(0,30)+
  geom_signif(comparisons = list(c("Early", "Late")),map_signif_level = TRUE)+ 
  theme_bw()+theme(text = element_text(size=14))
Totaleppplot

a213432
a213432/Totaleppplot+ plot_annotation(title = "Fig. 3", tag_levels = "A")

Totaleppplotnoclone <- ggplot(data=HatchlingParseLongslimBforgraph, aes(x=Vernalized, y=TotalEpp)) +
  geom_boxplot()+ scale_x_discrete(limit= c("Early", "Late"))+
  ylab("Total Ephippia Produce Per Replicate")+ xlab("Condition") +
  geom_signif(comparisons = list(c("Early", "Late")),map_signif_level = TRUE)+
  theme_bw()
Totaleppplotnoclone


Fig4 <- ggplot(data=pred.dthatchsibseggs8, aes(x=FloatSink, y=pred, col=Clone)) +
  geom_point(position = position_dodge(width = 0.2), size = 2) + ylim(0,0.5)+
  theme_bw()+
  geom_errorbar(aes(ymin=uci, ymax=lci), width=.1, position = position_dodge(width = 0.2)) +
  ylab("Proportion Hatched Early")+ xlab("Buoyancy")


# pdf("C:\\Users\\rjpor\\Desktop\\Rimages\\plot1all.pdf", width=4, height=6)
# plot1simp/plot2+ plot_annotation(title = "Fig. 1", tag_levels = "A")
# dev.off()
# 
# pdf("C:\\Users\\rjpor\\Desktop\\Rimages\\fig2.pdf", width=8, height=4)
# Dissectedvsnonclonegraph+Reproductionplot + plot_annotation(title = "Fig. 2", tag_levels = "A")
# dev.off()
# 
# pdf("C:\\Users\\rjpor\\Desktop\\Rimages\\fig3.pdf", width=4, height=6)
# Totaleppplot/a213432+ plot_annotation(title = "Fig. 3", tag_levels = "A")
# dev.off()
# 
# pdf("C:\\Users\\rjpor\\Desktop\\Rimages\\fig4.pdf", width=3, height=3)
# Fig4+ plot_annotation(title = "Fig. 4")
# dev.off()
