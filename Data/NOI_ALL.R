# Eva, April 2016 #

rm(list=ls())

library(doBy)
library(ggplot2)
library(car)
library(lme4)
library(languageR)
library(xtable)
library(reshape2)
library(scales)

#read data in
getwd()
setwd("/Users/evinapatata/GitHub/NOI/Data")
dat <- read.csv("NOI_ALL_RawData.csv")
head(dat)
dat$participant <- as.character(dat$participant)
dat$language <- as.character(dat$language)
dat$participant <- paste(dat$participant,dat$language,sep='_')

str(dat)

#drop stuff we don't need -- ONLY critical items
dat <- droplevels(subset(dat, dat$category=="E"))
dat <- droplevels(subset(dat, dat$framesetter!=""))
dat <- droplevels(subset(dat, dat$framesetter!=" "))
dat <- droplevels(subset(dat, dat$framesetter!="4"))
dat <- droplevels(subset(dat, dat$framesetter!="na"))
dat <- droplevels(subset(dat, dat$topic!=""))
dat <- droplevels(subset(dat, dat$topic!=" "))
dat <- droplevels(subset(dat, dat$topic!="4"))
dat <- droplevels(subset(dat, dat$topic!="na"))
dat <- droplevels(subset(dat, dat$verb!=""))
dat <- droplevels(subset(dat, dat$verb!=" "))
dat <- droplevels(subset(dat, dat$verb!="4"))
dat <- droplevels(subset(dat, dat$verb!="na"))
dat <- droplevels(subset(dat, dat$verb!="0"))
dat <- droplevels(subset(dat, dat$verbal_topic!="NA"))
dat <- droplevels(subset(dat, dat$verbal_framesetter!="NA"))


str(dat)
####action data####
dat$Forder_A <- recode(dat$framesetter, "'1' = 'Ffirst'; 
                          '2' = 'Fmid'; 
                          '3' = 'Flast'")
dat$Vorder_A <- recode(dat$verb, "'1' = 'Vfirst'; 
                          '2' = 'Vmid'; 
                          '3' = 'Vlast'")
dat$Torder_A <- recode(dat$topic, "'1' = 'Tfirst'; 
                          '2' = 'Tmid'; 
                          '3' = 'Tlast'")
####verbal data####
dat$Forder_V <- recode(dat$verbal_framesetter, "'1' = 'Ffirst'; 
                          '2' = 'Fmid'; 
                          '3' = 'Flast'")
dat$Vorder_V <- recode(dat$verbal_Verb, "'1' = 'Vfirst'; 
                          '2' = 'Vmid'; 
                          '3' = 'Vlast'")
dat$Torder_V <- recode(dat$verbal_topic, "'1' = 'Tfirst'; 
                          '2' = 'Tmid'; 
                          '3' = 'Tlast'")


#make an action_order column
dat$action_order <- paste(dat$framesetter,dat$topic,dat$verb,sep='')
unique(dat$action_order)

dat$action_order <- recode(dat$action_order, "'231' = 'TVF' ; 
                           '123' = 'FTV'; 
                           '213' = 'TFV'; 
                           '312' = 'VFT'; 
                           '321' = 'VTF'; 
                           '213' = 'TFV'; 
                           '132' = 'FVT'")
dat <- droplevels(subset(dat, dat$action_order!="112"))
dat <- droplevels(subset(dat, dat$action_order!="113"))


#make a verbal_order column
dat$verbal_order <- paste(dat$verbal_framesetter,dat$verbal_topic,dat$verbal_Verb,sep='')
unique(dat$verbal_order)
dat$verbal_order <- recode(dat$verbal_order, "'231' = 'TVF' ; 
                           '123' = 'FTV'; 
                           '213' = 'TFV'; 
                           '312' = 'VFT'; 
                           '321' = 'VTF'; 
                           '213' = 'TFV'; 
                           '132' = 'FVT'")

write.csv(dat, "NOI_ALL_CleanData.csv")

str(dat)
library(reshape)

####make long table for verbs####
which(colnames(dat)=="Vorder_A")
which(colnames(dat)=="Vorder_V")
dat.long <- dat[c(1:3,13,16)]; head(dat.long)
dat.long<-melt(dat.long, id.vars = c("participant", "language","item"), measure.vars = c("Vorder_V", "Vorder_A"))
str(dat.long)

#Data summaries and chi-square tests
library(plyr)
library(MASS)       # load the MASS package 
tblALL = table(dat.long$language, dat.long$value, dat.long$variable ) 
tblALL                 # the contingency table 
chisq.test(tblALL) 


deutsch <- droplevels(subset(dat.long, language=="D"))
tbldeutsch = table(deutsch$variable, deutsch$value) 
tbldeutsch                 # the contingency table 
chisq.test(tbldeutsch) 

turkish <- droplevels(subset(dat.long, language=="T"))
tblturkish = table(turkish$variable, turkish$value) 
tblturkish                 # the contingency table 
chisq.test(tblturkish) 

eng <- droplevels(subset(dat.long, language=="E"))
tbleng = table(eng$variable, eng$value) 
tbleng                 # the contingency table 
chisq.test(tbleng) 


Alltable <- xtabs(~verbal_order + language, data=dat)
ftable(Alltable) # print table
summary(Alltable) # chi-square test of indepedence 

Ftable <- xtabs(~Forder + language, data=dat)
ftable(Ftable) # print table
summary(Ftable) # chi-square test of indepedence

Ttable <- xtabs(~Torder + language, data=dat)
ftable(Ttable) # print table
summary(Ttable) # chi-square test of indepedence

Vtable <- xtabs(~Vorder + language, data=dat)
ftable(Vtable) # print table
summary(Vtable) # chi-square test of indepedence

######Data visualization
dat$Forder <- as.factor(dat$Forder)
print(levels(dat$Forder)) 
dat$Forder = factor(dat$Forder,levels(dat$Forder)[c(1,3,2)])
print(levels(dat$Forder)) 

library(vcd)
doubledecker(verbal_order ~ language, data=dat)
doubledecker(Forder ~ language, data=dat)
doubledecker(Torder ~ language, data=dat)
doubledecker(Vorder ~ language, data=dat)

dat$language <- as.factor(dat$language)
dat$language <- factor(dat$language, levels(dat$language)[c(1,3,2)])

#look at Framesetters
dat.counts <- with(dat,aggregate(list(Count=Forder),list(order=Forder,language=language),length))
dat.sums <- with(dat.counts,tapply(Count,list(language=language),sum))
dat.counts$Proportion <- with(dat.counts,Count/dat.sums[cbind(language)])
order <- factor(dat.counts$language)
str(dat.counts)

bar <- ggplot(dat.counts, aes(x=language,y=Proportion, fill = order))
dodge <- position_dodge(width=0.9)
bar + geom_bar(stat="identity",position=dodge) + 
  scale_fill_manual(values=c("#99d8c9", "#2ca25f","#006d2c"))  + 
  theme_bw()+ 
  theme(axis.text.y = element_text(size=16), 
        axis.text.x = element_text(size=16),
        strip.text.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size=20))+
  scale_y_continuous(labels=percent, limits = c(0, 1))+
  labs(x="", y="Proportion of order for Framesetters", fill="Answer")
  ggsave("VerbalOrder_Framesetters_Language.pdf", width=12, height=8, unit="in")

bar <- ggplot(dat.counts, aes(x=order,y=Proportion, fill = language))
dodge <- position_dodge(width=0.9)
bar + geom_bar(stat="identity",position=dodge) + 
  scale_fill_manual(values=c("#99d8c9", "#2ca25f","#006d2c"))  + 
  theme_bw()+ 
  theme(axis.text.y = element_text(size=16), 
        axis.text.x = element_text(size=16),
        strip.text.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size=20))+
  scale_y_continuous(labels=percent, limits = c(0, 1))+
  labs(x="", y="Proportion of order for Framesetters", fill="Answer")
ggsave("VerbalOrder_Language_Framesetters.pdf", width=12, height=8, unit="in")

#look at Verbs
str(dat)
dat$Vorder <- as.factor(dat$Vorder)
print(levels(dat$Vorder)) 
dat$Vorder = factor(dat$Vorder,levels(dat$Vorder)[c(1,3,2)])
print(levels(dat$Vorder)) 


dat.counts <- with(dat,aggregate(list(Count=Vorder),list(order=Vorder,language=language),length))
dat.sums <- with(dat.counts,tapply(Count,list(language=language),sum))
dat.counts$Proportion <- with(dat.counts,Count/dat.sums[cbind(language)])
order <- factor(dat.counts$language)
dat.counts

bar <- ggplot(dat.counts, aes(x=language,y=Proportion, fill = order))
dodge <- position_dodge(width=0.9)
bar + geom_bar(stat="identity",position=dodge) + 
  scale_fill_manual(values=c("#99d8c9", "#2ca25f","#006d2c"))  + 
  theme_bw()+ 
  theme(axis.text.y = element_text(size=16), 
        axis.text.x = element_text(size=16),
        strip.text.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size=20))+
  scale_y_continuous(labels=percent, limits = c(0, 1))+
  labs(x="", y="Proportion of order for Verbs", fill="Answer")
ggsave("VerbalOrder_Verbs_Language.pdf", width=12, height=8, unit="in")

bar <- ggplot(dat.counts, aes(x=order,y=Proportion, fill = language))
dodge <- position_dodge(width=0.9)
bar + geom_bar(stat="identity",position=dodge) + 
  scale_fill_manual(values=c("#99d8c9", "#2ca25f","#006d2c"))  + 
  theme_bw()+ 
  theme(axis.text.y = element_text(size=16), 
        axis.text.x = element_text(size=16),
        strip.text.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size=20))+
  scale_y_continuous(labels=percent, limits = c(0, 1))+
  labs(x="", y="Proportion of order for Verbs", fill="Answer")
ggsave("VerbalOrder_Language_Verbs.pdf", width=12, height=8, unit="in")


#look at Vlast
str(Vlast)
Vlast.counts <- with(Vlast,aggregate(list(Count=verbal_order),list(order=verbal_order,language=language),length))
Vlast.sums <- with(Vlast.counts,tapply(Count,list(language=language),sum))
Vlast.counts$Proportion <- with(Vlast.counts,Count/Vlast.sums[cbind(language)])
order <- factor(Vlast.counts$language)
str(Vlast.counts)

bar <- ggplot(Vlast.counts, aes(x=language,y=Proportion, fill = order))
dodge <- position_dodge(width=0.9)
bar + geom_bar(stat="identity",position=dodge) + 
  scale_fill_manual(values=c("#99d8c9", "#2ca25f","#006d2c"))  + 
  theme_bw()+ 
  theme(axis.text.y = element_text(size=16), 
        axis.text.x = element_text(size=16),
        strip.text.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size=20))+
  scale_y_continuous(labels=percent, limits = c(0, 1))+
  labs(x="", y="Proportion of order for Verbs", fill="Answer")
ggsave("VerbalOrder_Vlast_Language.pdf", width=12, height=8, unit="in")


####German####
#look at Verbs
german <- droplevels(subset(dat, language=="D"))
str(german)
dat.counts <- with(german,aggregate(list(Count=Vorder),list(order=Vorder,language=language),length))
dat.sums <- with(dat.counts,tapply(Count,list(language=language),sum))
dat.counts$Proportion <- with(dat.counts,Count/dat.sums[cbind(language)])
order <- factor(dat.counts$language)
str(dat.counts)

dat.counts

gbar <- ggplot(dat.counts, aes(x=order,y=Proportion, fill = language))
dodge <- position_dodge(width=0.9)
gbar + geom_bar(stat="identity",position=dodge) + 
  scale_fill_manual(values=c("#99d8c9", "#2ca25f","#006d2c"))  + 
  theme_bw()+ 
  theme(axis.text.y = element_text(size=16), 
        axis.text.x = element_text(size=16),
        strip.text.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size=20))+
  scale_y_continuous(labels=percent, limits = c(0, 1))+
  labs(x="", y="Proportion of order for Verbs in German", fill="Answer")


####Turkish####
#look at Verbs
turkish <- droplevels(subset(dat, language=="T"))
str(turkish)
dat.counts <- with(turkish,aggregate(list(Count=Vorder),list(order=Vorder,language=language),length))
dat.sums <- with(dat.counts,tapply(Count,list(language=language),sum))
dat.counts$Proportion <- with(dat.counts,Count/dat.sums[cbind(language)])
order <- factor(dat.counts$language)
str(dat.counts)

dat.counts

gbar <- ggplot(dat.counts, aes(x=order,y=Proportion, fill = language))
dodge <- position_dodge(width=0.9)
gbar + geom_bar(stat="identity",position=dodge) + 
  scale_fill_manual(values=c("#99d8c9", "#2ca25f","#006d2c"))  + 
  theme_bw()+ 
  theme(axis.text.y = element_text(size=16), 
        axis.text.x = element_text(size=16),
        strip.text.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size=20))+
  scale_y_continuous(labels=percent, limits = c(0, 1))+
  labs(x="", y="Proportion of order for Verbs in Turkish", fill="Answer")


####English####
#look at Verbs
english <- droplevels(subset(dat, language=="E"))
str(english)
dat.counts <- with(english,aggregate(list(Count=Vorder),list(order=Vorder,language=language),length))
dat.sums <- with(dat.counts,tapply(Count,list(language=language),sum))
dat.counts$Proportion <- with(dat.counts,Count/dat.sums[cbind(language)])
order <- factor(dat.counts$language)
str(dat.counts)

dat.counts

gbar <- ggplot(dat.counts, aes(x=order,y=Proportion, fill = language))
dodge <- position_dodge(width=0.9)
gbar + geom_bar(stat="identity",position=dodge) + 
  scale_fill_manual(values=c("#99d8c9", "#2ca25f","#006d2c"))  + 
  theme_bw()+ 
  theme(axis.text.y = element_text(size=16), 
        axis.text.x = element_text(size=16),
        strip.text.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size=20))+
  scale_y_continuous(labels=percent, limits = c(0, 1))+
  labs(x="", y="Proportion of order for Verbs", fill="Answer")


