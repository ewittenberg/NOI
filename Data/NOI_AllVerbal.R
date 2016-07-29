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
unique(dat$participant)

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


####subset to verbal####
which(colnames(dat)=="participant")
which(colnames(dat)=="language")
which(colnames(dat)=="item")
which(colnames(dat)=="verbal_framesetter")
which(colnames(dat)=="verbal_Verb")
datV <- dat[c(1:3,9:11)]; head(dat)
write.csv(datV, "NOI_ALL_CleanVerbalData.csv")
str(datV)

datV$Forder <- recode(datV$verbal_framesetter, "'1' = 'Ffirst'; 
                          '2' = 'Fmid'; 
                          '3' = 'Flast'")
datV$Vorder <- recode(datV$verbal_Verb, "'1' = 'Vfirst'; 
                          '2' = 'Vmid'; 
                          '3' = 'Vlast'")
datV$Torder <- recode(datV$verbal_topic, "'1' = 'Tfirst'; 
                          '2' = 'Tmid'; 
                          '3' = 'Tlast'")



#make a verbal_order column
datV$verbal_order <- paste(datV$verbal_framesetter,datV$verbal_topic,datV$verbal_Verb,sep='')
unique(datV$verbal_order)
datV <- droplevels(subset(datV, datV$verbal_order!="NA12"))
datV <- droplevels(subset(datV, datV$verbal_order!="NANANA"))
datV$verbal_order <- recode(datV$verbal_order, "'231' = 'TVF' ; 
                           '123' = 'FTV'; 
                           '213' = 'TFV'; 
                           '312' = 'VFT'; 
                           '321' = 'VTF'; 
                           '213' = 'TFV'; 
                           '132' = 'FVT'")


#data summaries and chi-square tests
library(plyr)
count(datV, 'verbal_order')
do.call(rbind , by(datV$language, datV$Vorder, summary))
library(MASS)       # load the MASS package 
tblALL = table(datV$language, datV$Vorder) 
tblALL                 # the contingency table 
chisq.test(tblALL) 

str(datV)
Vlast <- droplevels(subset(datV, Vorder=="Vlast"))
tblVlast = table(Vlast$verbal_order, Vlast$language) 
tblVlast                 # the contingency table 
chisq.test(tblVlast) 
plot(tblVlast)



deutsch <- droplevels(subset(datV, language=="D"))
tbldeutsch = table(deutsch$Vorder) 
tbldeutsch                 # the contingency table 
chisq.test(tbldeutsch) 

turkish <- droplevels(subset(datV, language=="T"))
tblturkish = table(turkish$Vorder) 
tblturkish                 # the contingency table 
chisq.test(tblturkish) 

eng <- droplevels(subset(datV, language=="E"))
tbleng = table(eng$Vorder) 
tbleng                 # the contingency table 
chisq.test(tbleng) 


Alltable <- xtabs(~verbal_order + language, data=datV)
ftable(Alltable) # print table
summary(Alltable) # chi-square test of indepedence 

Ftable <- xtabs(~Forder + language, data=datV)
ftable(Ftable) # print table
summary(Ftable) # chi-square test of indepedence

Ttable <- xtabs(~Torder + language, data=datV)
ftable(Ttable) # print table
summary(Ttable) # chi-square test of indepedence

Vtable <- xtabs(~Vorder + language, data=datV)
ftable(Vtable) # print table
summary(Vtable) # chi-square test of indepedence

######data visualization
datV$Forder <- as.factor(datV$Forder)
print(levels(datV$Forder)) 
datV$Forder = factor(datV$Forder,levels(datV$Forder)[c(1,3,2)])
print(levels(datV$Forder)) 

library(vcd)
doubledecker(verbal_order ~ language, data=datV)
doubledecker(Forder ~ language, data=datV)
doubledecker(Torder ~ language, data=datV)
doubledecker(Vorder ~ language, data=datV)

datV$language <- as.factor(datV$language)
datV$language <- factor(datV$language, levels(datV$language)[c(1,3,2)])

#look at Framesetters
datV.counts <- with(datV,aggregate(list(Count=Forder),list(order=Forder,language=language),length))
datV.sums <- with(datV.counts,tapply(Count,list(language=language),sum))
datV.counts$Proportion <- with(datV.counts,Count/datV.sums[cbind(language)])
order <- factor(datV.counts$language)
str(datV.counts)

bar <- ggplot(datV.counts, aes(x=language,y=Proportion, fill = order))
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

bar <- ggplot(datV.counts, aes(x=order,y=Proportion, fill = language))
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
datV$Vorder <- as.factor(datV$Vorder)
print(levels(datV$Vorder)) 
datV$Vorder = factor(datV$Vorder,levels(datV$Vorder)[c(1,3,2)])
print(levels(datV$Vorder)) 


datV.counts <- with(datV,aggregate(list(Count=Vorder),list(order=Vorder,language=language),length))
datV.sums <- with(datV.counts,tapply(Count,list(language=language),sum))
datV.counts$Proportion <- with(datV.counts,Count/datV.sums[cbind(language)])
order <- factor(datV.counts$language)
datV.counts

bar <- ggplot(datV.counts, aes(x=language,y=Proportion, fill = order))
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

####barplot verb placement by language####
head(datV.counts)
datV.counts$language <-as.character(datV.counts$language)
datV.counts$language <- recode(datV.counts$language, "'T' = 'Turkish'; 
                          'E' = 'English'; 'D' = 'German'")
datV.counts$language <-as.factor(datV.counts$language)

bar <- ggplot(datV.counts, aes(x=order,y=Proportion, fill = language))
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
  labs(x="", y="Proportion of order for Verbs", fill="Language")
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
german <- droplevels(subset(datV, language=="D"))
str(german)
datV.counts <- with(german,aggregate(list(Count=Vorder),list(order=Vorder,language=language),length))
datV.sums <- with(datV.counts,tapply(Count,list(language=language),sum))
datV.counts$Proportion <- with(datV.counts,Count/datV.sums[cbind(language)])
order <- factor(datV.counts$language)
str(datV.counts)

datV.counts

gbar <- ggplot(datV.counts, aes(x=order,y=Proportion, fill = language))
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
turkish <- droplevels(subset(datV, language=="T"))
str(turkish)
datV.counts <- with(turkish,aggregate(list(Count=Vorder),list(order=Vorder,language=language),length))
datV.sums <- with(datV.counts,tapply(Count,list(language=language),sum))
datV.counts$Proportion <- with(datV.counts,Count/datV.sums[cbind(language)])
order <- factor(datV.counts$language)
str(datV.counts)

datV.counts

gbar <- ggplot(datV.counts, aes(x=order,y=Proportion, fill = language))
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
english <- droplevels(subset(datV, language=="E"))
str(english)
datV.counts <- with(english,aggregate(list(Count=Vorder),list(order=Vorder,language=language),length))
datV.sums <- with(datV.counts,tapply(Count,list(language=language),sum))
datV.counts$Proportion <- with(datV.counts,Count/datV.sums[cbind(language)])
order <- factor(datV.counts$language)
str(datV.counts)

datV.counts

gbar <- ggplot(datV.counts, aes(x=order,y=Proportion, fill = language))
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


####subset to nonverbal####
str(dat)
which(colnames(dat)=="participant")
which(colnames(dat)=="language")
which(colnames(dat)=="item")
which(colnames(dat)=="framesetter")
which(colnames(dat)=="verb")
datN <- dat[c(1:3,6:8)]; head(datN)
write.csv(datNN, "NOI_ALL_CleanNonVerbaldatNa.csv")
str(datNN)

datN$Forder <- recode(datN$verbal_framesetter, "'1' = 'Ffirst'; 
                     '2' = 'Fmid'; 
                     '3' = 'Flast'")
datN$Vorder <- recode(datN$verbal_Verb, "'1' = 'Vfirst'; 
                     '2' = 'Vmid'; 
                     '3' = 'Vlast'")
datN$Torder <- recode(datN$verbal_topic, "'1' = 'Tfirst'; 
                     '2' = 'Tmid'; 
                     '3' = 'Tlast'")


#make a verbal_order column
datN$verbal_order <- paste(datN$verbal_framesetter,datN$verbal_topic,datN$verbal_Verb,sep='')
unique(datN$verbal_order)
datN <- droplevels(subset(datN, datN$verbal_order!="NA12"))
datN <- droplevels(subset(datN, datN$verbal_order!="NANANA"))
datN$verbal_order <- recode(datN$verbal_order, "'231' = 'TVF' ; 
                           '123' = 'FTV'; 
                           '213' = 'TFV'; 
                           '312' = 'VFT'; 
                           '321' = 'VTF'; 
                           '213' = 'TFV'; 
                           '132' = 'FVT'")


#datNa summaries and chi-square tests
library(plyr)
count(datN, 'verbal_order')
do.call(rbind , by(datN$language, datN$Vorder, summary))
library(MASS)       # load the MASS package 
tblALL = table(datN$language, datN$Vorder) 
tblALL                 # the contingency table 
chisq.test(tblALL) 

str(datN)
Vlast <- droplevels(subset(datN, Vorder=="Vlast"))
tblVlast = table(Vlast$verbal_order, Vlast$language) 
tblVlast                 # the contingency table 
chisq.test(tblVlast) 
plot(tblVlast)
