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
dat <- read.csv("NOI_AllAction_RawData.csv")
head(dat)
dat$subject <- as.character(dat$subject)
dat$language <- as.character(dat$language)
dat$subject <- paste(dat$subject,dat$language,sep='_')
unique(dat$subject)

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

which(colnames(dat)=="subject")
which(colnames(dat)=="language")
which(colnames(dat)=="abbreviation")
which(colnames(dat)=="framesetter")
which(colnames(dat)=="verb")
dat <- dat[c(1:3,9:11)]; head(dat)
write.csv(dat, "NOI_ALL_CleanActionData.csv")

dat$Forder <- recode(dat$framesetter, "'1' = 'Ffirst'; 
                          '2' = 'Fmid'; 
                          '3' = 'Flast'")
dat$Vorder <- recode(dat$verb, "'1' = 'V-front'; 
                          '2' = 'V-mid'; 
                          '3' = 'V-end'")
dat$Torder <- recode(dat$topic, "'1' = 'Tfirst'; 
                          '2' = 'Tmid'; 
                          '3' = 'Tlast'")

str(dat)

#make an action_order column
dat$action_order <- paste(dat$framesetter,dat$topic,dat$verb,sep='')
unique(dat$action_order)
dat$action_order <- recode(dat$action_order, "'231' = 'VFT' ; '123' = 'FTV'; '213' = 'TFV'; '312' = 'TVF'; 
                           '321' = 'VTF'; '213' = 'TFV'; '132' = 'FVT'")
dat <- droplevels(subset(dat, dat$action_order!="112"))
dat <- droplevels(subset(dat, dat$action_order!="113"))

#Data summaries and chi-square tests
library(plyr)
count(dat, 'action_order')
do.call(rbind , by(dat$language, dat$Forder, summary))
library(MASS)       # load the MASS package 
tblALL = table(dat$language, dat$Vorder) 
tblALL                 # the contingency table 
chisq.test(tblALL) 

str(dat)

str(dat)
Vend <- droplevels(subset(dat, Vorder=="Vend"))
tblVend = table(Vend$action_order, Vend$language) 
tblVend                 # the contingency table 
chisq.test(tblVend) 
plot(tblVend)



deutsch <- droplevels(subset(dat, language=="D"))
tbldeutsch = table(deutsch$Vorder) 
tbldeutsch                 # the contingency table 
chisq.test(tbldeutsch) 

turkish <- droplevels(subset(dat, language=="T"))
tblturkish = table(turkish$Vorder) 
tblturkish                 # the contingency table 
chisq.test(tblturkish) 

eng <- droplevels(subset(dat, language=="E"))
tbleng = table(eng$Vorder) 
tbleng                 # the contingency table 
chisq.test(tbleng) 


Alltable <- xtabs(~action_order + language, data=dat)
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
print(levels(dat$Forder)) 
dat$Forder = factor(dat$Forder,levels(dat$Forder)[c(1,3,2)])
print(levels(dat$Forder)) 

dat$language <- as.factor(dat$language)
dat$language <- factor(dat$language, levels(dat$language)[c(1,3,2)])
dat$language

#look at Framesetters
dat.counts <- with(dat,aggregate(list(Count=Forder),list(order=Forder,language=language),length))
dat.sums <- with(dat.counts,tapply(Count,list(language=language),sum))
dat.counts$Proportion <- with(dat.counts,Count/dat.sums[cbind(language)])
order <- factor(dat.counts$language)
str(dat.counts)

bar <- ggplot(dat.counts, aes(x=order,y=Proportion, fill = language))
dodge <- position_dodge(width=0.9)
bar + geom_bar(stat="identity",position=dodge) + 
  scale_fill_manual(values=c("#99d8c9", "#2ca25f","#1a9641"))  + 
  theme_bw()+ 
  theme(axis.text.y = element_text(size=16), 
        axis.text.x = element_text(size=16),
        strip.text.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        axis.title.x = element_text(size=20))+
  scale_y_continuous(labels=percent, limits = c(0, 1))+
  labs(x="", y="Proportion of order for Framesetters", fill="Language")
  ggsave("Order_Framesetters_Language.pdf", width=12, height=8, unit="in")

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
ggsave("Order_Language_Framesetters.pdf", width=12, height=8, unit="in")

#look at Verbs
print(levels(dat$Vorder)) 
dat$Vorder = factor(dat$Vorder,levels(dat$Vorder)[c(1,3,2)])
print(levels(dat$Vorder)) 


dat.counts <- with(dat,aggregate(list(Count=Vorder),list(order=Vorder,language=language),length))
dat.sums <- with(dat.counts,tapply(Count,list(language=language),sum))
dat.counts$Proportion <- with(dat.counts,Count/dat.sums[cbind(language)])
order <- factor(dat.counts$language)
str(dat.counts)


####barplot verb placement by language####
dat.counts
dat.counts$language <-as.character(dat.counts$language)
dat.counts$language <- recode(dat.counts$language, "'T' = 'Turkish'; 
                               'E' = 'English'; 'D' = 'German'")
dat.counts$language <-as.factor(dat.counts$language)
#dat.counts$order <-relevel(dat.counts$order,levels(dat.counts$order)[3])
#dat.counts$order
dat.counts$order  <- factor(dat.counts$order , levels=c("V-front", "V-mid", "V-end"))

bar <- ggplot(dat.counts, aes(x=order,y=Proportion, fill = language))
dodge <- position_dodge(width=0.9)
bar + geom_bar(stat="identity",position=dodge, color="black", size=0.2) + 
  geom_text(aes(label=paste(round(100*Proportion, 1), "%", sep="")), position=position_dodge(width=0.9), vjust=-0.25) +
  scale_fill_manual(values=c('#fc8d59','#ffffbf','#1a9641'))  + 
  theme_bw()+ 
  theme(axis.text.y = element_text(size=24), 
        axis.text.x = element_text(size=24),
        strip.text.x = element_text(size=24),
        axis.title.y = element_text(size=24),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        axis.title.x = element_text(size=24))+
  scale_y_continuous(labels=percent, limits = c(0, 1))+
  labs(x="", y="Proportion of order for Verbs", fill="Language")
ggsave("Order_Language_Verbs.pdf", width=12, height=6, unit="in")



####look at Vend####
str(Vend)
Vend.counts <- with(Vend,aggregate(list(Count=action_order),list(order=action_order,language=language),length))
Vend.sums <- with(Vend.counts,tapply(Count,list(language=language),sum))
Vend.counts$Proportion <- with(Vend.counts,Count/Vend.sums[cbind(language)])
order <- factor(Vend.counts$language)
str(Vend.counts)

Vend.counts$language <-as.character(Vend.counts$language)
Vend.counts$language <- recode(Vend.counts$language, "'T' = 'Turkish'; 
                              'E' = 'English'; 'D' = 'German'")
Vend.counts$language <-as.factor(Vend.counts$language)


Vend.counts$order <-as.character(Vend.counts$order)
Vend.counts$order <- recode(Vend.counts$order, "'FTV' = 'framesetter- topic'; 
                                'TFV' = 'topic- framesetter'")
Vend.counts$order <-as.factor(Vend.counts$order)

levels(Vend.counts$order)


levels(Vend.counts$order) <- gsub(" ", "\n", levels(Vend.counts$order))

bar <- ggplot(Vend.counts, aes(x=order,y=Proportion, fill = language))
dodge <- position_dodge(width=0.9)
bar + geom_bar(stat="identity",position=dodge, color="black", size=0.2) + 
  scale_fill_manual(values=c('#fc8d59','#ffffbf','#1a9641'))  + 
  theme_bw()+ 
  theme(axis.text.y = element_text(size=24), 
        axis.text.x = element_text(size=24),
        strip.text.x = element_text(size=24),
        axis.title.y = element_text(size=24),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        axis.title.x = element_text(size=24))+
  scale_y_continuous(labels=percent, limits = c(0, 1))+
  labs(x="", y="Orders for Topics and Framesetters", fill="Language")
ggsave("Order_Vend_Language.pdf", width=12, height=8, unit="in")



####English####
#look at Verbs
english <- droplevels(subset(dat, language=="E"))
english <- droplevels(subset(english, Vorder!="V-front"))
str(english)
dat.counts <- with(english,aggregate(list(Count=Vorder),list(order=Vorder,language=language, framesetter=framesetter, topic=topic),length))
dat.counts$ft <- dat.counts$framesetter
dat.counts$ft <- recode(dat.counts$ft, "'1' = 'framesetter first'; '2' = 'topic first'; '3' = 'topic first'")
dat.sums <- with(dat.counts,tapply(Count,list(order=order),sum))
dat.sums
dat.counts$Proportion <- with(dat.counts,Count/dat.sums[cbind(order)])

dat.counts
dat.counts$order  <- factor(dat.counts$order , levels=c("V-mid", "V-end"))

gbar <- ggplot(dat.counts, aes(x=order,y=Proportion, fill = ft))
dodge <- position_dodge(width=0.9)
gbar + geom_bar(stat="identity",position=dodge, color="black") + 
  geom_text(aes(label=paste(round(100*Proportion, 1), "%", sep="")), position=position_dodge(width=0.9), vjust=-0.25) +
  scale_fill_manual(values=c("#99d8c9", "#2ca25f","#006d2c"))  + 
  theme_bw()+ 
  theme(axis.text.y = element_text(size=24), 
        axis.text.x = element_text(size=24),
        strip.text.x = element_text(size=24),
        axis.title.y = element_text(size=24),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        axis.title.x = element_text(size=24))+
  scale_y_continuous(labels=percent, limits = c(0, 1))+
  labs(x="", y="Proportion", fill="Order")
ggsave("Order_FT_English.pdf", width=12, height=8, unit="in")

####look at Vmid####
Vmid <- droplevels(subset(dat, Vorder=="V-mid"))
tblVmid = table(Vmid$action_order, Vmid$language) 
tblVmid                 # the contingency table 
chisq.test(tblVmid) 
plot(tblVmid)

Vmid$action_order <- as.factor(Vmid$action_order)
levels(Vmid$action_order)
Vmid.counts <- with(Vmid,aggregate(list(Count=action_order),list(order=action_order,language=language),length))
Vmid.sums <- with(Vmid.counts,tapply(Count,list(language=language),sum))
Vmid.counts$Proportion <- with(Vmid.counts,Count/Vmid.sums[cbind(language)])
order <- factor(Vmid.counts$language)
str(Vmid.counts)

Vmid.counts$language <-as.character(Vmid.counts$language)
Vmid.counts$language <- recode(Vmid.counts$language, "'T' = 'Turkish'; 
                                'E' = 'English'; 'D' = 'German'")
Vmid.counts$language <-as.factor(Vmid.counts$language)


Vmid.counts$order <-as.character(Vmid.counts$order)
Vmid.counts$order <- recode(Vmid.counts$order, "'FVT' = 'framesetter- first'; 
                             'TVF' = 'topic- first'")
Vmid.counts$order <-as.factor(Vmid.counts$order)

levels(Vmid.counts$order)


levels(Vmid.counts$order) <- gsub(" ", "\n", levels(Vmid.counts$order))

bar <- ggplot(Vmid.counts, aes(x=order,y=Proportion, fill = language))
dodge <- position_dodge(width=0.9)
bar + geom_bar(stat="identity",position=dodge, color="black", size=0.2) + 
  scale_fill_manual(values=c('#fc8d59','#ffffbf','#1a9641'))  + 
  theme_bw()+ 
  theme(axis.text.y = element_text(size=24), 
        axis.text.x = element_text(size=24),
        strip.text.x = element_text(size=24),
        axis.title.y = element_text(size=24),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        axis.title.x = element_text(size=24))+
  scale_y_continuous(labels=percent, limits = c(0, 1))+
  labs(x="", y="Orders for Topics and Framesetters", fill="Language")
ggsave("Order_Vmid_Language.pdf", width=12, height=8, unit="in")


