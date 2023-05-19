library(ggplot2)
library(tidyr)
library(gridExtra)
library(ggpubr)
library(RColorBrewer)
library(multcompView)
library(cowplot)
library(tidyverse)
library(dplyr)
library(lme4)
library(stats)
library(devtools)



## MAKING LEGEND ##

dflegend = data.frame(region <- c('East','East','East','East','East'),
                      clade <- c('U1','U2','U3','L','O'),
                      value <- c(1,1,1,1,1))

dflegend$region <- factor(dflegend$region)
dflegend$clade <- factor(dflegend$clade) 

dflegendp <- ggplot(data=dflegend, aes(x=" ", y=value, group=clade, colour=clade, fill=clade)) +
  geom_bar(width = 1, stat = "identity", color='black') +
  coord_polar("y", start=0) + 
  geom_text(aes(x = 1 , label = clade), position = position_stack(vjust = 0.5), color="black",
            fontface = "bold", size = 20) +
  facet_grid(.~ region) +theme_void() + scale_fill_manual(values = c("U1" = "cyan3",
                                                                     "U2" = "lemonchiffon",
                                                                     "U3" = "lightcoral",
                                                                     "L" = "tan2",
                                                                     "O" = "plum2"))

legend <- get_legend(dflegendp) 
grid.newpage()                              
grid.draw(legend) 


## outputs East pie chart for native colonies

df1 = data.frame(region <- c('East','East'),
                 type <- c('U1','L'),
                 value <- c(15,1))

df1$region <- factor(df1$region)
df1$type <- factor(df1$type) 

df1p <- ggplot(data=df1, aes(x=" ", y=value, group=type, colour=type, fill=type)) +
  geom_bar(width = 1, stat = "identity", color='black') +
  coord_polar("y", start=0) + 
  geom_text(aes(x = 1 , label = value), position = position_stack(vjust = 0.5), color="black",
            fontface = "bold", size = 23) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  facet_grid(.~ region) +theme_void() + scale_fill_manual(values = c("U1" = "cyan3",
                                                                     "U2" = "lemonchiffon",
                                                                     "U3" = "lightcoral",
                                                                     "L" = "tan2",
                                                                     "O" = "plum2"))

df1p


## outputs EbiilFR pie chart for native colonies

df2 = data.frame(region <- c('EbiilFR','EbiilFR','EbiilFR','EbiilFR'),
                 type <- c('U1','U2','L','O'),
                 value <- c(18,10,1,2))

df2$region <- factor(df2$region)
df2$type <- factor(df2$type) 

df2p <- ggplot(data=df2, aes(x=" ", y=value, group=type, colour=type, fill=type)) +
  geom_bar(width = 1, stat = "identity", color='black') +
  coord_polar("y", start=0) + 
  geom_text(aes(x = 1 , label = value), position = position_stack(vjust = 0.5), color="black",
            fontface = "bold", size = 23) +
  facet_grid(.~ region) +theme_void() + scale_fill_manual(values = c("U1" = "cyan3",
                                                                     "U2" = "lemonchiffon",
                                                                     "U3" = "lightcoral",
                                                                     "L" = "tan2",
                                                                     "O" = "plum2"))

df2p

## outputs NofEbiil pie chart for native colonies

df3 = data.frame(region <- c('NofEbiil','NofEbiil','NofEbiil','NofEbiil'),
                 type <- c('U1','U2','L','O'),
                 value <- c(19,32,12,5))

df3$region <- factor(df3$region)
df3$type <- factor(df3$type) 

df3p <- ggplot(data=df3, aes(x=" ", y=value, group=type, colour=type, fill=type)) +
  geom_bar(width = 1, stat = "identity", color='black') +
  coord_polar("y", start=0) + 
  geom_text(aes(x = 1 , label = value), position = position_stack(vjust = 0.5), color="black",
            fontface = "bold", size = 23) +
  facet_grid(.~ region) +theme_void() + scale_fill_manual(values = c("U1" = "cyan3",
                                                                     "U2" = "lemonchiffon",
                                                                     "U3" = "lightcoral",
                                                                     "L" = "tan2",
                                                                     "O" = "plum2"))

df3p

## outputs North pie chart for native colonies

df4 = data.frame(region <- c('North','North'),
                 type <- c('U1','L'),
                 value <- c(15,5))

df4$region <- factor(df4$region)
df4$type <- factor(df4$type) 

df4p <- ggplot(data=df4, aes(x=" ", y=value, group=type, colour=type, fill=type)) +
  geom_bar(width = 1, stat = "identity", color='black') +
  coord_polar("y", start=0) + 
  geom_text(aes(x = 1 , label = value), position = position_stack(vjust = 0.5), color="black",
            fontface = "bold", size = 23) +
  facet_grid(.~ region) +theme_void() + scale_fill_manual(values = c("U1" = "cyan3",
                                                                     "U2" = "lemonchiffon",
                                                                     "U3" = "lightcoral",
                                                                     "L" = "tan2",
                                                                     "O" = "plum2"))

df4p



## outputs South pie chart for native colonies

df5 = data.frame(region <- c('South','South','South','South'),
                 type <- c('U1','U3','L','O'),
                 value <- c(5,20,32,1))

df5$region <- factor(df5$region)
df5$type <- factor(df5$type) 

df5p <- ggplot(data=df5, aes(x=" ", y=value, group=type, colour=type, fill=type)) +
  geom_bar(width = 1, stat = "identity", color='black') +
  coord_polar("y", start=0) + 
  geom_text(aes(x = 1 , label = value), position = position_stack(vjust = 0.5), color="black",
            fontface = "bold", size = 23) +
  facet_grid(.~ region) +theme_void() + scale_fill_manual(values = c("U1" = "cyan3",
                                                                     "U2" = "lemonchiffon",
                                                                     "U3" = "lightcoral",
                                                                     "L" = "tan2",
                                                                     "O" = "plum2"))

df5p

## outputs UlongFR pie chart for native colonies

df6 = data.frame(region <- c('UlongFR','UlongFR','UlongFR','UlongFR'),
                 type <- c('U1','U3','L','O'),
                 value <- c(1,22,1,15))

df6$region <- factor(df6$region)
df6$type <- factor(df6$type) 

df6p <- ggplot(data=df6, aes(x=" ", y=value, group=type, colour=type, fill=type)) +
  geom_bar(width = 1, stat = "identity", color='black') +
  coord_polar("y", start=0) + 
  geom_text(aes(x = 1 , label = value), position = position_stack(vjust = 0.5), color="black",
            fontface = "bold", size = 23) +
  facet_grid(.~ region) +theme_void() + scale_fill_manual(values = c("U1" = "cyan3",
                                                                     "U2" = "lemonchiffon",
                                                                     "U3" = "lightcoral",
                                                                     "L" = "tan2",
                                                                     "O" = "plum2"))

df6p

## outputs West pie chart for native colonies

df7 = data.frame(region <- c('West','West'),
                 type <- c('U1','L'),
                 value <- c(37,4))

df7$region <- factor(df7$region)
df7$type <- factor(df7$type) 

df7p <- ggplot(data=df7, aes(x=" ", y=value, group=type, colour=type, fill=type)) +
  geom_bar(width = 1, stat = "identity", color='black') +
  coord_polar("y", start=0) + 
  geom_text(aes(x = 1 , label = value), position = position_stack(vjust = 0.5), color="black",
            fontface = "bold", size = 23) +
  facet_grid(.~ region) +theme_void() + scale_fill_manual(values = c("U1" = "cyan3",
                                                                     "U2" = "lemonchiffon",
                                                                     "U3" = "lightcoral",
                                                                     "L" = "tan2",
                                                                     "O" = "plum2"))

df7p



## Chloroplast type distribution by reef bar plot
ctype <- Final_datasets_chloroplast_updatedOtypes

ggplot(ctype, aes(fill=type, y=value, key=region, x=factor(reef))) + 
  geom_bar(stat="identity") +
  facet_grid(~fct_relevel(region,'East','South','UlongFR','West','North','NofEbiil','EbiilFR'),scales="free", space="free_x") +
  labs(y= "Count", x = "Reef") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.text.y=element_text(size=18), 
        axis.title=element_text(size=18), axis.text.x = element_text(angle = 90, vjust = 0.5, size=14), strip.background = element_rect(fill="white"))+ 
  scale_fill_manual(values = c("U1" = "cyan3",
                               "U2" = "lemonchiffon",
                               "U3" = "lightcoral",
                               "L" = "tan2",
                               "O" = "plum2"))



## parent by reef pie charts 

df8p = data.frame(reef <- c('50'),
                  type <- c('U3'),
                  value <- c(9))

df8p$reef <- factor(df8p$reef)
df8p$type <- factor(df8p$type) 

df8pp <- ggplot(data=df8p, aes(x=" ", y=value, group=type, colour=type, fill=type)) +
  geom_bar(width = 1, stat = "identity", color='black') +
  coord_polar("y", start=0) + 
  geom_text(aes(x = 1 , label = value), position = position_stack(vjust = 0.5), color="black",
            fontface = "bold", size = 23) +
  facet_grid(.~ reef) +theme_void() + scale_fill_manual(values = c("U1" = "cyan3",
                                                                   "U2" = "lemonchiffon",
                                                                   "U3" = "lightcoral",
                                                                   "L" = "tan2",
                                                                   "O" = "plum2"))
df8pp


df9p = data.frame(reef <- c('51'),
                  type <- c('U1','U3','O'),
                  value <- c(1,4,2))

df9p$reef <- factor(df9p$reef)
df9p$type <- factor(df9p$type) 

df9pp <- ggplot(data=df9p, aes(x=" ", y=value, group=type, colour=type, fill=type)) +
  geom_bar(width = 1, stat = "identity", color='black') +
  coord_polar("y", start=0) + 
  geom_text(aes(x = 1 , label = value), position = position_stack(vjust = 0.5), color="black",
            fontface = "bold", size = 23) +
  facet_grid(.~ reef) +theme_void() + scale_fill_manual(values = c("U1" = "cyan3",
                                                                   "U2" = "lemonchiffon",
                                                                   "U3" = "lightcoral",
                                                                   "L" = "tan2",
                                                                   "O" = "plum2"))
df9pp


df10p = data.frame(reef <- c('52'),
                   type <- c('U3','O'),
                   value <- c(6,4))

df10p$reef <- factor(df10p$reef)
df10p$type <- factor(df10p$type) 

df10pp <- ggplot(data=df10p, aes(x=" ", y=value, group=type, colour=type, fill=type)) +
  geom_bar(width = 1, stat = "identity", color='black') +
  coord_polar("y", start=0) + 
  geom_text(aes(x = 1 , label = value), position = position_stack(vjust = 0.5), color="black",
            fontface = "bold", size = 23) +
  facet_grid(.~ reef) +theme_void() + scale_fill_manual(values = c("U1" = "cyan3",
                                                                   "U2" = "lemonchiffon",
                                                                   "U3" = "lightcoral",
                                                                   "L" = "tan2",
                                                                   "O" = "plum2"))
df10pp

df11p = data.frame(reef <- c('53'),
                   type <- c('U3','L','O'),
                   value <- c(2,1,4))

df11p$reef <- factor(df11p$reef)
df11p$type <- factor(df11p$type) 

df11pp <- ggplot(data=df11p, aes(x=" ", y=value, group=type, colour=type, fill=type)) +
  geom_bar(width = 1, stat = "identity", color='black') +
  coord_polar("y", start=0) + 
  geom_text(aes(x = 1 , label = value), position = position_stack(vjust = 0.5), color="black",
            fontface = "bold", size = 23) +
  facet_grid(.~ reef) +theme_void() + scale_fill_manual(values = c("U1" = "cyan3",
                                                                   "U2" = "lemonchiffon",
                                                                   "U3" = "lightcoral",
                                                                   "L" = "tan2",
                                                                   "O" = "plum2"))
df11pp

df12p = data.frame(reef <- c('54'),
                   type <- c('U3','O'),
                   value <- c(1,5))

df12p$reef <- factor(df12p$reef)
df12p$type <- factor(df12p$type) 

df12pp <- ggplot(data=df12p, aes(x=" ", y=value, group=type, colour=type, fill=type)) +
  geom_bar(width = 1, stat = "identity", color='black') +
  coord_polar("y", start=0) + 
  geom_text(aes(x = 1 , label = value), position = position_stack(vjust = 0.5), color="black",
            fontface = "bold", size = 23) +
  facet_grid(.~ reef) +theme_void() + scale_fill_manual(values = c("U1" = "cyan3",
                                                                   "U2" = "lemonchiffon",
                                                                   "U3" = "lightcoral",
                                                                   "L" = "tan2",
                                                                   "O" = "plum2"))
df12pp




## parent northern reefs grouped pie charts 

df13p = data.frame(region <- c('62-65'),
                   type <- c('U1','U2','L'),
                   value <- c(3,10,2))

df13p$region <- factor(df13p$region)
df13p$type <- factor(df13p$type) 

df13pp <- ggplot(data=df13p, aes(x=" ", y=value, group=type, colour=type, fill=type)) +
  geom_bar(width = 1, stat = "identity", color='black') +
  coord_polar("y", start=0) + 
  geom_text(aes(x = 1 , label = value), position = position_stack(vjust = 0.5), color="black",
            fontface = "bold", size = 23) +
  facet_grid(.~ region) +theme_void() + scale_fill_manual(values = c("U1" = "cyan3",
                                                                     "U2" = "lemonchiffon",
                                                                     "U3" = "lightcoral",
                                                                     "L" = "tan2",
                                                                     "O" = "plum2"))
df13pp

df14p = data.frame(region <- c('40-44'),
                   type <- c('U1','U2','L','O'),
                   value <- c(4,8,3,1))

df14p$region <- factor(df14p$region)
df14p$type <- factor(df14p$type) 

df14pp <- ggplot(data=df14p, aes(x=" ", y=value, group=type, colour=type, fill=type)) +
  geom_bar(width = 1, stat = "identity", color='black') +
  coord_polar("y", start=0) + 
  geom_text(aes(x = 1 , label = value), position = position_stack(vjust = 0.5), color="black",
            fontface = "bold", size = 23) +
  facet_grid(.~ region) +theme_void() + scale_fill_manual(values = c("U1" = "cyan3",
                                                                     "U2" = "lemonchiffon",
                                                                     "U3" = "lightcoral",
                                                                     "L" = "tan2",
                                                                     "O" = "plum2"))
df14pp

df15p = data.frame(region <- c('30-36'),
                   type <- c('U1','U2','L','O'),
                   value <- c(15,24,11,2))

df15p$region <- factor(df15p$region)
df15p$type <- factor(df15p$type) 

df15pp <- ggplot(data=df15p, aes(x=" ", y=value, group=type, colour=type, fill=type)) +
  geom_bar(width = 1, stat = "identity", color='black') +
  coord_polar("y", start=0) + 
  geom_text(aes(x = 1 , label = value), position = position_stack(vjust = 0.5), color="black",
            fontface = "bold", size = 23) +
  facet_grid(.~ region) +theme_void() + scale_fill_manual(values = c("U1" = "cyan3",
                                                                     "U2" = "lemonchiffon",
                                                                     "U3" = "lightcoral",
                                                                     "L" = "tan2",
                                                                     "O" = "plum2"))
df15pp

df16p = data.frame(region <- c('60-61,37-30'),
                   type <- c('U1','L'),
                   value <- c(30,6))

df16p$region <- factor(df16p$region)
df16p$type <- factor(df16p$type) 

df16pp <- ggplot(data=df16p, aes(x=" ", y=value, group=type, colour=type, fill=type)) +
  geom_bar(width = 1, stat = "identity", color='black') +
  coord_polar("y", start=0) + 
  geom_text(aes(x = 1 , label = value), position = position_stack(vjust = 0.5), color="black",
            fontface = "bold", size = 23) +
  facet_grid(.~ region) +theme_void() + scale_fill_manual(values = c("U1" = "cyan3",
                                                                     "U2" = "lemonchiffon",
                                                                     "U3" = "lightcoral",
                                                                     "L" = "tan2",
                                                                     "O" = "plum2"))
df16pp




## TEMPERATURE PLOTS 

temp <- W20_Nursery_HOBOs

##PR7

pr7 <- ggplot(temp, aes(x=date, y=temp, group=reef, color=reef))+
  geom_line()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+
  labs(y= "Temperature (°C)", x = "Date")
pr7

pr18 <- ggplot(temp, aes(x=date, y=temp, group=reef, color=reef))+
  geom_line()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+
  labs(y= "Temperature (°C)", x = "Date")
pr18

pr21 <- ggplot(temp, aes(x=date, y=temp, group=reef, color=reef))+
  geom_line()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+
  labs(y= "Temperature (°C)", x = "Date")
pr21

pr27 <- ggplot(temp, aes(x=date, y=temp, group=reef, color=reef))+
  geom_line()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+
  labs(y= "Temperature (°C)", x = "Date")
pr27

ggarrange(pr7, pr18, pr21, pr27)



## mortality analysis -mixed effects logistic regression

mort <- Final_datasets_chloroplast_updatedOtypes
mort$region<- as.factor(mort$region)
mort$transplant_reef <- as.factor(mort$transplant_reef)
mort$parent_type <- as.factor(mort$parent_type)

# estimate the model and store results in m
m <- glmer(survival ~ region + transplant_reef + parent_type + (1 | nursery_colony), data = mort, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

summary(m)

## comparing mortality to bleaching binomial regression

binom.model<-glm(mort$survival~mort$VBS, family="binomial")
summary(binom.model)

## Fisher exact test to test association b/w host ID and symbiont ID

hostsym <- Final_datasets_chloroplast_updatedOtypes

cont_table <- table(hostsym$Type, hostsym$HostID)
cont_table

ftL <- fisher.test(cont_table, workspace = 6e8)
ftL

dput(ftL$p.value)
print(ftL)

## IBD plot

dist <- final_IBD_chloroplast ## forR tab

ggplot(dist, aes(x=geo_dist, y=genet_dist)) + geom_point(size=2.5) +  
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color="turquoise4")+
  labs(x="Geographic Distance (km)", y = "Avg. Nucleotide Distance")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 14), 
        axis.text.y=element_text(size=14), 
        axis.title=element_text(size=14,face="bold"))+
  stat_regline_equation(label.y = 1.15, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 1.1, aes(label = ..rr.label..)) + 
  stat_cor(label.y = 1.05, aes(label = ..p.label..))

