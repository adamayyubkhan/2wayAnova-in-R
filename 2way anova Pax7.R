#The data set looks at the effect of either a drug (DMSO/ML355) or treatment (injured/uninjured) on the expression of pax7 muscle stem cells 
#H1 = Drug type has no significant effect on pax7 expressin 
#H2 = Treatment has no significant effect on Pax7 expression 
#H3 = Treatment and Drug interaction will have no signiifcant effect on pax7 expressioin

library(readr)
library(ggplot2)
library(multcompView)
library(dplyr)
library(ggpubr)
library(multcompView)
library(tibble)
library(tidyverse)

head(zebrafish)
str(zebrafish)
#I first wanted to check the first 6 and bottom 6 of the data set to make sure that al factors were correctly labelled prior to any statiscial analys

zebrafish$Treatment <- as.factor(zebrafish$Treatment)
zebrafish$drug <- as.factor(zebrafish$drug)
#The factors were not correctly labelled hence I coded for these adjustments

str(zebrafish)
head(zebrafish)
#I checked to make sure the necessary changes had occured as planned


ggplot(data = zebrafish, mapping = aes(Treatment, pax7_expression, fill = drug))+
  geom_boxplot()+
  theme_classic()
#I plotted the iniital data set to review the data

twoANOVA <- aov(pax7_expression ~ factor(Treatment)* factor(drug), data = zebrafish)
summary(twoANOVA)
#This provided a statitcal table summary of the twoANOVA for me to 

TUKEY <- TukeyHSD(x=twoANOVA, conf.level = .95)
plot(TUKEY, las=1 , col='blue')
#The initial plot function in R produced a graph which cuts off the y axis labels.
#In order to work around this I had to use the ggplot function in order to create a way around this

df <- TUKEY[["factor(Treatment):factor(drug)"]] %>% as.data.frame() 
df2 <- tibble::rownames_to_column(df, "id")
#I had to name the inital column with all the y axis titels as id
df2%>%
  ggplot(aes(id))+
  geom_errorbar(aes(ymin=lwr,
                    ymax=upr,
                    y=diff),
                width = 0.2)+
  geom_point(aes(x=id,y=diff),shape=3,size=4)+
  geom_hline(yintercept=0,
             color="red")+
  labs(x=NULL)+
  coord_flip()
#The graph had to be designed inverted in order to allow the cord_flip function to then rever the graph to the correct orientation.















