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

zebrafish$Treatment <- as.factor(zebrafish$Treatment)
zebrafish$drug <- as.factor(zebrafish$drug)

str(zebrafish)
head(zebrafish)

ggplot(data = zebrafish, mapping = aes(Treatment, pax7_expression, fill = drug))+
  geom_boxplot()+
  theme_classic()

twoANOVA <- aov(pax7_expression ~ factor(Treatment)* factor(drug), data = zebrafish)
summary(twoANOVA)

TUKEY <- TukeyHSD(x=twoANOVA, conf.level = .95)
plot(TUKEY, las=1 , col='blue')

df <- TUKEY[["factor(Treatment):factor(drug)"]] %>% as.data.frame() 
df2 <- tibble::rownames_to_column(df, "id")
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
















