#set working directory
setwd("~/Mitchell Lab Stuff/Data")

#import dataset and make it an object
count1 <- read.csv("RS4_count_data1.csv")

#preview dataset
head(count1)

rm(count1)

#make a simple plot of number of microglia vs. genotype
library(ggplot2)

ggplot(count1, aes(x=genotype, y=Number_MiG))+
  geom_boxplot()+ geom_jitter()

#rearrange genotype levels so they appear in a specific order
library(dplyr)

str(count1)

levels(count1$genotype)

count1$genotype <- factor(count1$genotype , levels=c("wt", "het", "mut"))

#set y-axis by viewing range for the number of microglia overall
count1%>%
  summarise(range=range(Number_MiG))

#can also look at range by genotype
count1%>%
  group_by(genotype) %>%
  summarise(range=range(Number_MiG))

#make a prettier plot
#add theme, scale y-axis, change axis labels
ggplot(count1, aes(x=genotype, y=Number_MiG))+
  geom_boxplot()+ geom_jitter()+
  theme_classic()+scale_y_continuous(limits = c(0,30))+
  labs(x=NULL, y="Total Number Microglia")

#make this plot an object so you can add to it later on
plot <-ggplot(count1, aes(x=genotype, y=Number_MiG))+
  geom_boxplot()+ geom_jitter()+
  theme_classic()+scale_y_continuous(limits = c(0,30))+
  labs(x=NULL, y="Total Number Microglia")

#stats

#We want to compare differences in the number of microglia among the 3 different genotypes

#work through ANOVA assumptions

## 1. Check homogeneity of variances using Levene's Test

library(car)


levene.test(Number_MiG ~ genotype, data=count1) #won't work- selected incorrect test

leveneTest(Number_MiG ~ genotype, data=count1) #works
#significant p-value, homogeneity of Variance violated

## 2. Check for normality using Shapiro-Wilk Test
res.aov <- aov(Number_MiG ~ genotype, data=count1)
aov_residuals <- residuals(object = res.aov)
shapiro.test(x = aov_residuals )

#data is normally distributed, but assumption 1 still violated


#one-way test to relax assumption 1
oneway.test(Number_MiG ~ genotype, data=count1)

pairwise.t.test(count1$Number_MiG, count1$genotype,
                p.adjust.method = "bonf", pool.sd = FALSE)

#move on to non-parametric alternatives

#Kruskal test- are the 3 groups different?
kruskal.test(Number_MiG ~ genotype, data=count1)

#Post-hoc Dunn Test- where are these differences?

library(FSA)
dunnTest(Number_MiG ~ genotype, data=count1, method = "bonferroni")


#add significance bars to plot
library(rstatix)
library(ggpubr)

#create a dataframe with output of dunn test including asteriks for significance
stat.test <- count1 %>% dunn_test(Number_MiG ~ genotype,
                                  p.adjust.method = "bonferroni")
#output should match "dunnTest" output from above

#add xy position - tells R where to put significance bars on your plot
stat.test <- stat.test %>% add_xy_position(x = "genotype")

#add significance bars to plot we created before
plot + stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01,
                              y.position = c(27,30, 10), hide.ns = FALSE)
#y.position tells R where to put these bars along the y axis of your plot. 

#Your turn! 

#Import dataset "RS4_count_data2.csv"

#Make plots and run stats for Total_TUNEL, TUNEL_MG_engulf, TUNEL_nonMG, TUNEL_MG_dying, and Phagocytic_Index

#Good luck!

