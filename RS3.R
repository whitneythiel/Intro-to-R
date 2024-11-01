setwd("~/Mitchell Lab Stuff/Data")

raw <- read.csv("Ashley_data_R.csv")

library("dplyr")

raw_group <-group_by(raw, treatment, group)

#Make a new table with the average ct of technical replicates

qPCR_sum <-summarise(raw_group, 
                     X18s_avg=mean(x18s_ct),
                     bact_avg=mean(bact_ct),
                     gm_avg=mean(geomean_ct),
                     sib_avg=mean(sibusm_ct))

#calculate delta ct

qPCR_sum$sib18s_dct <- with(qPCR_sum, sib_avg-X18s_avg)
qPCR_sum$sibbact_dct <- with(qPCR_sum, sib_avg-bact_avg)
qPCR_sum$sibgm_dct <- with(qPCR_sum, sib_avg-gm_avg)

#average dCt of control group only

control_dct<-subset(qPCR_sum, treatment=='DMSO')

qPCR_sum$sib18s_avgdct <-with(control_dct, mean(sib18s_dct))
qPCR_sum$sibbact_avgdct <-with(control_dct, mean(sibbact_dct))
qPCR_sum$sibgm_avgdct <-with(control_dct, mean(sibgm_dct))

#delta delta ct

qPCR_sum$ddct_sib18s <- with(qPCR_sum, sib18s_dct-sib18s_avgdct)
qPCR_sum$ddct_sibbact <- with(qPCR_sum, sibbact_dct-sibbact_avgdct)
qPCR_sum$ddct_sibgm <- with(qPCR_sum, sibgm_dct-sibgm_avgdct)

#fold change

qPCR_sum$fc_sib18s <-with(qPCR_sum, 2^-(ddct_sib18s))
qPCR_sum$fc_sibbact <-with(qPCR_sum, 2^-(ddct_sibbact))
qPCR_sum$fc_sibgm <-with(qPCR_sum, 2^-(ddct_sibgm))


#done! 

#make some plots

#find fold change ranges for y-axis
qPCR_all<-ungroup(qPCR_sum)

fc_rangesib <-summarise(qPCR_all, range_18s=range(fc_sib18s),
                          range_bact=range(fc_sibbact),
                          range_gm=range(fc_sibgm))

fc_rangesib

#looks like a y-axis of 0-4 would work for all 3 plots

library(ggplot2)

p1_sib <-ggplot(qPCR_sum, aes(x=treatment, y=fc_sib18s)) +
  geom_boxplot()+ geom_jitter(shape=16, position = position_jitter(0.1)) +
  theme_classic() +ylab("Fold Change") + xlab("Sample")+
  labs(colour="Experiment", title = "Sibusm relative to 18s")+
  scale_y_continuous(limits = c(0,4))

p1_sib

p2_sib <-ggplot(qPCR_sum, aes(x=treatment, y=fc_sibbact)) +
  geom_boxplot()+ geom_jitter(shape=16, position = position_jitter(0.1)) +
  theme_classic() +ylab("Fold Change") + xlab("Sample")+
  labs(colour="Experiment", title = "Sibusm relative to bact")+
  scale_y_continuous(limits = c(0,4))

p2_sib

p3_sib <-ggplot(qPCR_sum, aes(x=treatment, y=fc_sibgm)) +
  geom_boxplot()+ geom_jitter(shape=16, position = position_jitter(0.1)) +
  theme_classic() +ylab("Fold Change") + xlab("Sample")+
  labs(colour="Experiment", title = "Sibusm relative to geomean")+
  scale_y_continuous(limits = c(0,4))

p3_sib

#stats

#check normality
shapiro.test(qPCR_sum$ddct_sib18s)
shapiro.test(qPCR_sum$ddct_sibbact)
shapiro.test(qPCR_sum$ddct_sibgm)

#check homogeniety of variances
var.test(ddct_sib18s ~ treatment, data = qPCR_sum)
var.test(ddct_sibbact ~ treatment, data = qPCR_sum)
var.test(ddct_sibgm ~ treatment, data = qPCR_sum)

#perform t-test
t.test(ddct_sib18s ~ treatment, data = qPCR_sum, var.equal = TRUE)
t.test(ddct_sibbact ~ treatment, data = qPCR_sum, var.equal = TRUE)
t.test(ddct_sibgm ~ treatment, data = qPCR_sum, var.equal = TRUE)

