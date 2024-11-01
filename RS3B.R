setwd("~/Mitchell Lab Stuff/Data")

raw.data <-read.csv("Ashley_data_R.csv")

head(raw.data)

library(dplyr)

grouped.data <-group_by(raw.data, treatment, group)

qPCR_sum <-summarise(grouped.data, 
                     X18s_avg=mean(x18s_ct),
                     bact_avg=mean(bact_ct),
                     gm_avg=mean(geomean_ct),
                     sib_avg=mean(sibusm_ct))

# delta ct calculation

qPCR_sum$sibX18s_dct <- with(qPCR_sum, sib_avg-X18s_avg)
qPCR_sum$sibbact_dct <- with(qPCR_sum, sib_avg-bact_avg)
qPCR_sum$sibgm_dct <- with(qPCR_sum, sib_avg-gm_avg)

#ddct calculation

control_dct<-subset(qPCR_sum, treatment=='DMSO')

qPCR_sum$sibX18s_avgdct <-with(control_dct, mean(sibX18s_dct)) 
qPCR_sum$sibbact_avgdct <-with(control_dct, mean(sibbact_dct))  
qPCR_sum$sibgm_avgdct <-with(control_dct, mean(sibgm_dct))  

qPCR_sum$ddct_sib18s <- with(qPCR_sum, sibX18s_dct-sibX18s_avgdct)
qPCR_sum$ddct_sibbact <- with(qPCR_sum, sibbact_dct-sibbact_avgdct)
qPCR_sum$ddct_sibgm <- with(qPCR_sum, sibgm_dct-sibgm_avgdct)

#fold change

qPCR_sum$fc_sib18s <-with(qPCR_sum, 2^-(ddct_sib18s))
qPCR_sum$fc_sibbact <-with(qPCR_sum, 2^-(ddct_sibbact))
qPCR_sum$fc_sibgm <-with(qPCR_sum, 2^-(ddct_sibgm))

#fc range

qPCR_all<-ungroup(qPCR_sum)

fc_rangesib <-summarise(qPCR_all, range_18s=range(fc_sib18s),
                          range_bact=range(fc_sibbact),
                          range_gm=range(fc_sibgm))

fc_rangesib


#plots

library(ggplot2)

p1_sib <-ggplot(qPCR_sum, aes(x=treatment, y=fc_sib18s)) +
  geom_boxplot(outlier.shape = NA)+ geom_jitter(shape=16, position = position_jitter(0.1)) +
  theme_classic() +ylab("Fold Change") + xlab("Sample")+
  labs(title = "Sibusm relative to 18s")+
  scale_y_continuous(limits = c(0,4))

p1_sib

p2_sib <-ggplot(qPCR_sum, aes(x=treatment, y=fc_sibbact)) +
  geom_boxplot(outlier.shape = NA)+ geom_jitter(shape=16, position = position_jitter(0.1)) +
  theme_classic() +ylab("Fold Change") + xlab("Sample")+
  labs(title = "Sibusm relative to Bact")+
  scale_y_continuous(limits = c(0,4))
p2_sib

p3_sib <-ggplot(qPCR_sum, aes(x=treatment, y=fc_sibgm)) +
  geom_boxplot(outlier.shape = NA)+ geom_jitter(shape=16, position = position_jitter(0.1)) +
  theme_classic() +ylab("Fold Change") + xlab("Sample")+
  labs(title = "Sibusm relative to Geomean")+
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

