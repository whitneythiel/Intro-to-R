#Intro to R Session 2: Intro to plots with ggplot2

#ggplot2

#Load packages
library(dplyr)
library(ggplot2)


# ggplot2 is a great package for creating publication quality graphics.
# The syntax can look intimadating, so today we will go over the basics and
# try a few different types of plots.

# The first dataset we will be working with is a test of a new qPCR primer pair.

#First, set your working directory & load your dataset

setwd("~/path/Data")

read.csv("RS2_sample_data1.csv",  
                       fileEncoding="UTF-8-BOM")

# We will begin by creating a basic scatter plot 

# ggplot works in layers, and each layer is seperated by a +, 
# the first layer we need to add is to initate our ggplot:
# This first layer is the ggplot() function, which then contains the data we are analysing. 

ggplot(Primer_data)  

# Next, we must speficy which type of geometric we want our graph to take.
# we do this using the geom_X() function
# Because we are interested in generating a scatter plot we will use the geom_point() function.

ggplot(Primer_data) +
  geom_point(aes(x=log_temp, y = mean_ct))

plot(mean_ct ~log_temp, data = Primer_data)

# We can add a trendline by adding another layer to our plot
# Here I chose to add a linear trendline using geom_smooth
# Since we want to use the same x and y variables for both geom_point & geom_smooth,
# we can move the aes() designation up to our base plot -ggplot().


ggplot(Primer_data, aes(x=log_temp, y = mean_ct)) +
  geom_point()+
  geom_smooth(method = lm, se=FALSE)

ggplot(Primer_data, aes(x=log_temp, y = mean_ct)) +
  geom_point()+
  geom_smooth(method = lm, se=TRUE)

#We can also do some stats to find the r^2, slope, and intercept of our trendline.
#First we'll create a linear model.
Primer.lm<-lm(mean_ct ~ log_temp, data = Primer_data)

#Then we can extract the r^2 from our model
summary(Primer.lm)$r.squared

#and the slope
summary(Primer.lm)$coeff[2]

#and the intercept
summary(Primer.lm)$coeff[1]

#We can use the information from the model in equations
#For instance, we can calculate primer efficienty using the following equation

slope <- summary(Primer.lm)$coeff[2]

(10^(-1/slope)-1)*100



# Now we'll work with a new dataset to generate some other types of plots

#Import the second dataset. 
gene_data <-read.csv("RS2_sample_data2.csv")

head(gene_data)

#For the first couple plots we'll work with the variable Number_MiG
#First we'll visualize the distruction of the data using a histogram
ggplot(gene_data, aes(x=Number_MiG)) + 
  geom_histogram(na.rm = TRUE)

#We can change the binwidth to get a more informative plot
ggplot(gene_data, aes(x=Number_MiG)) + 
  geom_histogram(na.rm = TRUE, binwidth = 5)

#We can make this plot more visually appealing by:
#Adding some color to the bars and changing the theme 

ggplot(gene_data, aes(x=Number_MiG)) + 
  geom_histogram(na.rm = TRUE, binwidth = 5, color="black", fill="#1b9e77")+
  theme_classic()

#We can use violin plots to visualize distribution by genotype
ggplot(gene_data, aes(x=genotype, y=Number_MiG))+
  geom_violin(trim = TRUE, na.rm = TRUE)

#Add a point for the mean
ggplot(gene_data, aes(x=genotype, y=Number_MiG))+
  geom_violin(trim = FALSE, na.rm = TRUE)+ 
  stat_summary(fun=mean, shape=5, color="green", size=2)


#We can also make our baseplots an object (p) 

p<-ggplot(gene_data, aes(x=genotype, y=Number_MiG, color=genotype))+
  geom_violin(trim = FALSE, na.rm = TRUE)

#And then try adding different color palettes

p+ scale_color_brewer(palette="Dark2")

p+ scale_color_grey()+ theme_classic()


#Now let's work with a different variable to create a boxplot
#For this plot we'll work with Phagocytic Load
ggplot(gene_data, aes(x=genotype, y=phagocytic_load)) +
  geom_boxplot(na.rm = TRUE)

#reorder genotype levels for plots

gene_data$genotype <- factor(gene_data$genotype , levels=c("wt", "het", "mut"))

ggplot(gene_data, aes(x=genotype, y=phagocytic_load)) +
  geom_boxplot(na.rm = TRUE)

#Now let's make it pretty

#add a theme
ggplot(gene_data, aes(x=genotype, y=phagocytic_load)) +
  geom_boxplot(na.rm = TRUE)+
  theme_minimal()

#adjust the y axis

ggplot(gene_data, aes(x=genotype, y=phagocytic_load)) +
  geom_boxplot(na.rm = TRUE)+
  theme_classic()+
  scale_y_continuous(limits = c(0,3))

# we can also add more breaks to the y axis if we want

ggplot(gene_data, aes(x=genotype, y=phagocytic_load)) +
  geom_boxplot(na.rm = TRUE)+
  theme_classic()+
  scale_y_continuous(limits = c(0,3), breaks = c(0, 0.5,1,1.5,2,2.5,3))

ggplot(gene_data, aes(x=genotype, y=phagocytic_load)) +
  geom_boxplot(na.rm = TRUE)+
  theme_classic()+
  scale_y_continuous(limits = c(0,3), breaks = seq(0,3,0.5))

# We can even make our y axis breaks a vector and then use that vector in our plot

y.breaks <-c(0, 0.5,1,1.5,2,2.5,3)

ggplot(gene_data, aes(x=genotype, y=phagocytic_load)) +
  geom_boxplot(na.rm = TRUE)+
  theme_classic()+
  scale_y_continuous(limits = c(0,3), breaks = y.breaks)

#If we want, we could remove the outlier points
ggplot(gene_data, aes(x=genotype, y=phagocytic_load)) +
  geom_boxplot(na.rm = TRUE, outlier.shape = NA)+
  theme_classic()+
  scale_y_continuous(limits = c(0,3))

#Change axis labels & add title
ggplot(gene_data, aes(x=genotype, y=phagocytic_load)) +
  geom_boxplot(na.rm = TRUE)+
  theme_classic()+
  scale_y_continuous(limits = c(0,3))+
  labs(title= "Phagocytic Load in Eyes", x="Genotype", y="Phagocytic Load")

#Change the color of the boxes by groups. Example, genotype

ggplot(gene_data, aes(x=genotype, y=phagocytic_load, color=genotype)) +
  geom_boxplot(na.rm = TRUE)+
  theme_classic()+
  scale_y_continuous(limits = c(0,3))+
  labs(title= "Phagocytic Load in Eyes", x="Genotype", y="Phagocytic Load")

# You'll see ggplot automatically added a legend.
# We can move it or remove this if we don't want it. 

#move it to the top & rename it

ggplot(gene_data, aes(x=genotype, y=phagocytic_load, color=genotype)) +
  geom_boxplot(na.rm = TRUE)+
  theme_classic()+
  scale_y_continuous(limits = c(0,3))+
  labs(title= "Phagocytic Load in Eyes", x="Genotype", y="Phagocytic Load",
       color="Fish Genotype")+
  theme(legend.position = "top")

#remove the legend

ggplot(gene_data, aes(x=genotype, y=phagocytic_load, color=genotype)) +
  geom_boxplot(na.rm = TRUE)+
  theme_classic()+
  scale_y_continuous(limits = c(0,3))+
  labs(title= "Phagocytic Load in Eyes", x="Genotype", y="Phagocytic Load")+
  theme(legend.position = "none")

#We can add points to our plots with geom_dotplot or geom_jitter
#going back to the black and white version for this

ggplot(gene_data, aes(x=genotype, y=phagocytic_load)) +
  geom_boxplot(na.rm = TRUE)+
  theme_classic()+
  scale_y_continuous(limits = c(0,3))+
  labs(title= "Phagocytic Load in Eyes", x="Genotype", y="Phagocytic Load")+
  geom_jitter(position = position_jitter(0.1), na.rm = TRUE)

# We can change the color and shape of the points
#Let's change the color to embryo number- looks weird- continuous color


ggplot(gene_data, aes(x=genotype, y=phagocytic_load)) +
  geom_boxplot(na.rm = TRUE)+
  theme_classic()+
  scale_y_continuous(limits = c(0,3))+
  labs(title= "Phagocytic Load in Eyes", x="Genotype", y="Phagocytic Load")+
  geom_jitter(position = position_jitter(0.1), na.rm = TRUE,
              aes(color= embryo))

str(gene_data)

#Embryo is set as an integer, not a factor we need to change this
gene_data$embryo <-as.factor(gene_data$embryo)

#try the plot again- looks great! Separate colors for each embryo
ggplot(gene_data, aes(x=genotype, y=phagocytic_load)) +
  geom_boxplot(na.rm = TRUE)+
  theme_classic()+
  scale_y_continuous(limits = c(0,3))+
  labs(title= "Phagocytic Load in Eyes", x="Genotype", y="Phagocytic Load")+
  geom_jitter(position = position_jitter(0.1), na.rm = TRUE,
              aes(color= embryo))

# now let's also change the shape of the points to batch number

gene_data$batch <-as.factor(gene_data$batch)

ggplot(gene_data, aes(x=genotype, y=phagocytic_load)) +
  geom_boxplot(na.rm = TRUE)+
  theme_classic()+
  scale_y_continuous(limits = c(0,3))+
  labs(title= "Phagocytic Load in Eyes", x="Genotype", y="Phagocytic Load")+
  geom_jitter(position = position_jitter(0.1), na.rm = TRUE,
              aes(color= embryo, shape=batch))

# now you'll see there are two legends, one for color and one for shape.


#How to save a plot

#First make the plot you want to save an object
#For this final plot, I removed the points marking the outliers.
# I also removed the x-axis label
# and I am changing the text size

tiff("Final_Plot 2.tiff", width = 2.5, height = 2, units = "in", dpi=300)
ggplot(gene_data, aes(x=genotype, y=phagocytic_load)) +
  geom_boxplot(na.rm = TRUE, outlier.shape = NA)+
  theme_classic()+
  scale_y_continuous(limits = c(0,3))+
  labs(title= "Phagocytic Load in Eyes", x=NULL, y="Phagocytic Load")+
  geom_jitter(position = position_jitter(0.1), na.rm = TRUE)+
  theme(text= element_text(size = 5))
dev.off()

Final_plot

#This will automatically save the plot to your working directory
ggsave("Final_plot.tiff", Final_plot, width = 2.5, height = 2, units = "in", dpi=300)

ggsave("Final_plot.pdf", Final_plot, width = 2.5, height = 2, units = "in", dpi=300)

#Let's try some qiuck stats
#We'll run an anova to see if phagocytic load differs among genotypes

library(car)

#First, check assumptions for ANOVA

## 1. Check homogeneity of variances using Levene's Test
leveneTest(phagocytic_load ~ genotype, data = gene_data)
## 2. Check for normality using Shapiro-Wilk Test
res.aov <- aov(phagocytic_load ~ genotype, data = gene_data)
aov_residuals <- residuals(object = res.aov)
shapiro.test(x = aov_residuals )

plot(res.aov, 1)
plot(res.aov, 2)

#normality is violated so we can use a non-parametric test

kruskal.test(phagocytic_load ~ genotype, data = gene_data)
















