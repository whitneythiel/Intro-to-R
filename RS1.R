# Intro to R Session 1 - Fundamentals

#Parts of workspace: 
##Script, console, Environment/History, Files/Plots/Packages/Help

# This is a comment!
This is your code

# Common data types

# Integers (whole numbers)
9
# Numerical (continuous numbers)
9.5
# Logical
TRUE or FALSE
# Character / String (alphabetic- categorical)
"wildtype", "heterozygote","mutant"
# Factor (alphanumeric- categorical)
"Male" or "Female"

### Mathematical operators ###
### These return numerical values

# Addition 
2 + 2
# Subtraction
2 - 1
# Multiplication
5  * 5
# Division 
10 / 2
# Exponentiation
** or ^
  2**2
3^3


### These assess how one value relates to another so return Logical answers

# Greater than
4 > 2 
# Less than
4 < 2
# Equal to
10  == 5
# Not equal to
10 != 10
# Greater than OR equal to
10 >= 10
# Less than OR equal to
10 <= 10

### Mathematical Functions ###

# Mean
mean(c(1,2,3,4,5)) #c() == cobine
mean(1:5) # : == through
# Median
median(1:10)
median(2:25)
?median
# Standard Deviation
sd(1:10)
# Minimum value
min(1:10)
# Maximum value
max(1:10)
# Range
range(1:10)
# Round
round(5.91234567,digits = 5)

### R Objects ###

### R can create and handle several different types of object that differ in their
# complexity and function

#Once you create an object, it will show up in your environment

## Variables ##
# The most simple R objects are often called variables
x <- 2

x

y = 3

y

# We can use all of the mathematical operators upon variables like they are numbers
x + y
y^x
y > x


# Variables can also take the form of any other data type
my.logical <- TRUE

my.character <- "mutant"

## Vectors ##
## Vectors are objects that contain more than one value, in a one dimensional format

steps.taken <- c(4000,5000,8000,10000,4000)

steps.taken

days.of.the.week <- c("Monday","Tuesday", "Wednesday", "Thursday", "Friday")

days.of.the.week


### Data frames ###

# Data containing different data types can be read by R as a data frame
# We can manually compile data frames from multiple vectors in R

# Assign vectors
City <- c("London","Paris","Rio","Lima")
City
Population <- c(9000000, 2000000,13000000,10000000)
Population
Continent <- c("Europe","Europe","South America","South America")
Continent

Cities.dataframe <- data.frame(City, Population, Continent)

Cities.dataframe

# We can use the $ sign to select individual columns

Cities.dataframe$City
Cities.dataframe$Population

# We can add rows and columns to data frames just like with matricies

# Add a column to a data frame 
Cities.dataframe$Hemisphere <- c("Northern", "Northern", "Southern", "Southern")

Cities.dataframe

# Add a row

# Create a new data frame containing just the new row

Berlin <- data.frame(City = "Berlin", Population = 4000000, Continent = "Europe", Hemisphere = "Northern")

# Bind it to full data frame
?rbind

Cities.dataframe <- rbind(Cities.dataframe,Berlin)

Cities.dataframe

## Very basic plotting from data frames 
plot(Cities.dataframe$Population ~ Cities.dataframe$City) 

plot(Population ~ City, Cities.dataframe)

plot(Population ~ Continent, Cities.dataframe)

plot(Population ~ Hemisphere, Cities.dataframe) #Will throw an error

#Using the str() function on a database will tell you how R is interpreting the data

str(Cities.dataframe)

#Reassign Hemisphere to a Factor and rerun plot 

Cities.dataframe$Hemisphere <- as.factor(Cities.dataframe$Hemisphere)

Cities.dataframe$City <- as.factor(Cities.dataframe$City)

str(Cities.dataframe)

plot(Population ~ Hemisphere, Cities.dataframe) #Will now work

# We can use the subset() function to select only rows of interest

European.Cities <- subset(Cities.dataframe, Continent == "Europe")



European.Cities

Small.Cities <- subset(Cities.dataframe, Population < 5000000)

Small.Cities

Cities.dataframe <- subset(Cities.dataframe, City!= "Berlin")

Cities.9mil <- subset(Cities.dataframe, Population == 9000000 )



# As an alternative to creating a dataframe, you can also import data 
# This is probably the most common way you will use to get data into R

#First, set your working directory. This tells R where is your data on your computer

#Session, set working directory, choose directory
setwd("~/Mitchell Lab Stuff/Data")

#import your data and make it an object so you can manipulate it 
mydata <- read.csv("RS1_sample_data.csv", fileEncoding = "UTF-8-BOM")

mydata

#install the package you want to use to work with your data
## You just have to do this the first time you use a package
install.packages("dplyr")

#load the package into the R session.
## you have to do this everytime you want to use a package
## you can also select the package in the "packages tab" to load a package.
library(dplyr)


#You can use the head function to do a quickview of your data
# Will pull up the column headings and 1st 6 rows of data
head(mydata)

#You can use str to see how R is interpreting your data
str(mydata)

#You can change categorical varibles to factors
mydata$genotype <- as.factor(mydata$genotype)

#view levels
levels(mydata$genotype)

#reorder levels
# This is helpful if you want them to appear in a certain order in a plot

mydata$genotype <- factor(mydata$genotype , levels=c("wt", "het", "mut"))

levels(mydata$genotype)

#filter to look at just experiement 1
#can use piping tool to filter 
mydata %>%
  filter(Experiment=="1")

#or
filter(mydata, Experiment=="1")

# you can make either of these an object
# if you want to use the filtered data set for additional calculations or a plot

mydata.filtered <- filter(mydata, Experiment=="1")

#arrange descending sample number

mydata %>%
  filter(Experiment=="1") %>%
  arrange(desc(sample))


# If we only wanted to calculate the mean 18s CT value for all mutants from experiement 1
# and add a column with this calculation

mydata %>% 
  filter(Experiment == "1", genotype=="mut") %>%
  mutate(mean_18s_ct = mean(X18s_ct))

#We can also make a summary tables
## Let's summarize the mean ct for 18s, bact, and geomean
# but let's group our data by experiment, dpf and genotype
mydata %>% 
  group_by(Experiment, dpf, genotype) %>%
  summarise(mean_18s_ct = mean(X18s_ct),
            mean_bact_ct = mean(bact_ct),
            mean_geom_ct= mean(geomean_ct))

# This table can also be made into an object

Summary.mydata <- mydata %>% 
  group_by(Experiment, dpf, genotype) %>%
  summarise(mean_18s_ct = mean(X18s_ct),
            mean_bact_ct = mean(bact_ct),
            mean_geom_ct= mean(geomean_ct))

Summary.mydata

# You can also export this table you made in R to excel
install.packages("WriteXL")
library(writexl)

write_xlsx(Summary.mydata, "Summary.mydata.xlsx", col_names = TRUE)

#Sneak peak at next week

library(ggplot2)

ggplot(mydata)+
  geom_point(aes(x=genotype, y=X18s_ct))




