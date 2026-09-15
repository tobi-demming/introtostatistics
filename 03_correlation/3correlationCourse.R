
# data import of file regression
regression <- read.table("03_correlation/regression.txt", header=T)

#Set your working directory
("regression.txt",header=T) #Load the dataset regression
 #Attach into higher memory

attach(regression)

 #Variable names
colnames(regression)

growth <- growth
tannin <- tannin

 #Data structure
str(regression)

#Dimensions
dim(regression)

# data examination through plotting the two variables
plot(tannin, growth,
     main = "Growth and Tannin",
     xlab = "Tannin Concentration",
     ylab = "Growth",
     col= "red")

 #Plot the data 

# Check for normal distribution by making a histogram

hist(growth, main = "Histogram of Growth", xlab = "Growth")
hist(tannin, main = "Histogram of Tannin", xlab = "Tannin")


 #Does this look normally distributed
#or this?
#You can also mkae a shapiro test, if you want
#Seems normally distributed as wells well

# parametric correlation
#Simple correlation coefficient
#And now the whole package, use the cor.test

# non parametric correlation
#For data not normally distributed, make a spearman correlation
#Pearson, for normally distributed data, use the standard setting
#Usually useless, but try also the kendall setting and compare

#Load the swiss data

#Load the swiss data
data(swiss)

#Check out the data
head(swiss)

#Inspect the data
str(swiss)
dim(swiss)
colnames(swiss)

#Check out the summary
summary(swiss)

#Make correlations of all the variables
cor_matrix <- cor(swiss)
round(cor_matrix, 2)

#Make a correlation plot of all variables
library(corrplot)
corrplot(cor_matrix, method = "color", type = "upper",
         order = "hclust",
         addCoef.col = "black", number.cex = 0.6,
         tl.col = "black", tl.srt = 45,
         diag = FALSE)

#Now find testable relations in the dataset and make cor.test with these
# (inspect cor_matrix / the plot above first, then run cor.test on the pairs
#  that show the strongest relationships)

cor.test(swiss$Fertility, swiss$Education)
cor.test(swiss$Fertility, swiss$Agriculture)
cor.test(swiss$Examination, swiss$Education)


#last dataset, check out varechem

library(vegan)
data(varechem)

#Check out the dimensions of the dataset
dim(varechem)


#Inspect the dataset
head(varechem)
str(varechem)
summary(varechem)

 #Make correlations of all relations and identify all larger cor 0.5

cor_matrix <- cor (varechem)
cor_matrix
round(cor_matrix, 2)

#Make a pairs-plot of all variables

install.packages("corrplot") # only has to be run once i believe
library(corrplot)

corrplot(cor_matrix, method = "color", type = "full",
         order = "hclust",
         addCoef.col = "black", number.cex = 0.6,
         tl.col = "black", tl.srt = 45,
         tl.pos = "lt")









