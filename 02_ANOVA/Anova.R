####Some simple Anovas####
#Load the ChickWeight data
data(ChickWeight)


#Inspect the dataset and get familiar with it
attach(ChickWeight)

head(ChickWeight)      # first 6 rows — quick look at structure
str(ChickWeight)       # data types of each column (numeric, factor, character, etc.)
summary(ChickWeight)   # min/max/mean/quartiles for each column
dim(ChickWeight)       # how many rows and columns
names(ChickWeight)     # column names

unique(Diet)

#Visualize the weight per Chick in a boxplot

boxplot(weight,
        names = c("Weight"),
        main = "Boxplot of chicken weight",
        ylab = "weight",
        col = c("lightblue"))

#Visualize the weight of the Diets in a boxplot

boxplot(weight ~ Diet,
        data = ChickWeight,
        main = "Boxplot of chicken weight by diet",
        xlab = "Diet",
        ylab = "Weight",
        col = c("lightblue", "lightgreen", "lightyellow", "lightpink"))


#Now make an Anova where you test whether the weight differs between the different Diets

anova_result <- aov(weight ~ Diet, data = ChickWeight)


#Inspect the summary of the model. Is it significant. How much Variance is explained?
summary(anova_result)

# Yes it does. variance between groups is 10.81 bigger than inbetween group vaiance with very low p value. 


#Is the weight actually normally distributed

hist(weight, 
     main = "Histogram of Chick Weight", 
     xlab = "Weight",
     breaks = 7)


#Is the weight maybe log-normally distributed

hist(log(weight), 
     main = "Histogram of Chick Weight", 
     xlab = "Weight",
     breaks = 9)

#How is the prevalence of the factor variables?
#Many factor levels are uneven. Make a lm model, look at the summary, then make atype III Anova
###Well, does not seem to make a difference)

#Load the InsectSprays 

#Inspect the data


#Make a boxplot of the data
#Now check out the residuals per factor variable
#Now compared the factor levels in a posthoc test


install.packages("car")   # only needed once
library(car)

####Some simple Anovas####
#Load the ChickWeight data
data(ChickWeight)
#Inspect the dataset and get familiar with it
names(ChickWeight)
str(ChickWeight)
summary(ChickWeight)

#Visualize the weight per Chick in a boxplot
boxplot(weight~Chick,data=ChickWeight)
#Visualize the weight of the Diets in a boxplot
boxplot(weight~Diet,data=ChickWeight)
#Now make an Anova where you test whether the weight differs between the different Diets
model<-aov(weight~Diet,data=ChickWeight)
#Inspect the summary of the model. Is it significant. How much Variance is explained?
summary(model)
#Is the weight actually normally distributed
hist(ChickWeight$weight)
#Is the weight maybe log-normally distributed
hist(log(ChickWeight$weight))
#How is the prevalence of the factor variables?
table(ChickWeight$Chick)
table(ChickWeight$Time)
table(ChickWeight$Diet)
#Many factor levels are uneven. Make a type III Anova
model2<-lm(weight~Diet,data=ChickWeight)
summary(model2)
Anova(model2,type="III")
summary(model)
###Well, does not seem to make a difference)


#Load the InsectSprays 
data(InsectSprays)
#Inspect the data
str(InsectSprays)
summary(InsectSprays)

model3<-aov(InsectSprays$count~InsectSprays$spray)
summary(model3)
#Make a boxplot of the data
boxplot(InsectSprays$count~InsectSprays$spray)
#Now check out the residuals per factor variable
boxplot(resid(model3)~InsectSprays$spray)
#Now compared the factor levels in a posthoc test
TukeyHSD(model3)