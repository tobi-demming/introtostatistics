
splityield<-read.table(text="yield	block	irrigation	density	fertilizer
           90	A	control	low	N
           95	A	control	low	P
           107	A	control	low	NP
           92	A	control	medium	N
           89	A	control	medium	P
           92	A	control	medium	NP
           81	A	control	high	N
           92	A	control	high	P
           93	A	control	high	NP
           80	A	irrigated	low	N
           87	A	irrigated	low	P
           100	A	irrigated	low	NP
           121	A	irrigated	medium	N
           110	A	irrigated	medium	P
           119	A	irrigated	medium	NP
           78	A	irrigated	high	N
           98	A	irrigated	high	P
           122	A	irrigated	high	NP
           83	B	control	low	N
           80	B	control	low	P
           95	B	control	low	NP
           98	B	control	medium	N
           98	B	control	medium	P
           106	B	control	medium	NP
           74	B	control	high	N
           81	B	control	high	P
           74	B	control	high	NP
           102	B	irrigated	low	N
           109	B	irrigated	low	P
           105	B	irrigated	low	NP
           99	B	irrigated	medium	N
           94	B	irrigated	medium	P
           123	B	irrigated	medium	NP
           136	B	irrigated	high	N
           133	B	irrigated	high	P
           132	B	irrigated	high	NP
           85	C	control	low	N
           88	C	control	low	P
           88	C	control	low	NP
           112	C	control	medium	N
           104	C	control	medium	P
           91	C	control	medium	NP
           82	C	control	high	N
           78	C	control	high	P
           94	C	control	high	NP
           60	C	irrigated	low	N
           104	C	irrigated	low	P
           114	C	irrigated	low	NP
           90	C	irrigated	medium	N
           118	C	irrigated	medium	P
           113	C	irrigated	medium	NP
           119	C	irrigated	high	N
           122	C	irrigated	high	P
           136	C	irrigated	high	NP
           86	D	control	low	N
           78	D	control	low	P
           89	D	control	low	NP
           79	D	control	medium	N
           86	D	control	medium	P
           87	D	control	medium	NP
           85	D	control	high	N
           89	D	control	high	P
           83	D	control	high	NP
           73	D	irrigated	low	N
           114	D	irrigated	low	P
           114	D	irrigated	low	NP
           109	D	irrigated	medium	N
           131	D	irrigated	medium	P
           126	D	irrigated	medium	NP
           116	D	irrigated	high	N
           136	D	irrigated	high	P
           133	D	irrigated	high	NP", header=TRUE)



#Inspect dataset
attach(splityield)

head(splityield)
str(splityield)


#Make histogram of the variable yields

hist(yield)

#Make individual boxplots or yields against all factor variables, one by one
boxplot(yield ~ block,
        data = splityield,
        main = "Yield by Block",
        xlab = "Block", ylab = "Yield")

boxplot(yield ~ irrigation,
        data = splityield,
        main = "Yield by Irrigation",
        xlab = "Irrigation", ylab = "Yield")

boxplot(yield ~ density,
        data = splityield,
        main = "Yield by Density",
        xlab = "Density", ylab = "Yield")

boxplot(yield ~ fertilizer,
        data = splityield,
        main = "Yield by Fertilizer",
        xlab = "Fertilizer", ylab = "Yield")


#Make an Anova with three way interactions and include block as an error term in an Anova
model1 <- aov(yield ~ irrigation * density * fertilizer + Error(block),
             data = splityield)
summary(model1)


#Interpret the summary
#Remove the non-significant three way interaction
#Look at the summary of the model, then remove the least non-significant 2-way interaction

#reduce the model further to contain only significant interactions

#Plot residuals of the final model in a histogram


#Now redo the same model reduction procedure again without the Error term

#Compare the final model with the previous model that included the Error term using AIC

#Now try the same procedure with the command lm

#Compare the AICs of all lm models


######################
#Another example: npk

#Inspect the dataset
#Create a full model with aov and block as an Error, since the dataset is balanced

#Now the same without the Error term

#Reduce the model, and compare the minimum adequate model with Error term with the same model without Error term




###Another example#####
####barley#############
#attach the dataset barley

#Inspect the dataset
#Are the variable combinations of factor variables balanced?
#Are the variable combinations of factor variables balanced?
#Are the variable combinations of factor variables balanced?

#Make normal glm null model
#Check out the summary
#Then build a GLM full model with 3 way interactions
#Hm, there does not seem to be enough statistical power #Three way interactions have NAs
#try the Anova command from car
#Still not enough statistical power
#Two way interactions, then, build the model
#Check out II way Anova: Looking good, but interaction between variety and year not significant
#reduce model thus further, exclude least significant interaction
#Only significant terms remain
#Identify individual factor levels by looking at the summary
#Compare p-values of reduced models vs AICs of reduced models
vif(reducedtwowaymodel,type = 'predictor') #Use the variance inflation factor to check for redundancy in the predictor variables