library(bbmle)
library(AICcmodavg)
library(vegan)
#load the dataset varechem, but do not attach it- Instead, write it into a new dataframe
dat <- varechem
data(varespec)
colnames(varechem)

#scale all variables within your new dataframe
dat$N<-scale(varechem$N) #Ignore warning
dat$P<-scale(varechem$P)
dat$K<-scale(varechem$K)
dat$Ca<-scale(varechem$Ca)
dat$Mg<-scale(varechem$Mg)
dat$S<-scale(varechem$S)
dat$Al<-scale(varechem$Al)
dat$Fe<-scale(varechem$Fe)
dat$Mn<-scale(varechem$Mn)
dat$Zn<-scale(varechem$Zn)
dat$Mo<-scale(varechem$Mo)
dat$Baresoil<-scale(varechem$Baresoil)
dat$Humdepth<-scale(varechem$Humdepth)
dat$pH<-scale(varechem$pH)

#Calculate the number of species/plot using the apply command
dat$divers<-apply(ifelse(varespec>0,1,0),1,sum)
hist(dat$divers)#Make a histogram of the number of spcies/plot
#Build five cand.models with glms with poisson distributions, where you combine several non-redundant predictor variables
cand.models<-list()
cand.models[[1]] <- glm(divers ~ N+ P + Fe,data=dat, family=poisson)
cand.models[[2]]<- glm(divers ~ Zn + K + Mo,data=dat, family=poisson)
cand.models[[3]] <- glm(divers ~ Baresoil+Humdepth+pH,data=dat, family=poisson)
cand.models[[4]] <- glm(divers ~ K + Ca + S,data=dat, family=poisson)
cand.models[[5]] <- glm(divers ~ Al + Mn + Mo,data=dat, family=poisson)


#Create a Vector of the names of the five candidate models
modnames<-paste("mod", 1:length(cand.models),sep="")
#Use aictab to create a table with all cand.models
aic.table<-aictab(cand.set=cand.models, modnames=modnames, sort=TRUE)
#Print the five models. Check which model had the lowest AIC 
print(aic.table,digits=4,LL=TRUE)


#Compare the best model with the null model
richnull<-glm(divers~1, family=poisson,data=dat)
richnull
AIC(richnull,cand.models[[3]])#All in vain! Null modell is better..



#Now a more independent task: dry.frog
##Mazerolle (2006) frog water loss example
#Build five candiatte models that make sense to you, and compare
#Use log_Mass_lost as a dependent variable in an lm
hist(dry.frog$log_Mass_lost)
data(dry.frog)
str(dry.frog)
head(dry.frog)

## Look at corr of variables to understand which ones are redundant
# select just the numeric candidate predictors
num_vars <- dry.frog[, c("SVL", "Initial_mass", "cent_Initial_mass", "Initial_mass2",
                         "Airtemp", "cent_Air", "Cloud", "Perc.cloud", 
                         "Wind_cat", "Wind")]

cor_matrix <- cor(num_vars, use = "pairwise.complete.obs")
round(cor_matrix, 2)

library(corrplot)

cor_matrix <- cor(num_vars, use = "pairwise.complete.obs")

corrplot(cor_matrix, 
         method = "circle",      # classic circle style (size + color = strength)
         type = "upper",         # only show upper triangle
         tl.col = "black",       # variable name text color
         tl.srt = 45,            # angle names 45° so they don't overlap
         tl.cex = 0.9,           # size of variable name text
         addCoef.col = NULL)     # leave NULL for pure classic look (no numbers inside)

##setup a subset of models of Table 1 # This time try it for yourself


##create a vector of names to trace back models in set

##generate AICc table
##round to 4 digits after decimal point and give log-likelihood





#Now the beetle data
#Use Mortality as a dependent variable, and Dose as an independent variable
##Burnham and Anderson (2002) flour beetle data
data(beetle)
##models as suggested by Burnham and Anderson p. 198          
Cand.set <- list( )
#Build three GLMS, one with logit, one with probit, one with cloglog as the binomial link function
#Add weights with the variable "Numer_tested"
Cand.set[[1]] <- glm(Mortality_rate ~ Dose, family =
                       binomial(link = "logit"), weights = Number_tested,
                     data = beetle)
Cand.set[[2]] <- glm(Mortality_rate ~ Dose, family =
                       binomial(link = "probit"), weights = Number_tested,
                     data = beetle)
Cand.set[[3]] <- glm(Mortality_rate ~ Dose, family =
                       binomial(link ="cloglog"), weights = Number_tested,
                     data = beetle)

##check c-hat
c_hat(Cand.set[[1]])
c_hat(Cand.set[[2]])
c_hat(Cand.set[[3]])
##lowest value of c-hat < 1 for these non-nested models, thus use
##c.hat = 1 

##set up named list
names(Cand.set) <- c("logit", "probit", "cloglog")

##compare models
##model names will be taken from the list if modnames is not specified
res.table <- aictab(cand.set = Cand.set, second.ord = FALSE)
##note that delta AIC and Akaike weights are identical to Table 4.7
print(res.table, digits = 2, LL = TRUE) #print table with 2 digits and
##print log-likelihood in table
print(res.table, digits = 4, LL = FALSE) #print table with 4 digits and
##do not print log-likelihood


#Last example: Iron
##two-way ANOVA with interaction
data(iron) #Again, try this one on your own
##Create a full model with interaction
##create an additive model
##Creata a null model

##Compare the three candidate models
##
##


