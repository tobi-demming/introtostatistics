library(tidyverse) #load tidyverse
library(car) #load car

# generalized linear model binomial distribution ####

#load and attach mtcars
data(mtcars)
attach(mtcars)


#rewrite mtcars into tempcars
tempcars <- mtcars # is this just creating an identical dataframe? Why cant we stick with mtcars?


#Make proper colnames e.g. Miles_Gallon
colnames(tempcars)

colnames(tempcars) <- c("mpg", "cylinders", "displacement_cuin", "horsepower",
                        "rear_axle_ratio", "weight_1000lbs", "quarter_mile_time",
                        "engine_vshape_straight", "transmission_auto_manual",
                        "forward_gears", "carburetors") #verified using ?mtcars


#Transform Cylinders, Engine, Transmission and Gear into Factor variables

tempcars$cylinders_factor <- factor(tempcars$cylinders)

tempcars$engine_factor <- factor(tempcars$engine_vshape_straight,
                                 levels = c(0, 1),
                                 labels = c("V-shaped", "straight"))

tempcars$transmission_factor <- factor(tempcars$transmission_auto_manual,
                                       levels = c(0, 1),
                                       labels = c("automatic", "manual"))

tempcars$gears_factor <- factor(tempcars$forward_gears)

str(tempcars)

#Inspect the dataset, and check out histograms of all quantitative variables

hist(tempcars$mpg)
hist(tempcars$displacement_cuin)
hist(tempcars$horsepower)
hist(tempcars$rear_axle_ratio)
hist(tempcars$weight_1000lbs)
hist(tempcars$quarter_mile_time)



#Build models to explain engine as a 0/1 variable with Miles/Gallon and Axle ration, both with and without interaction

model_no_interaction <- glm(engine_vshape_straight ~ mpg + rear_axle_ratio,
                            data = tempcars, family = binomial) #+ says no interaction (additive effects)
summary(model_no_interaction)


model_interaction <- glm(engine_vshape_straight ~ mpg * rear_axle_ratio,
                            data = tempcars, family = binomial) #+ says no interaction (additive effects)
summary(model_interaction)

#Build models to explain engine with Miles/Gallon and Axle ration, both with and without interaction
model1_pre <- glm(engine_factor ~ mpg * rear_axle_ratio, data=tempcars,family=binomial)
model2_pre <- glm(engine_factor ~ mpg + rear_axle_ratio, data=tempcars,family=binomial)
#Build model to explian Engine with Axle Ratio and Quarter Mile Time, both single effects and interactions
model3_pre <- glm(engine_factor ~ rear_axle_ratio * quarter_mile_time,data=tempcars, family=binomial)
model4_pre <- glm(engine_factor ~ rear_axle_ratio + quarter_mile_time,data=tempcars, family=binomial)
#Compare all models with AIC
AIC(model1_pre);AIC(model2_pre);AIC(model3_pre);AIC(model4_pre)
#Maybe try to build a minimum adequate model



#Build model to explain Engine with Axle Ratio and Quarter Mile Time, both single effects and interactions
#Compare all models with AIC
#Maybe try to build a minimum adequate model



#####Another example######
#####Swiss################
#load swiss data

#Attach data
#Inspect data
#Transform Catholics variable into 1/0 variable using ifelse
#Check for prevalence of variable
#Create binomia models with all variables tested against 0/1 religion
#Create minimum adequate model with two significant variables
#Looks better accodring to AIC, but one term is not significant?
#Multicolinearity does not seem to be a problem, values <10

####Now an example from medicine####
#Load zprostate data

#attach data
#inspect data

#check distribution of lpsa variable
#Build all individual GLMs with Gaussian distribution

#Now try to build a full model with the GLM

#Reduce the full model with the step function
#Compare the best model wuth a null model