#Load (and maybe install) main packages
library(lme4)
library(multcomp)
library(nlme)
library(ncf)
library(ape)

#Load splityield file
yields<-read.table("09_Mixed_Effects_Models/splityield.txt",header=T)
#Attach dataset
attach(yields)
names(yields) #Inspect dataset
str(yields)
#Check out factor levels -including interactions- with table
table(block,density,fertilizer)
# Create a loop with simple LMs
results<-matrix(nrow=dim(yields)[2]-1,ncol=4)
for( i in 2:dim(yields)[2]){
  model<-lm(yield~yields[,i])
  results[i-1,1]<-names(yields)[i]
  results[i-1,2]<-round(summary(model)$coefficients[2,4],d=8)
  results[i-1,3]<-summary(model)$coefficients[2,1]
  results[i-1,4]<-AIC(model)
}

colnames(results) <- c("predictor","p","estimate","AIC")
results # Block: not significant. We are not interested in this factor anyway, if it would be significnat we would have a problem

#Create nullmodel with nested random factors block/irrigation/density. 
m0<-lme(yield~1,random=~1|block/irrigation/density, method="ML") 

# Create full model (all possible interaction effects), same random factor structure
mf<-lme(yield~irrigation*density*fertilizer,
        random=~1|block/irrigation/density, method="ML") 
#Check out the summary of the model
summary(mf)
#Compare the AIC of the two models
AIC(m0,mf)# mf better in terms of AIC

# Reducemodels -------------------------------
# Reduce the model based on p-values, first model without any interactions
misimple<-lme(yield~fertilizer+irrigation+density,
              random=~1|block/irrigation/density, method="ML")

AIC(mf,misimple) # better AIC

# Now make a model only with 2-way interactions
mi2way<-lme(yield~(fertilizer+irrigation+density)^2,
            random=~1|block/irrigation/density, method="ML")
#Compare the model with AICs
AIC(mf,mi2way) # better AIC, but anova n.s.
AIC(m0,mi2way) # better AIC, significantly different

# -> so far: model with 2-way interaction best

# Also check out all models with single predictors
ms1<-lme(yield~fertilizer,
         random=~1|block/irrigation/density, method="ML")

ms2<-lme(yield~irrigation,
         random=~1|block/irrigation/density, method="ML")

ms3<-lme(yield~density,
         random=~1|block/irrigation/density, method="ML")

AIC(mi2way,ms1) #compare AICs
AIC(mi2way,ms2) #compare AICs
AIC(mi2way,ms3) #compare AICs

# -> still 2-way interaction best

# Now reduce the model by throwing out all non-significant two way interactions
m2.1<- lme(yield~fertilizer*density,
           random=~1|block/irrigation/density, method="ML")
m2.2<- lme(yield~fertilizer*irrigation,
           random=~1|block/irrigation/density, method="ML")
m2.3<- lme(yield~irrigation*density,
           random=~1|block/irrigation/density, method="ML")
#Compare th AICs of all models
AIC(mi2way,m2.1) # sign., AIC worse (and BIC worse)
BIC(mi2way,m2.1)
AIC(mi2way,m2.2) # sign., AIC worse (but BIC better)
BIC(mi2way,m2.2)
AIC(mi2way,m2.3) # sign., AIC worse (BIC almost the same)
BIC(mi2way,m2.3)

# -> still two-way interaction best

# Now one could try all the different interactions
mi1a<-lme(yield~fertilizer*irrigation+density,
          random=~1|block/irrigation/density, method="ML")

mi1b<-lme(yield~fertilizer+irrigation*density,
          random=~1|block/irrigation/density, method="ML")

mi1c<-lme(yield~irrigation+fertilizer*density,
          random=~1|block/irrigation/density, method="ML")


mi1d<-lme(yield~irrigation*density+fertilizer*density,
          random=~1|block/irrigation/density, method="ML")

mi1e<-lme(yield~irrigation*density+fertilizer*irrigation,
          random=~1|block/irrigation/density, method="ML") # mi1e has lower AIC than mi2way

mi1f<-lme(yield~density*fertilizer+irrigation*density,
          random=~1|block/irrigation/density, method="ML")

AIC(mi2way,mi1a) 
AIC(mi2way,mi1b)
AIC(mi2way,mi1c) 
AIC(mi2way,mi1d) 
AIC(mi2way,mi1e) # mi1e has lower AIC than mi2way
BIC(mi2way, mi1e)# but mi1e has a higher BIC
AIC(mi2way,mi1f) 

# -> hm, mi1e?
#Keep comparing all the different models, just for practice. For good measure, also check out the BIC results


AIC(m0,mi1e) # sign., AIC & BIC better
BIC(m0,mi1e)

#What is the best model
#Also, check out some interaction plots, which some people seem to like
interaction.plot(fertilizer,irrigation,yield) 
# -> irrigated always higher, but highest for NP-fertilizer & lowest for N-fertilizer

interaction.plot(density,irrigation,yield)
# -> irrigation has lowest impact if density low & highest if density high


---------------------------------------------------------------------------------
  # Now compare everything with a simple Anova Model with nested Error terms-------------------------------------------------------------
# anova
model<-aov(yield~irrigation*density*fertilizer+Error(block/irrigation/density))
summary(model) # fertilizer & irrigation:fertilizer significant

str(yield) # inspect variable "yield"
hist(yield) # plot the data

# "full" lm with all possible interactions
modellm<-lm(yield~block*irrigation*density*yield)
summary(modellm)
AIC(modellm)#remember that this is not the same model, you cannot compare this!
# a lot of significant effects, also for blocks 
# Now compare with package lme4 -------------------------------------------

library(lme4)
model<-lmer(yield~
              irrigation*density*fertilizer+
              (1|block)+(1|irrigation)+(1|density), # random effects for all predictors?
            yields, # data 
            REML=F) 

summary(model)
cftest(model) # z-tests for each predictor


# ??? why this part -> again with nlme? -----------------------------------
# null model (intercept & RE only)
model.lme0<-lme(yield~1,random=~1|block/irrigation/density, method="ML") # ah, here the ML was used
summary(model.lme0)

#Full model
model.lme1<-lme(yield~irrigation*density*fertilizer,random=~1|block/irrigation/density, method="ML")
summary(model.lme1)


# remove 3-way interaction
model.lme2<-update(model.lme1,~. -irrigation:density:fertilizer) 
summary(model.lme2)
AIC(model.lme1,model.lme2) # same result as before: not significant
# remove interaction density*fertilizer
model.lme3<-lme(yield~irrigation*density+irrigation*fertilizer,
                random=~1|block/irrigation/density, method="ML") 
summary(model.lme3)

AIC(model.lme1,model.lme2,model.lme3) # no significant differences
# still: reduced models have lower AIC


# remove interaction irrigation*fertilizer
model.lme4<-update(model.lme3,~. -irrigation:fertilizer)
summary(model.lme4)


# Compare all models
AIC(model.lme0,model.lme1,model.lme2,model.lme3,model.lme4)
# 1 better than 0 (null model), 4 worse than 3 (-> AIC for 4 larger)



#Now some model inspection for good measure

hist(resid(model.lme3)) # look very normally distributed
boxplot(as.numeric(resid(model.lme3))~block) #Errors look similiar across block levels

qqnorm(residuals(model.lme3)) # looks good (-> almost in line)
plot(model.lme3,yield~fitted(.)) # Visualize -> plot fitted vs. actual values 
qqnorm(model.lme3,~resid(.)|block) # per block


# Compare with a simple Anova
model<-aov(yield~
             irrigation*density+irrigation+fertilizer+ # fixed effects
             Error(block/irrigation/density)) # "random effects"

summary(model) # irrigation, irrigation*density & fertilizer significant
summary(model.lme4) 
cftest(model.lme4)

par(mfrow=c(1,1))

interaction.plot(fertilizer,irrigation,yield) 
# -> irrigated always higher, but highest for NP-fertilizer

interaction.plot(density,irrigation,yield)
# -> irrigation has lowest impact if density low & highest if density high

#In case you are confused, this is by design ;-)










