#Load the regression dataset

regression <- read.table("03_correlation/regression.txt", header=T)

#Attach the dataset

attach(regression)

#Inspect the data

head(regression)
str(regression)
dim(regression)
colnames(regression)



#Make a simple linear model

model <- lm(growth ~ tannin, data = regression) #tanin is independetn. growth depends on it
summary(model)

#Look at the summary, focus on p-value and estimates
#Test str of the summary command, there is much information hidden there
#Access p-value out of the summary
#Access r-suqared value out of the summary
#Are the residuals normally distributed? Make a histogram!
#Plot residuals. Is there a pattern in the plot?


#Another simple example: Load the cars dataset

#Attach the dataset
#Check out the names of the variables
#Inspect the dataset
par(mfrow=c(1,2)) #Make an empty window for 2 plots
#Plot histograms of two variables
#Create linear model of the two variables
#Make a summary of the model
#Make only one plot
#plot only within the quadrangle
#plot relation between speed and distance
#fit regression line into the x-y-plot
#Are the residuals normally distributed? Make a histogram

#Now we look at a lrger dataset: mtcars

#Inspect the dataset
#Look at a summary of the dataset
#What explains the mpg. Let us test this variables against all other variables
#Start with disp

#Interpret the summary of the variables
##Now let us test all the variables for p-values, Estimate and R-squared values
results<-matrix(ncol=3,nrow=c(dim(mtcars)[[2]]-1))#Create empty matrix for results
for(i in 2:11){
  modeltemp<-lm(disp~scale(mtcars[,i]),data=mtcars) #make model i
  results[i-1,1]<-round(summary(modeltemp)$coefficients[2,4],d=6) #collect p-value
  results[i-1,2]<-round(summary(modeltemp)$coefficients[2,1],d=2) #collect scaled estimate
  results[i-1,3]<-round(summary(modeltemp)$r.squared,d=2) #collect r-squared value
  print(i) #progress tracker
}
results #check out the results

###some people use the step function####
#Try it out: Build a full model with all variables and throw it into the step function
#Full model with all predictors
#Look at the summary of the full model
#Let us make a quick and dirty step function
#Look at the "minimum adequate model"