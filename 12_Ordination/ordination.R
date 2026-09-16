#There are many packages that may be relevant when it comes to ordinations and cluster analyses
library(vegan)
library(nlme)
library(multcomp)
library(cluster)
library(ade4)
library(stats)
library(vcd)
library(asbio)
library(labdsv)
library(e1071)
library(car)
library(psych)


#ORDINATIONS
############
data(swiss) #load the swiss data
str(swiss) #inspect the data
summary(swiss) #Look at a summary
pcamodel<-prcomp(swiss,scale=T) #make a PCA model of the dataset
plot(pcamodel) #Check out how much each axis explains by ploting the model
biplot(pcamodel, cex = c(0.7, 0.7))  # c(site labels, species/variable labels) #Make a Visualisation of the PCA axes
round(cor(swiss),d=2) #Compare this with a with correlation matrix of the variables

############
data(VADeaths) #Nww load the VADeaths dataset
VADeaths #Small morbid table
modelpca2<-prcomp(VADeaths) #Reduce the data in a PCA
plot(modelpca2) #How much does the model explain?
summary(modelpca2)
biplot(modelpca2) #Make a biplot
round(cor(VADeaths),d=2) #Compare again with a correlation matrix

#Now do the smae for the varechem dataset
x

#Now let us look at ecological species#
#Use a decorana analysis
data(dune) #Load the dune data
modeldca<-decorana(dune) #Make a decorana model
plot(modeldca) #Make a plot of the model
modeldca #Hoch much does the model explain? Just write down the result
summary(modeldca) #Compare with the summary, which is much longer
modelcluster1<-agnes(dune) #Now let us make an agglomerative cluster analysis with the dune dataset
plot(modelcluster1) #Plot a dendrogram of the model
modelcluster2<-hclust(dist(dune)) #Now make an agglomerative cluster analysis
plot(modelcluster2) #Compare Hierarchical with agglomerative clustering using the plots


#Now let us make a Factor analysis with mtcars
data(mtcars)
#Make a factor analysis with three factors
factor_analysis<-factanal(mtcars,factors = 3)
factor_analysis #Check out the results

#Now we make a NMDS with the varespec data
data(varespec) #Load data
#Make a model mith metaMDS, set k=2
modelnmds1<-metaMDS(varespec,k=2) #Stress above 0.2, which is not good
#Change k=3 and make a new model
modelnmds2<-metaMDS(varespec,k=3) #Below 0.15, much better
modelnmds2 #Check out the results
plot(modelnmds2,type="t") #plot the model with species and plot numbers
modelenv1<-envfit(modelnmds2, varechem,perm=1000) #Now calculate the varechem data onto the ordination
modelenv1
plot(modelenv1,choices=c(1,1)) #Add the significant environmental variables to the plot

#Now make an analysis yourselves with mtcars. What would you do, what does it explain?
