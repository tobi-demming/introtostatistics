library(bipartite)

# F-Test (var.test): Compare 2 variances

#Compare 2 distributions
#get your working directory (done: connected to github)
#set working directory (kind of done trough the github)

#load data f.test.txt, name session "populations"
populations <- read.table("01_simple_tests/f.test.txt", header = TRUE)

#attach populations
attach(populations)


#inspect populations
head(populations)      # first 6 rows — quick look at structure
str(populations)       # data types of each column (numeric, factor, character, etc.)
summary(populations)   # min/max/mean/quartiles for each column
dim(populations)       # how many rows and columns
names(populations)     # column names



#test if PopA and PopB diff in their variances
var.test(PopA,PopB)

#visualize the range and median for PopA and PopB

boxplot(populations,
        names = c("Population A", "Population B"),
        main = "Comparison of Population A and Population B",
        ylab = "Value",
        col = c("lightblue", "lightgreen"))


#overwrite value in line 7 for PopA with 50

populations$PopA[7]
populations$PopA[7] <- 50

#check how new value influences results of F test


# T-Test (t.test): Comparision of 2 means
#Load table t.test.txt
#attach the file

#run a t-Test comparing Golze with Finguren, assume equal variances
#visualize the data 

#Now check out an example where there is no significant difference
#Load the sleep data from base and compare the variable "extra" for the groups
#Now make a t-test to compare the difference between the two groups


# T-Test for paired samples (t.test; paired=T): Comparision of 2 means
#Load the t.test.paired.txt file
#attach it
#inspect the data and its structure


#inspect only range of parameter "before"


#run a t-Test for paired samples comparing "before" and "after"
# assign your test statistic to a new object called "compareweight"

#inspect the structure of "compareweight"

#round p values in object "compareweight" to 4 digits

#make a boxplot of before and after


# Wilcoxon-test(wilcox.test): Compare 2 medians
#load file wilcox.ranksum.txt
#attach it
#inspect data 

#compare the values of GroupA and GroupB with Wilcoxon Test





# Chi-Square-Test to compare counts
#load file chiquadr.txt
#attach it
#inspect data

#run chi square test of drinks

# shows expected values
#Make a plotweb of the file drinks

#If you still have time, discuss which examples from your research you know where simple tests may play a role

