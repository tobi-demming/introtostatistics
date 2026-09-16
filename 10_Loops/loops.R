###Loops###
# for Loops
for (i in 1:10){print(i)}
for (i in 5:1){print(i)}
for (i in c(1,3,4,10)){print(i)}

# while Loop
n=1
while(n<11){
  print(n)
  n<-n+1
}

# repeat Loop
n=1
repeat{
  print(n)
  n<-n+1
  if(n>10) break
}

# if Loop
x <- 5
if(x > 0){
  print("Positive number")
}


# Examples
z <- 1:10
for (i in 1:10){
  z[i] <- z[i]+1
}

z <- 1:10
z <- z+1

phrase<-"the quick brown fox jumps over the lazy dog"
q<-character(20)
for (i in 1:20) q[i]<- substr(phrase,1,i)
q

# Repeated plots with normal distribution
par(mfrow=c(2,2))
for (i in 1:4){
  daten <- rnorm(10000)
  hist(daten,main=paste("Sample",i))
}



#loop with data simulation
mydata<-matrix(sample(c(0,1,2,3),50,replace=T),nrow=5,ncol=10)
mydata
mydata01<-ifelse(mydata>0,1,0)

##Loop to make lots of correlations
mtcars
round(cor(mtcars),d=2)
dim(mtcars)
hvwmatrix<-matrix(ncol=dim(mtcars)[2],nrow=dim(mtcars)[2])
for(i in c(1:dim(mtcars)[2])){
  for(j in c(1:dim(mtcars)[2])){
    temp<-round(cor.test(mtcars[,i],mtcars[,j])$p.value,d=5)
    hvwmatrix[i,j]<-temp		
  }
}


#Functions#
fahrenheit_to_kelvin <- function(temp_F) {
  temp_K <- ((temp_F - 32) * (5 / 9)) + 273.15
  return(temp_K)
}

##I love sum of squares
sum.of.squares <- function(x,y) {
  x^2 + y^2
}
sum.of.squares(2,3)

library("wesanderson")
par(xpd=T)
par(mfrow=c(1,1))
plot(x=c(1:100),y=c(1:100),type="n",yaxt="n",xaxt="n",xlab=" ",ylab="",bty="n")
for(i in 1:100){
  x<-sample(c(10:90),1)
  y<-sample(c(10:90),1)
  coltest<-sample(c(1:4),1)
  textsize<-sample(c(0.8,1,1.2,1.4),1)
  text(x,y,label="I like R",col=wes_palette("Royal1")[coltest],cex=textsize)
  Sys.sleep(.5)
  rect(0,0,100,100,col="white",border=NA)