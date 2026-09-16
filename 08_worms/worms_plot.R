worms <- read.table("08_worms/worms", header = TRUE)
attach(worms)
head(worms)


#par(mfrow = c(2, 2))
#par(mar=c(2,3,2,0))

#top left lm plot 
model <- lm(Worm.density ~ Soil.pH, data = worms)

plot(Worm.density ~ Soil.pH, xlim = c(3.5, 5.5), ylim = c(0, 8),
     xlab = "Soil pH", ylab = "Worm density")
abline(model, lwd=2, lty = 2)

intercept <- round(coef(model)[1], 2)
slope <- round(coef(model)[2], 2)

legend("topleft",
       legend = c(paste("Intercept =", intercept),
                  paste("Slope =", slope)),
       bty = "o",
       cex = 0.9,
       bg = "white")

#top right boxplot plot
boxplot(Soil.pH ~ Vegetation, data = worms, main = "", xlab = "", ylab = "", ylim = c(3.5,6))

#bottom left distribution plot
hist(Slope, main="")





