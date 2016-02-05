rm(list=ls())

filename <- "trees.csv"
print(c("Reading file: ",filename))
Trees <- read.csv(filename,header = TRUE,sep=",")

print(c("Dimensions of data file ",dim(Trees)))
print("Structure of data file")
str(Trees)

TreeHeight <-Trees[,4]
CrownRadii <- Trees[,5]

height.summary <- c(min(TreeHeight),max(TreeHeight),mean(TreeHeight),median(TreeHeight))
print("Height Summary: min, max, mean, median")
str(height.summary);
hist(TreeHeight)

radius.summary <- c(min(CrownRadii),max(CrownRadii),mean(CrownRadii),median(CrownRadii))
print("Crown Radius Summary: min, max, mean, median")
hist(CrownRadii)

print("Log-Histograms")

hist(log(TreeHeight))
hist(log(CrownRadii))

print("Scatter Plot with BoxPlots")

par(fig=c(0,0.8,0,0.8))
plot(TreeHeight,CrownRadii,xlab="Tree Height [m]",ylab="Crown Radius [m]")
par(fig=c(0,0.8,0.55,1),new=TRUE)
boxplot(TreeHeight,horizontal=TRUE, xaxt='n', frame.plot=F)
par(fig=c(0.65,1,0,0.8),new=TRUE)
boxplot(CrownRadii, yaxt='n', frame.plot=F)

print(cor.test(TreeHeight,CrownRadii))

EDA = function(x) {
  return( c(min(x),mean(x),median(x),max(x),range(x),sd(x),100*sd(x)/mean(x)) )
}

print("Function call to EDA (summary function) returns:")
print("min / mean / median / max / bottom range / top range / standard deviation / Coefficient of Variation")
print(EDA(TreeHeight))