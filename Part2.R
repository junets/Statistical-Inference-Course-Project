# load the data
library(ggplot2)
data(ToothGrowth)

# summary
summary(ToothGrowth)
str(ToothGrowth)
head(ToothGrowth)

# mean
sapply(split(ToothGrowth$len, ToothGrowth$supp), mean)
sapply(split(ToothGrowth$len, ToothGrowth$dose), mean)

# variance
sapply(split(ToothGrowth$len, ToothGrowth$supp), var)
sapply(split(ToothGrowth$len, ToothGrowth$dose), var)

# plot
p <- ggplot(aes(x = supp, y = len), data = ToothGrowth) 
p <- p + geom_boxplot(aes(fill = factor(supp))) 
P <- p + xlab("Supplement") 
p <- p + ylab("Tooth Length [mm]") 
P <- p + ggtitle("Tooth Length by Supplement") 
p <- p + geom_point()
p

p0 <- ggplot(aes(x = dose, y = len), data = ToothGrowth) 
p0 <- p0 + geom_boxplot(aes(fill = factor(dose))) 
P0 <- p0 + xlab("Dosage") 
p0 <- p0 + ylab("Tooth Length [mm]") 
P0 <- p0 + ggtitle("Tooth Length by Dosage") 
p0 <- p0 + geom_point()
p0

p1 <- ggplot(ToothGrowth, aes(x=dose, y=len)) 
p1 <- p1 + geom_boxplot(aes(fill=factor(dose)))
p1 <- p1 + geom_point()
p1 <- p1 + facet_grid(.~supp)
p1 <- p1 + ggtitle("Tooth Growth by Supplement & Dosage")
p1 <- p1 + xlab("Supplement")
p1 <- p1 + ylab("Tooth Length [mm]")
p1

p2 <- ggplot(ToothGrowth, aes(x=supp, y=len)) 
p2 <- p2 + geom_boxplot(aes(fill=supp))
p2 <- p2 + geom_point()
p2 <- p2 + facet_grid(.~dose)
p2 <- p2 + ggtitle("Tooth Growth by Dosage & Supplement")
p2 <- p2 + xlab("Dosage")
p2 <- p2 + ylab("Tooth Length [mm]")
p2

# test1
t.test(len~supp,data=ToothGrowth)
# test2
t.test(len~dose,data=subset(ToothGrowth, ToothGrowth$dose %in% c(0.5,1)))
#Test 3
t.test(len~supp,data=subset(ToothGrowth, ToothGrowth$dose %in% c(0.5,2)))
#Test 4
t.test(len~supp,data=subset(ToothGrowth, ToothGrowth$dose %in% c(1,2)))
