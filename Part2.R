# load the data
library(ggplot2)
data(ToothGrowth)

# summary
summary(ToothGrowth)
str(ToothGrowth)
head(ToothGrowth)
dose05 <- subset(ToothGrowth,dose==0.5)
dose10 <- subset(ToothGrowth, dose==1.0)
dose20 <- subset(ToothGrowth, dose==2.0)

# mean
sapply(split(ToothGrowth$len, ToothGrowth$supp), mean)
sapply(split(dose05$len, dose05$supp), mean)
sapply(split(dose10$len, dose10$supp), mean)
sapply(split(dose20$len, dose20$supp), mean)
# variance
sapply(split(ToothGrowth$len, ToothGrowth$supp), var)
sapply(split(dose05$len, dose05$supp), var)
sapply(split(dose10$len, dose10$supp), var)
sapply(split(dose20$len, dose20$supp), var)

# plot
p <- ggplot(aes(x = supp, y = len), data = ToothGrowth) 
p <- p + geom_boxplot(aes(fill = factor(supp))) 
P <- p + xlab("Supplement") 
p <- p + ylab("Tooth Length [mm]") 
P <- p + ggtitle("Tooth Length by Supplement") 
p <- p + geom_point()
p


p2 <- ggplot(ToothGrowth, aes(x=supp, y=len)) 
p2 <- p2 + geom_boxplot(aes(fill=supp))
p2 <- p2 + geom_point()
p2 <- p2 + facet_grid(.~dose)
p2 <- p2 + ggtitle("Tooth Growth by Dosage & Supplement")
p2 <- p2 + xlab("Dosage")
p2 <- p2 + ylab("Tooth Length [mm]")
p2

# test1
t.test(len~supp,paired=FALSE, var.equal=FALSE,data=ToothGrowth)
# test2
t.test(len~supp,paired=FALSE, var.equal=FALSE,data=dose05)
#Test 3
t.test(len~supp,paired=FALSE, var.equal=FALSE,data=dose10)
#Test 4
t.test(len~supp,paired=FALSE, var.equal=FALSE,data=dose20)

