#Defining Functions
avg <- function(x){
  sum(x)/length(x)
}

#Q1 Hanes Data with Gender Indicators
han <- na.omit(hanes)
plot(x=han$height, y=han$weight,
     xlab="Height (Inches)",
     ylab="Weight (Pounds)",
     main="Hanes Data with Gender Indicators",
     pch= ifelse(han$gender =="F",20,8),
     col =ifelse(han$gender == "F","red","black"))

#linear regression
lm(han$weight ~ han$height + han$gender)
#regression line for female
abline(a=-203.190, b=5.772, col="red", lwd=2 )
#regression line for male
abline(a=-214.805, b=5.772, col="black", lwd=2 )

#Legend regression lines
legend("topleft", c("Female", "Male"), lwd =2,
       col=c("red","black"))

#calculating Sum of squares & Coefficient of Determination
TotalSumSqHan <- sum((han$weight - avg(han$weight))^2)
ResidualSumSqHan <- sum(lm(han$weight ~ han$height + han$gender)$residuals^2)
CoDHan <- ((TotalSumSqHan - ResidualSumSqHan)/TotalSumSqHan)*100
CoDHan
