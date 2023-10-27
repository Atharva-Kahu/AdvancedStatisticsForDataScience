#Defining Functions
avg <- function(x){
  sum(x)/length(x)
}
#Q2 Hanse Data 
han <- na.omit(hanes)
plot(x=han$height, y=han$weight,
     xlab="Height (Inches)",
     ylab="Weight (Pounds)",
     main="Hanes Data with gender indicators and interaction terms",
     pch= ifelse(han$gender =="F",20,8),
     col =ifelse(han$gender == "F","red","black"))

#linear regression
lm(han$weight ~ han$height + han$gender + han$gender:han$height)
#regression line for female
abline(a=-143.998, b=4.854, col="red", lwd=2 )
#regression line for male
abline(a=-289.141, b=6.852, col="black", lwd=2 )

#Legend regression lines
legend("topleft", c("Female", "Male"), lwd =2,
       col=c("red","black"))

#calculating Sum of squares & Doefficient of Determination
TotalSumSqHan <- sum((han$weight - avg(han$weight))^2)
ResidualSumSqHan <- sum(lm(han$weight ~ han$height + han$gender + han$gender:han$height)$residuals^2)
CoDHan <- ((TotalSumSqHan - ResidualSumSqHan)/TotalSumSqHan)*100
CoDHan
