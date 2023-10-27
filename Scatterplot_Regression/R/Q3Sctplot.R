# Function Definitions
avg <- function(x){
  sum(x)/length(x)
}
#Q3
#Loading CSV File for sweden population from Year 2000 - 2022
sweden <- read.csv("sweden.csv")

#plotting scatterplot
plot(x=sweden$year, y=sweden$population,
     xlab="Year (2000 - 2022)",
     ylab="Population in Millions",
     main="Sweden Population data",
     pch=10)

#Degree 2 Polynomial
year_2 <- sweden$year^2
#Degree 3 Polynomial
year_3 <- sweden$year^3

#Linear regression for degree2 polynomial
lm(sweden$population ~ sweden$year + year_2)
deg_2 <- lm(sweden$population ~ sweden$year + year_2)
deg_2$coefficients
curve(8.835217 + 0.041269 *x + 0.001755 *x^2 , add=TRUE, col='green',lwd=2, lty =1)

#Linear regression for degree3 polynomial
lm(sweden$population ~ sweden$year + year_2 + year_3)
deg_3 <- lm(sweden$population ~ sweden$year + year_2 + year_3)
deg_3$coefficients
curve(8.9075819 - 0.0032148 *x + 0.0069236*x^2  -0.0001566*x^3, add=TRUE,col='orange',lwd=2, lty=2 )

#Legends for curve
legend("topleft", c("Degree 2 ploynomial curve", "Degree 3 polynomial curve"), lwd =2 ,lty=c(1,2),col
       =c("green","orange"))

#Sum of squares & Coefficient of Determination Calucations
TotalSumSq <- sum((sweden$population - avg(sweden$population))^2)
ResidualSumSq_2 <- sum(lm(sweden$population ~ sweden$year + year_2)$residuals^2)
ResidualSumSq_3 <- sum(lm(sweden$population ~ sweden$year + year_2 + year_3)$residuals^2)
CoeffD_2 <- (TotalSumSq - ResidualSumSq_2)/TotalSumSq
CoeffD_3 <- (TotalSumSq - ResidualSumSq_3)/TotalSumSq
CoeffD_2
CoeffD_3
