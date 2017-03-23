demand <- read.csv(file.choose())

demand

demand$logy <- log(demand$Quantity)

regr <- lm(logy~Price,data=demand)
summary(regr)
intercept <- regr$coefficients[1]
intercept
slope <- regr$coefficients[2]
slope

alpha <- exp(intercept)
beta <- exp(slope)
alpha 
beta

demand$pred <- alpha * beta^demand$Price
plot(demand$Price,demand$pred,col="blue",type="b")
points(demand$Price,demand$Quantity,col="orange",type="b")
