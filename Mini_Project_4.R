# Simulation done sequentially

# Initialize B0hat and B1hat
B0hat = rep(0, 10000)
B1hat = rep(0, 10000)

# Run our simulation and extract our 10,000 coefficient estimates
for (i in 1:10000){
  x = rnorm(50, mean = 0, sd = 1)
  se = rnorm(50, mean = 0, sd = 4)
  y = 2 + 3*x +se
  
  cdata = data.frame(cbind(y, x))
  
  linModel = lm(y~x, cdata)
  
  B0hat[i] = linModel$coefficients[1]
  B1hat[i] = linModel$coefficients[2]
}

# Find the average of all B0hat values
B0ave = mean(B0hat)
print(paste("The average B0hat estimate is", B0ave))

# Find the average of all B1hat values
B1ave = mean(B1hat)
print(paste("The average B0hat estimate is", B1ave))

# Find how much the average estimates differ from reality
print(paste("The average B0hat estimate differs from the true B0 value by", 2 - B0ave))
print(paste("The average B1hat estimate differs from the true B0 value by", 3 - B1ave))
