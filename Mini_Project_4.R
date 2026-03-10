doSequential = function(){
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
  
  print("Using sequential computation:")
  
  # Find the average of all B0hat values
  B0ave = mean(B0hat)
  print(paste("The average B0hat estimate is", B0ave))
  
  # Find the average of all B1hat values
  B1ave = mean(B1hat)
  print(paste("The average B0hat estimate is", B1ave))
  
  # Find how much the average estimates differ from reality
  print(paste("The average B0hat estimate differs from the true B0 value by", 2 - B0ave))
  print(paste("The average B1hat estimate differs from the true B0 value by", 3 - B1ave))
}

doUsingParallel = function(){
  # Simulation done in parallel
  
  # Import dependencies
  library(foreach)
  library(doParallel)
  
  # Detect number of cores and reserve all except for 1
  detectCores() # I have 12 cores in my laptop
  numCores <- detectCores() - 1 # Use 11 cores, in my case
  registerDoParallel(numCores)
  
  # Create the simulation study function
  sim.function =  function(){
    x = rnorm(50, mean = 0, sd = 1)
    se = rnorm(50, mean = 0, sd = 4)
    y = 2 + 3*x +se
    
    cdata = data.frame(cbind(y, x))
    
    linModel = lm(y~x, cdata)
    
    c(linModel$coefficients[1], linModel$coefficients[2])
  };
  
  
  results = foreach(i = 1:10000, .combine = "rbind") %dopar% sim.function()
  
  B0hat = results[,1]
  B1hat = results[,2]
  
  print("Using parallel computation...")
  
  # Find the average of all B0hat values
  B0ave = mean(B0hat)
  print(paste("The average B0hat estimate is", B0ave))
  
  # Find the average of all B1hat values
  B1ave = mean(B1hat)
  print(paste("The average B0hat estimate is", B1ave))
  
  # Find how much the average estimates differ from reality
  print(paste("The average B0hat estimate differs from the true B0 value by", 2 - B0ave))
  print(paste("The average B1hat estimate differs from the true B0 value by", 3 - B1ave))
}

system.time(doSequential())
system.time(doUsingParallel())