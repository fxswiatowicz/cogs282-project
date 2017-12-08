#creating test data (10,000 trials)
input.test <- matrix(0, nrow = 10000, ncol = 260)
output.test <- matrix(0, nrow = 10000, ncol = 10)

for(j in 0:9)
  for(i in 1:1000) {
    input.test[i+j * 1000, sample(1:250, (j+1)*10)] <- 1
    #only nonsymbolic input tested
    input.test[i+j*1000,] <- add.noise(input.test[i+j*1000,], 0.05)
    output.test[i+j*1000, j+1] <- 1
  }