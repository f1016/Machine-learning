table <- read.table("SalaryData.txt", header = TRUE, sep =" ", dec = ".")


#Initialize########
pi <- c(0.4, 0.35,0)
mean <- c(3344, 7000, 19000)
var <- c(400^2, 1700^2, 9000^2)
w <- c(0,5, 0.3, 0.2)
###################

i <- 1
t <- 1
str_w <- matrix(0,nrow = 8001,ncol = 3, byrow = TRUE)

pi_nunom <- c(0, 0)
pi_denom <- c(0, 0)

mean_nunom <- c(0, 0, 0)
mean_denom <- c(0, 0, 0)

var_nunom <- c(0, 0, 0)
var_denom <- c(0, 0, 0)

while(t != 10){

  while(i != 8001){
#E step 
    w[1] <- pi[1] * dnorm(table[i,1], mean[1], sqrt(var[1]))
    w[2] <- pi[2] * dnorm(table[i,1], mean[2], sqrt(var[2]))
    w[3] <- (1 - pi[1] - pi[2]) * dnorm(table[i,1], mean[3], sqrt(var[3]))
    
    str_w[i,1] <- w[1]/(w[1] + w[2] + w[3])
    str_w[i,2] <- w[2]/(w[1] + w[2] + w[3])
    str_w[i,3] <- w[3]/(w[1] + w[2] + w[3])
    
    i <- i+1
  }

# M STEP
  i <- 1
  pi_nunom <- c(0, 0)
  pi_denom <- c(0, 0)
  
  mean_nunom <- c(0, 0, 0)
  mean_denom <- c(0, 0, 0)
  
  var_nunom <- c(0, 0, 0)
  var_denom <- c(0, 0, 0)
  
  i <- 1
  k <- 1
  #CALCULATE PI_1 AND MEAN
  while(i != 8001){
      pi_nunom[1] <- pi_nunom[1] + (1-pi[2])*(str_w[i,1])
      pi_denom[1] <- pi_denom[1] + (str_w[i,1]+ str_w[i,3])

      
      for(k in 1:3){
        mean_nunom[k] <- mean_nunom[k] + table[i, 1] * str_w[i,k]
        mean_denom[k] <- mean_denom[k] + str_w[i, k]
      }
      
      i <- i +1
  }
  pi[1] <- pi_nunom[1]/pi_denom[1]
  
  for(k in 1:3){
    mean[k] <- mean_nunom[k] / mean_denom[k]
  }
  ######################################################
  #CALCULATE PI_2 as it depends on PI_1(t+1)
  i <- 1
  while(i != 8001){
    pi_nunom[2] <- pi_nunom[2] + (1-pi[1])*(str_w[i,2])
    pi_denom[2] <- pi_denom[2] + (str_w[i,2]+ str_w[i,3])
    i <- i +1
  }

  pi[2] <- pi_nunom[2]/pi_denom[2]

  ####################################################
  #CALCULATE var_k as it depends on mean_k(t+1)
  
  i<-1
  while(i != 8001){
    
      for(k in 1:3){
        var_nunom[k] <- var_nunom[k] + str_w[i,k]*(table[i, 1] - mean[k])^2
        var_denom[k] <- var_denom[k] + str_w[i,k]
      }
      
    i <- i +1
  }

  for(k in 1:3){
    var[k] <- var_nunom[k] /var_denom[k]
  }
#################################
  t <- t + 1
}
pi[3] <- 1 - pi[1] - pi[2]
print(pi)
print(mean)
print(sqrt(var))
hist(prob=TRUE,table[,1],breaks = 8000)
curve(dnorm(x,mean[1],sqrt(var[1])), add=TRUE,col = "darkred",lwd=2)
curve(dnorm(x,mean[2],sqrt(var[2])), add=TRUE,col = "darkred",lwd=2)
curve(dnorm(x,mean[3],sqrt(var[3])), add=TRUE,col = "darkred",lwd=2)
x#output the first 50 employee's salary group

for(i in 1:50){
  prob1 <- dnorm(table[i,1],mean[1],sqrt(var[1]))
  prob2 <- dnorm(table[i,1],mean[2],sqrt(var[2]))
  prob3 <- dnorm(table[i,1],mean[3],sqrt(var[3]))
  if(prob1 > prob2 && prob1 > prob3){
    cat(sprintf("The %s employee is belongs to Group 1\n", i))
  }else if(prob2 > prob1 && prob2 > prob3){
    cat(sprintf("The %s employee is belongs to Group 2\n", i))
  }else{
    cat(sprintf("The %s employee is belongs to Group 3\n", i))
    
  }
  
}

