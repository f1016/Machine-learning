#Using Newton's method 

table <- read.table("LogitRegData.txt", header = TRUE, sep =" ", dec = ".")

# x = table[i,1,1]
# y = table[i,2,1]

Mat <- matrix(c(1.5,1.6),nrow = 2, ncol = 1, byrow = TRUE) # by approximation 
TBT_M <- matrix(0, nrow = 2, ncol = 2, byrow = TRUE)
TBO_M <- matrix(0, nrow = 2, ncol = 1, byrow =TRUE)
i <- 1
j <- 1

while(j != 50){
  
  while(i != 51){
    
    TBT_M[1, 1] <- TBT_M[1, 1] - (exp( Mat[1,1]+Mat[2,1]*table[i,1,1]) / (exp(Mat[1,1] + Mat[2,1]*table[i,1,1]) + 1)^2)
    TBT_M[1, 2] <- TBT_M[1, 2] - table[i,1,1]*(exp(Mat[1,1]+Mat[2,1]*table[i,1,1])) / (exp(Mat[1,1] + Mat[2,1]*table[i,1,1])+1)^2
    TBT_M[2, 1] <- TBT_M[2, 1] - table[i,1,1]*(exp(Mat[1,1]+Mat[2,1]*table[i,1,1])) / (exp(Mat[1,1] + Mat[2,1]*table[i,1,1])+1)^2
    TBT_M[2, 2] <- TBT_M[2, 2] - (table[i,1,1]^2)*(exp(Mat[1,1]+Mat[2,1]*table[i,1,1])) / (exp(Mat[1,1] + Mat[2,1]*table[i,1,1])+1)^2
    
    TBO_M[1, 1] <- TBO_M[1 ,1] + table[i,2,1] - (exp( Mat[1,1]+Mat[2,1]*table[i,1,1]) / (exp(Mat[1,1] + Mat[2,1]*table[i,1,1])+1))
    TBO_M[2, 1] <- TBO_M[2 ,1] + table[i,2,1]*table[i,1,1] - table[i,1,1]*(exp( Mat[1,1]+Mat[2,1]*table[i,1,1]) / (exp(Mat[1,1] + Mat[2,1]*table[i,1,1])+1))
    
    i <- i + 1 
  }
  
  Mat = Mat - solve(TBT_M)%*%TBO_M
  TBT_M <- matrix(0, nrow = 2, ncol = 2, byrow = TRUE)
  TBO_M <- matrix(0, nrow = 2, ncol = 1, byrow = TRUE)
  i <- 1
  j <- j + 1
  
}
print(Mat)


plot( -4:4, type="n")
curve(exp( Mat[1,1] + Mat[2,1]*x)/(exp(Mat[1,1] + Mat[2,1]*x)+1),-4,4)
i<- 1
while(i != 51){
  points(table[i,1,1],table[i,2,1])
  i <- i+1
}

