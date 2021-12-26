#Code used to generate the dummy data used to test the genetic algorithm
#Put here for reproducability
n <- 1000
set.seed(0)
dummy_data <- data.frame(matrix(runif(n*10,min=0,max=30),nrow=n,ncol=10))
colnames(dummy_data) <- c("x1","x2","x3","x4","x5","x6","x7","x8","x9","x10")
beta0 <- rnorm(n,mean=100,sd=10)
beta <- rnorm(5,mean=0,sd=5)
noise <- rnorm(n,0,5)
y <- as.matrix(dummy_data)[,1:5]%*%beta + beta0 + noise
dummy_data$y <- y
