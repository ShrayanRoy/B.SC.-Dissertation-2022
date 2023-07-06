rm(list = ls(all = T)) #Removes all data
#Loading Required Library
library(ggplot2) #for graphics
library(wesanderson) #for colours

# Here We will do a comparative study of Mann Whitney U test &
# T test in two sample problems, where data is from Normal Distribution
# A function to calculate U statistic
Ustat <- function(x,y){
  u <- 0 #to store the value 
  for(i in 1:length(x)){
    u <- u + sum(y < x[i])
  }
  return(u) #returns the value of u
}
#Equal Variance
Power_comparison.T2 <- function(n1,n2,d,mu,R,alpha,exact.crit = F){
  
  # The function takes sample sizes,a vector of differences &
  # Replication Number,Level of Significance as argument
  set.seed(seed = 987654321) #for uniformity of result
  
  power.matrix <- matrix(0,nrow = length(d),ncol = 2) #A matrix to store power values
  colnames(power.matrix) = c('MWU test','T Test')
  
  for(i in 1:length(d)){
    
    test.statistic <- replicate(R,{
      
      x <- rnorm(n1,d[i] + mu,1); y <- rnorm(n2,mu,1) #our sample
      
      #computation of test statistics
      
      #Mann Whitney U test statistic
      MWU.statistic <- Ustat(x,y)
      
      #T test statistic
      est.var <- ((n1-1)*var(x) + (n2-1)*var(y))/(n1+n2-2) #pooled 
      variance
      T.statistic <- (mean(x) - mean(y))/sqrt(est.var*((1/n1) + (1/n2)))
      
      #Storing the values 
      c(MWU.statistic,T.statistic)
    })
    
    #Simulated power for Mann Whitney U test
    if(exact.crit){
      MNW.power <- mean(test.statistic[1,] > exact.crit) 
    }else{
      a <- (n1*n2)/2 ; b <- n1*n2*(n1+n2+1)/12
      cutpoint.1 <- qnorm(alpha,lower.tail = F)
      MNW.power <- mean((test.statistic[1,] - a- 0.5)/sqrt(b) >
                          cutpoint.1) #after continuity correction
    }
    
    #Simulated power for t test
    cutpoint.2 <- qt(alpha,n1+n2-2,lower.tail = F)
    T.power <- mean(test.statistic[2,] > cutpoint.2)
    
    #Storing the simulated powers in matrix
    power.matrix[i,] <- c(MNW.power,T.power)
  }
  
  power.matrix <- cbind(Diiference = d,power.matrix) #adding the difference column
  
  return(power.matrix) #returns a matrix 
}
#Function to draw Power Curve
Visualize_Power_comparison.T2 <- function(n1,n2,d,mu,R,alpha,exact.crit = F){
  #Storing Simulated Powers
  M <- Power_comparison.T2(n1,n2,d,mu,R,alpha,exact.crit)
  
  #Graph
  index.1 <- rep(c('Mann Whitney \n U Test','T Test'),each = nrow(M))
  graph.data <- data.frame(x = c(M[,1],M[,1]),y = c(M[,2],M[,3]),Index =
                             index.1)
  graph.1 <- ggplot(graph.data,aes(x,y,col = Index)) +
    geom_hline(yintercept = 1,linetype = 'dashed') + geom_line(size = 1.1) 
  +
    geom_point(show.legend = F,size = 2) +
    labs(x = expression(paste('Difference ',(mu[1] - mu[2]))),y =
           'Simulated Power',
           subtitle = paste('Mu = ',mu,',n1 =',n1,',n2 = ',n2,',Alpha =',alpha,',R = ',R)) +
    ggtitle('Simulated Power Curve of Mann Whitney U Test & T Test') +
    theme_bw(14)
  
  #Self_defined theme 
  mytheme <- theme(plot.subtitle = element_text(family = "mono",size = 11,
                                                face = "bold",hjust =
                                                  0.01),axis.title = element_text(family = "serif"),
                   axis.text = element_text(size = 10),plot.title =
                     element_text(family = "serif",colour = "red", hjust = -0.01),
                   legend.text = element_text(size = 10,family = "serif"), 
                   legend.title = element_text(family = "serif"),legend.background = element_blank(),
                   legend.box.background = element_rect(colour = "black"))
  #Final Output
  return(graph.1 + mytheme)
} 