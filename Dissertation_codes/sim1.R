rm(list = ls(all = T)) #Removes all data
#Loading Required Library
library(ggplot2) #for graphics
library(wesanderson) #for colours

# Here We will do a comparative study of Kolmogorov Smirnov test &
# T test in two sample problems, where data is from Normal Distribution
# A function to calculate D statistic
Kolmogorv_smirnov.stat <- function(x,y){
  
  Fx <- ecdf(x) #empirical CDF of X
  Fy <- ecdf(y) #empirical CDF of Y
  
  D <- max(Fy(c(x,y)) - Fx(c(x,y))) #computing D statistic
  
  return(D) #returns the value of u
}
#Equal Variance
Power_comparison.T1 <- function(n1,n2,d,mu,R,alpha,exact.crit = F){
  
  # The function takes sample sizes,a vector of differences &
  # Level of Significance as argument
  
  set.seed(seed = 987654321) #for uniformity of result
  
  power.matrix <- matrix(0,nrow = length(d),ncol = 2) #A matrix to store power values
  colnames(power.matrix) = c('Kolmogorov Smirnov Test','T Test')
  for(i in 1:length(d)){
    
    test.statistic <- replicate(R,{
      
      x <- rnorm(n1,d[i] + mu,1); y <- rnorm(n2,mu,1) #our sample
      
      #computation of test statistics
      
      #Kolmogorov U test statistic
      Kolmogorov_smirnov.statistic <- Kolmogorv_smirnov.stat(x,y)
      
      #T test statistic
      est.var <- ((n1-1)*var(x) + (n2-1)*var(y))/(n1+n2-2) #pooled variance
      T.statistic <- (mean(x) - mean(y))/sqrt(est.var*((1/n1) + (1/n2)))
      
      #Storing the values 
      c(Kolmogorov_smirnov.statistic,T.statistic)
    })
    
    #Simulated power for Kolmogorov Smirnov test
    if(exact.crit){
      Kolmogorov_smirnov.power <- mean(test.statistic[1,] > exact.crit)
    }else{
      cut.point.1 <- sqrt(-log(alpha)/2)
      Kolmogorov_smirnov.power <-
        mean(test.statistic[1,]*sqrt((n1*n2)/(n1+n2)) > cut.point.1)
    }
    
    #Simulated power for t test
    cutpoint.2 <- qt(alpha,n1+n2-2,lower.tail = F)
    T.power <- mean(test.statistic[2,] > cutpoint.2)
    
    #Storing the simulated powers in matrix
    power.matrix[i,] <- c(Kolmogorov_smirnov.power,T.power)
  }
  
  power.matrix <- cbind(Diiference = d,power.matrix) #adding the difference column
  
  return(power.matrix) #returns a matrix 
}
#Function to draw power curve
Visualize_Power_comparison.T1 <- function(n1,n2,d,mu,R,alpha,exact.crit =
                                            F){
  
  #Storing Simulated Powers
  M <- Power_comparison.T1(n1,n2,d,mu,R,alpha,exact.crit)
  
  #Graph
  index.1 <- rep(c('Kolmogorov-Smirnov \n Test','T Test '),each = nrow(M))
  graph.data <- data.frame(x = c(M[,1],M[,1]),y = c(M[,2],M[,3]),Index = index.1)
  graph.1 <- ggplot(graph.data,aes(x,y,col = Index)) +
    geom_hline(yintercept = 1,linetype = 'dashed') + geom_line(size = 1.5) 
  +
    geom_point(show.legend = F,size = 2) +
    labs(x = expression(paste('Difference ',(mu[1] - mu[2]))),y =
           'Simulated Power',
         subtitle = paste('Mu = ',mu,',n1 =',n1,',n2 = ',n2,',Alpha 
=',alpha,',R = ',R)) +
    ggtitle('Simulated Power Curve of Kolmogorov Smirnov Test & T Test') +
    theme_bw(14)
  
  #Self_defined theme 
  mytheme <- theme(plot.subtitle = element_text(family = "mono",size = 11,
                   face = "bold",hjust = 0.01),axis.title = element_text(family = "serif"),
                   axis.text = element_text(size = 10),plot.title =
                   element_text(family = "serif",
                   colour = "red", hjust = -0.01),legend.text = element_text(size = 10,family = "serif"), 
                   legend.title = element_text(family ="serif"),legend.background = element_blank(),
                   legend.box.background = element_rect(colour = "black"))
  #Final Output
  return(graph.1 + mytheme)
}

                             