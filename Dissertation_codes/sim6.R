rm(list = ls(all = T)) #Removes all data
#Loading Required Library
library(ggplot2) #for graphics
library(wesanderson) #for colours

# Here We will do a comparative study of Wilcoxon Rank Sum U test &
# Exponential LRT in two sample problems, where data is from Exponential 
Distribution
# A function to calculate U statistic
Wilcoxon_Ranksum_stat <- function(x,y){
  z <- c(x,y)
  u <- sum(rank(z)[c(1:length(x))]) #to store the value
  
  return(u) #returns the value of u
}
Power_comparison.LRT3 <- function(n1,n2,d,lamda,R,alpha,exact.crit = F){
  
  # The function takes sample sizes,a vector of differences &
  # Level of Significance as argument
  
  set.seed(seed = 987654321) #for uniformity of result
  
  power.matrix <- matrix(0,nrow = length(d),ncol = 2) #A matrix to store power values
  colnames(power.matrix) = c('Wilcoxon Ranksum test','Exponential LRT')
  
  
  for(i in 1:length(d)){
    
    test.statistic <- replicate(R,{
      
      x <- rexp(n1,rate = 1/(lamda + d[i])); y <- rexp(n2,rate = 1/lamda) 
      #our sample
      
      #computation of test statistics
      
      #Wilcoxon Rank Sum test statistic
      WilcoxonRankSum.statistic <- Wilcoxon_Ranksum_stat(x,y)
      
      #Exponential LRT statistic
      Exponential_LRT.statistic <- mean(x)/mean(y)
      
      #Storing the values 
      c(WilcoxonRankSum.statistic,Exponential_LRT.statistic)
    })
    
    #Simulated power for Wilcoxon Rank Sum test
    if(exact.crit){
      WilcoxonRankSum.power <- mean(test.statistic[1,] > exact.crit) 
    }else{
      a <- (n1*(n1+n2+1))/2 ; b <- n1*n2*(n1+n2+1)/12
      cutpoint.1 <- qnorm(alpha,lower.tail = F)
      WilcoxonRankSum.power <- mean((test.statistic[1,] - a - 0.5)/sqrt(b) 
                                    > cutpoint.1) #after continuity correction
    }
    
    #Simulated power for Exponential LRT of equality of mean
    cutpoint.2 <- qf(alpha,2*n1,2*n2,lower.tail = F)
    Exponential_LRT.power <- mean(test.statistic[2,] > cutpoint.2)
    
    #Storing the simulated powers in matrix
    power.matrix[i,] <- c(WilcoxonRankSum.power,Exponential_LRT.power)
  }
  
  power.matrix <- cbind(Diiference = d,power.matrix) #adding the difference column
  
  return(power.matrix) #returns a matrix 
}
#Function to draw power Curve
Visualize_Power_comparison.LRT3 <-
  function(n1,n2,d,lamda,R,alpha,exact.crit = F){
    
    #Storing Simulated Powers
    M <- Power_comparison.LRT3(n1,n2,d,lamda,R,alpha,exact.crit)
    
    #Graph
    index.1 <- rep(c('Wilcoxon Ranksum \n Test','Exponential \n LRT'),each =
                     nrow(M))
    graph.data <- data.frame(x = c(M[,1],M[,1]),y = c(M[,2],M[,3]),Index =
                               index.1)
    graph.1 <- ggplot(graph.data,aes(x,y,col = Index)) +
      geom_hline(yintercept = 1,linetype = 'dashed') + geom_line(size = 1.5) 
    + geom_point(show.legend = F,size = 2) +
      labs(x = expression(paste('Difference ',(mu[1] - mu[2]))),y = 'Simulated Power',
           subtitle = paste('Lamda = ',lamda,',n1 =',n1,',n2 = ',n2,',Alpha =',alpha,',R = ',R)) +
      ggtitle('Simulated Power Curve of Wilcoxon Ranksum Test & Exponential LRT') +
      theme_bw(14)
    
    #Self_defined theme 
    mytheme <- theme(plot.subtitle = element_text(family = "mono",size = 11,
                    face = "bold",hjust =0.01),axis.title = element_text(family = "serif"),
                     axis.text = element_text(size = 10),plot.title =
                       element_text(family = "serif",
                                    colour = "red", hjust = -0.01),
                     legend.text = element_text(size = 10,family = "serif"), 
                     legend.title = element_text(family ="serif"),legend.background = element_blank(),
                     legend.box.background = element_rect(colour = "black"))
    #Final Output
    return(graph.1 + mytheme)
}