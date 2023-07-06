rm(list = ls(all = T)) #Removes all data
#Loading Required Library
library(ggplot2) #for graphics
library(wesanderson) #for colours

# Here We will do a comparative study of Different Non-Parametric &
# Exponential LRT in two sample problems, where data is from Exponential Distribution
Power_comparisonALL.2 <- function(n1,n2,d,lamda,R,alpha,exact.crit =c(F,F,F)){
  
  # The function takes sample sizes,a vector of differences &
  # Level of Significance as argument
  
  set.seed(seed = 987654321) #for uniformity of result
  
  power.matrix <- matrix(0,nrow = length(d),ncol = 4) #A matrix to store power values
  colnames(power.matrix) = c('Mann Whitney U Test','Wilcoxon Rank-Sum Test','Kolmogorov Smirnov Test','Exponential LRT')
  
  for(i in 1:length(d)){
    test.statistic <- replicate(R,{
      
      x <- rexp(n1,rate = 1/(lamda + d[i])); y <- rexp(n2,rate =
                                                         1/lamda) #our sample
      
      #computation of test statistics
      
      #Wilcoxon Rank Sum test statistic
      WilcoxonRankSum.statistic <- Wilcoxon_Ranksum_stat(x,y)
      
      #Mann Whitney U test statistic
      MWU.statistic <- Ustat(x,y)
      
      #Kolmogorov Smirnov test statistic
      Kolmogorov_smirnov.statistic <- Kolmogorv_smirnov.stat(x,y)
      
      #Exponential LRT statistic
      Exponential_LRT.statistic <- mean(x)/mean(y)
      
      #Storing the values 
      
      c(MWU.statistic,WilcoxonRankSum.statistic,Kolmogorov_smirnov.statistic,Exponential_LRT.statistic)
    })
    
    #Simulated power for Mann Whitney U test
    if(exact.crit[1]){
      MNW.power <- mean(test.statistic[1,] > exact.crit[1]) 
    }else{
      a <- (n1*n2)/2 ; b <- n1*n2*(n1+n2+1)/12
      cutpoint.1 <- qnorm(alpha,lower.tail = F)
      MNW.power <- mean((test.statistic[1,] - a- 0.5)/sqrt(b) >
                          cutpoint.1) #after continuity correction
    }
    
    #Simulated power for Wilcoxon Rank Sum test
    if(exact.crit[2]){
      WilcoxonRankSum.power <- mean(test.statistic[2,] > exact.crit[2]) 
    }else{
      a <- (n1*(n1+n2+1))/2 ; b <- n1*n2*(n1+n2+1)/12
      cutpoint.2 <- qnorm(alpha,lower.tail = F)
      WilcoxonRankSum.power <- mean((test.statistic[2,] - a -
                                       0.5)/sqrt(b) > cutpoint.2) #after continuity correction
    }
    
    #Simulated power for Kolmogorov Smirnov test
    if(exact.crit[3]){
      Kolmogorov_smirnov.power <- mean(test.statistic[3,] >
                                         exact.crit[3]) 
    }else{
      cut.point.3 <- sqrt(-log(alpha)/2)
      Kolmogorov_smirnov.power <-
        mean(test.statistic[3,]*sqrt((n1*n2)/(n1+n2)) > cut.point.3)
    }
    
    #Simulated power for Exponential LRT
    cutpoint.4 <- qf(alpha,2*n1,2*n2,lower.tail = F)
    Exponential_LRT.power <- mean(test.statistic[4,] > cutpoint.4)
    
    #Storing the simulated powers in matrix
    power.matrix[i,] <-
      c(MNW.power,WilcoxonRankSum.power,Kolmogorov_smirnov.power,Exponential_LRT.power)
  }
  
  power.matrix <- cbind(Diiference = d,power.matrix) #adding the difference column
  
  return(power.matrix) #returns a matrix 
}
#Function to draw power curve
Visualize_Power_comparisonALL.2 <-function(n1,n2,d,lamda,R,alpha,exact.crit = c(F,F,F)){
    
    #Storing power in matrix
    M <- Power_comparisonALL.2(n1,n2,d,lamda,R,alpha,exact.crit)
    
    #Graph
    index.1 <- factor(rep(c('Mann Whitney \n U Test','Wilcoxon Ranksum \nTest','Kolmogorov-Smirnov \n Test','Exponential \n LRT'),each = nrow(M)),
                      levels = c('Mann Whitney \n U Test','Wilcoxon Ranksum \n Test','Kolmogorov-Smirnov \n Test','Exponential \n LRT'))
    graph.data <- data.frame(x = c(M[,1],M[,1],M[,1],M[,1]),y =
                               c(M[,2],M[,3],M[,4],M[,5]),Index = index.1)
    graph.1 <- ggplot(graph.data,aes(x,y,col = Index)) +
      geom_hline(yintercept = 1,linetype = 'dashed') +
      geom_line(aes(linetype = Index),size = 1) +
      geom_point(show.legend = F,size = 2) +
      labs(x = expression(paste('Difference ',(mu[1] - mu[2]))),y =
             'Simulated Power',
           subtitle = paste('Lamda = ',lamda,',n1 =',n1,',n2 = ',n2,',Alpha =',alpha,',R = ',R)) +
      ggtitle('Simulated Power Curve of Different Non Parametric Tests and Exponential LRT') +
      theme_bw(14) + scale_color_manual(values = wes_palette(n = 4,name =
                                                               "Darjeeling1"))
    
    #Self_defined theme 
    mytheme <- theme(plot.subtitle = element_text(family = "mono",size = 11,
                                                  face = "bold",hjust =
                                                    0.01),axis.title = element_text(family = "serif"),
                     axis.text = element_text(size = 10),plot.title =
                      element_text(family = "serif",colour = "red", hjust = -0.01),
                     legend.text = element_text(size = 10,family = "serif"), 
                     legend.title = element_text(family ="serif"),legend.background = element_blank(),
                     legend.box.background = element_rect(colour = "black"))
    #Final Output
    return(graph.1 + mytheme)
}
