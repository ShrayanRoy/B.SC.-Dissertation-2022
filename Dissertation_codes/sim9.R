rm(list = ls(all = T)) #Removes all data
#Loading Required Library
library(ggplot2) #for graphics
library(wesanderson) #for colours

#Function to draw histogram Simulated distribution of Kolmogorov Smirnov test statistic
Simulated_KolmogorovSmirnov <- function(n1,n2,mu,d,R){
  
  set.seed(seed = 987654321) #for uniformity
  
  Simulated.Distribution <- NULL #to store statistic value
  
  for(i in 1:length(d)){
    
    test.statistic <- replicate(R,{ 
      
      #sample from normal distribution
      x.normal <- rnorm(n1,mean = mu + d[i],sd = 1)
      y.normal <- rnorm(n2,mean = mu,sd = 1)
      
      #sample from exponential distribution
      x.exp <- rexp(n1,rate = 1/(mu + d[i]))
      y.exp <- rexp(n2,rate = 1/mu)
      
      #calculating value of statistic
      KS.stat_Normal <- Kolmogorv_smirnov.stat(x.normal,y.normal)
      KS.stat_Exp <- Kolmogorv_smirnov.stat(x.exp,y.exp)
      
      c(KS.stat_Normal,KS.stat_Exp)
      
    }) 
    
    #Simulated Distribution
    Index.1 <- rep(paste('Difference = ',d[i]),each = R)
    Index.2 <- rep(c('Data From \n Normal','Data From \n Exponential'),each = R)
    M <- data.frame(Value = c(test.statistic[1,],test.statistic[2,]),
                    Index1 = c(Index.1,Index.1),
                    Index2 = Index.2)
    Simulated.Distribution <- rbind(Simulated.Distribution,M)
  }
  #Graph data
  Simulated.Distribution[,2] <- factor(Simulated.Distribution[,2],levels =
                                         paste('Difference = ',d))
  graphdata <- data.frame(Value = Simulated.Distribution[,1],Index =
                            Simulated.Distribution[,3],
                          diff_index = Simulated.Distribution[,2])
  
  #Graph
  graph.1 <- ggplot(graphdata,aes(Value,fill = Index)) +
    geom_histogram(aes(y = stat(count) / sum(count)),alpha = 0.6,bins =
                     15,col = 'black',
                   position = 'identity') +
    labs(x = '',y = 'Density',title = 'Simulated Distribution of Kolmogorov Smirnov Statistic',
         subtitle = paste('Mu = ',mu,',n1 = ',n1,',n2 = ',n2,',R = ',R)) +
    facet_wrap(.~diff_index) + scale_x_continuous(breaks=seq(0,1,by=0.1)) 
  +
    theme_bw(14)
  
  #Self Defined Theme
  mytheme <- theme(plot.subtitle = element_text(family = "mono",size = 11,
                                                face = "bold",hjust =0.01),axis.title = element_text(family = "serif"),
                   axis.text = element_text(size = 10),plot.title =
                     element_text(family = "serif",colour = "red", hjust = -0.01),
                   legend.text = element_text(size = 10,family = "serif"), 
                   legend.title = element_text(family = "serif"),legend.background = element_blank(),
                   legend.box.background = element_rect(colour = "black"))
  
  return(graph.1 + mytheme)
}
#Function to draw histogram Simulated distribution of Mann Whitney U statistic
Simulated_MannWhiteny <- function(n1,n2,mu,d,R){
  
  set.seed(seed = 987654321) #for uniformity
  
  Simulated.Distribution <- NULL #to store the data
  
  for(i in 1:length(d)){
    
    test.statistic <- replicate(R,{ 
      
      #sample from normal distribution
      x.normal <- rnorm(n1,mean = mu + d[i],sd = 1)
      y.normal <- rnorm(n2,mean = mu,sd = 1)
      
      #sample from exponential distribution
      x.exp <- rexp(n1,rate = 1/(mu + d[i]))
      y.exp <- rexp(n2,rate = 1/mu)
      
      #calculating value of statistic
      MNW.stat_Normal <- Ustat(x.normal,y.normal)
      MNW.stat_Exp <- Ustat(x.exp,y.exp)
      
      c(MNW.stat_Normal,MNW.stat_Exp)
      
    }) 
    
    #Simulated Distribution
    Index.1 <- rep(paste('Difference = ',d[i]),each = R)
    Index.2 <- rep(c('Data From \n Normal','Data From \n Exponential'),each = R)
    M <- data.frame(Value = c(test.statistic[1,],test.statistic[2,]),
                    Index1 = c(Index.1,Index.1),
                    Index2 = Index.2)
    Simulated.Distribution <- rbind(Simulated.Distribution,M)
  }
  
  #Graph data
  Simulated.Distribution[,2] <- factor(Simulated.Distribution[,2],levels =
                                         paste('Difference = ',d))
  graphdata <- data.frame(Value = Simulated.Distribution[,1],Index =
                            Simulated.Distribution[,3],
                          diff_index = Simulated.Distribution[,2])
  
  #Graph
  graph.1 <- ggplot(graphdata,aes(Value,fill = Index)) +
    geom_histogram(aes(y = stat(count) / sum(count)),alpha = 0.6,bins = 15,col = 'black',
                   position = 'identity') +
    labs(x = '',y = 'Density',title = 'Simulated Distribution of Mann Whitney U Statistic',
         subtitle = paste('Mu = ',mu,',n1 = ',n1,',n2 = ',n2,',R = ',R)) +
    facet_wrap(.~diff_index) + theme_bw(14)
  
  #Self Defined Theme
  mytheme <- theme(plot.subtitle = element_text(family = "mono",size = 11,
                                                face = "bold",hjust = 0.01),axis.title = element_text(family = "serif"),
                   axis.text = element_text(size = 10),plot.title =
                     element_text(family = "serif",
                                  colour = "red", hjust = -0.01),
                   legend.text = element_text(size = 10,family = "serif"), 
                   legend.title = element_text(family = "serif"),legend.background = element_blank(),
                   legend.box.background = element_rect(colour = "black"))
  return(graph.1 + mytheme)
}
#Function to draw histogram Simulated distribution of Wilcoxon Rank Sum test statistic
Simulated_WilcoxonRanksum <- function(n1,n2,mu,d,R){
  
  set.seed(seed = 987654321) #for uniformity
  
  Simulated.Distribution <- NULL #to store statistic value
  
  for(i in 1:length(d)){
    
    test.statistic <- replicate(R,{ 
      
      #sample from normal distribution
      x.normal <- rnorm(n1,mean = mu + d[i],sd = 1)
      y.normal <- rnorm(n2,mean = mu,sd = 1)
      
      #sample from exponential distribution
      x.exp <- rexp(n1,rate = 1/(mu + d[i]))
      y.exp <- rexp(n2,rate = 1/mu)
      
      #calculating value of statistic
      Wilcoxon.stat_Normal <- Wilcoxon_Ranksum_stat(x.normal,y.normal)
      Wilcoxon.stat_Exp <- Wilcoxon_Ranksum_stat(x.exp,y.exp)
      
      c(Wilcoxon.stat_Normal,Wilcoxon.stat_Exp)
      
    }) 
    
    #Simulated Distribution
    Index.1 <- rep(paste('Difference = ',d[i]),each = R)
    Index.2 <- rep(c('Data From \n Normal','Data From \n Exponential'),each = R)
    M <- data.frame(Value = c(test.statistic[1,],test.statistic[2,]),
                    Index1 = c(Index.1,Index.1),
                    Index2 = Index.2)
    Simulated.Distribution <- rbind(Simulated.Distribution,M)
  }
  
  #Graph data
  Simulated.Distribution[,2] <- factor(Simulated.Distribution[,2],levels =
                                         paste('Difference = ',d))
  graphdata <- data.frame(Value = Simulated.Distribution[,1],Index =
                            Simulated.Distribution[,3],
                          diff_index = Simulated.Distribution[,2])
  
  #Graph
  graph.1 <- ggplot(graphdata,aes(Value,fill = Index)) +
    geom_histogram(aes(y = stat(count) / sum(count)),alpha = 0.6,bins =
                     15,col = 'black',
                   position = 'identity') +
    labs(x = '',y = 'Density',title = 'Simulated Distribution of Wilcoxon Ranksum Statistic',
         subtitle = paste('Mu = ',mu,',n1 = ',n1,',n2 = ',n2,',R = ',R)) +
    facet_wrap(.~diff_index) + theme_bw(14)
  
  #Self Defined Theme
  mytheme <- theme(plot.subtitle = element_text(family = "mono",size = 11,
                                                face = "bold",hjust =
                                                  0.01),axis.title = element_text(family = "serif"),
                   axis.text = element_text(size = 10),plot.title =
                     element_text(family = "serif",colour = "red", hjust = -0.01),
                   legend.text = element_text(size = 10,family = "serif"), 
                   legend.title = element_text(family = "serif"),legend.background = element_blank(),
                   legend.box.background = element_rect(colour = "black"))
  
  return(graph.1 + mytheme)
}  