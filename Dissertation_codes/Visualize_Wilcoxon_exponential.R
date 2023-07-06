rm(list = ls(all = T))    #removes all objects

#Installing Required packages
library(ggplot2)
library(gganimate)

# Here We will do a comparative study of Wilcoxon Ranksum test &
# T test in two sample problems, where data is from Exponential Distribution

# A function to calculate U statistic
Wilcoxon_Ranksum_stat <- function(x,y){
  z <- c(x,y)
  u <- sum(rank(z)[c(1:length(x))])         #to store the value 
  
  return(u)   #returns the value of u
}

#A function to create an animated graph
#Description : The function takes sample sizes,difference b/w means,
#Replication number,alpha,critical value for non parametric test as input and produces
#a simulated distribution of the two tests.

visualize6 <- function(n1,n2,d,R,alpha,crit.value){
  
  set.seed(seed = 987654321)   #for uniformity of result

  lamda <- 1      #Value of Lamda
  
  graph.matrix <- NULL
  
  for(i in 1:length(d)){ 
    
    test.statistic <- replicate(R,{
      
      #Samples from Exponential Distribution
      x <- rexp(n1,rate = 1/(lamda+d[i]))
      y <- rexp(n2,rate = 1/lamda)
      
      #computing Wilcoxon Ranksum statistic    
      Wilcoxon_ranksum.statistic <- Wilcoxon_Ranksum_stat(x,y)
      
      #computing Exponential LRT statistic
      EXP_LRT.statistic <- mean(x)/mean(y)
      
      #Storing the statistic
      c(Wilcoxon_ranksum.statistic,EXP_LRT.statistic)
      
    })
    
    # A matrix to store the values 
    M <- matrix(c(rep(d[i],each = R),test.statistic[1,],test.statistic[2,]),ncol = 3,nrow = R,byrow = F)
    graph.matrix <- rbind(graph.matrix,M)
    
  }
  
  #Index for graph and data.frame to create a ggplot
  index.1 <- factor(rep(c('Wilcoxon RankSum Statistic','EXP LRT statistic'),each = nrow(graph.matrix)),levels = c('Wilcoxon RankSum Statistic','EXP LRT statistic'))
  graph.data <- data.frame(d = c(graph.matrix[,1],graph.matrix[,1]),
                           value = c(graph.matrix[,2],graph.matrix[,3]),
                           Index = index.1)
  
  #A function to calculate the vertical line at critical point
  my.intercept <- function(x){
    ifelse(x == 'Wilcoxon RankSum Statistic',crit.value,qf(alpha,2*n1,2*n2,lower.tail = F))
  }
  
  #Initial graph without animation
  graph.1 <- ggplot(graph.data,aes(value,fill = Index)) +
    geom_histogram(aes(y = stat(count) / sum(count)),bins = 14,show.legend = F,col = 'black') +
    ggtitle('Wilcoxon Ranksum statistic & Exponential LRT statistic')+ facet_wrap(.~ Index,scales = 'free_x') +
    theme_bw(14) + geom_vline(aes(xintercept = my.intercept(index.1)),linetype = 'dashed')
  
  #Final graph with animation
  graph.2 <- graph.1 + labs(subtitle = 'Difference : {frame_time}',x = '',y = 'Density',caption = paste('Note: n1 =',n1,',n2 = ',n2,'& R = ',R)) +
    theme(plot.caption= element_text(size = 13,color = 'black')) + 
    ggtitle('  Simulated Distribution : \n Wilcoxon Ranksum statistic & Exponential LRT statistic') + transition_time(d) + ease_aes('linear')
  
  #Self Defined Theme
  mytheme <- theme(plot.subtitle = element_text(family = "mono",size = 13,
                                                face = "bold",hjust = 0.01),axis.title = element_text(family = "serif"),
                   axis.text = element_text(size = 10),plot.title = element_text(size= 20,family = "serif",colour = "red", hjust = -0.01),
                   plot.caption = element_text(size = 12,hjust = 0.5))
  
  #Adjusting our animated graph to our desired requirement
  animate(graph.2 + mytheme,duration = 20,height = 800,width = 800)
  
}

visualize6(6,10,seq(0,15,by = 1),1000,0.05,66)
