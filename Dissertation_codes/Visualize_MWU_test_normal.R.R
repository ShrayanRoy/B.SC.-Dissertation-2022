rm(list = ls(all = T))   #removes all objects

#Installing Required packages
library(ggplot2)
library(gganimate)

# Here We will do a comparative study of Mann Whitney U test &
# T test in two sample problems, where data is from Normal Distribution

# A function to calculate U statistic
Ustat <- function(x,y){
  u <- 0         #to store the value 
  for(i in 1:length(x)){
    u <-  u + sum(y < x[i])
  }
  return(u)   #returns the value of u
}

#A function to create an animated graph
#Description : The function takes sample sizes,difference b/w means,
#Replication number,alpha,critical value for non parametric test as input and produces
#a simulated distribution of the two tests.

visualize1 <- function(n1,n2,d,R,alpha,crit.value){
  
 set.seed(seed = 987654321)   #for uniformity of result

  mu <- 0      #Value of Mu
 
 graph.matrix <- NULL
 
 for(i in 1:length(d)){ 
   
  test.statistic <- replicate(R,{
    
   #Samples from Normal Distribution
    x <- rnorm(n1,mu + d[i],1)
    y <- rnorm(n2,mu,1)
  
   #computing Mann Whitney U statistic    
    MWU.statistic <- Ustat(x,y)
    
   #computing T statistic
    pooled.var <- ((n1-1)*var(x) + (n2 -1)*var(y))/(n1+n2-2)
    T.statistic <- (mean(x) - mean(y))/sqrt(pooled.var*((1/n1) + (1/n2)))
    
   #Storing the statistic
    c(MWU.statistic,T.statistic)
      
  })
  
  # A matrix to store the values 
   M <- matrix(c(rep(d[i],each = R),test.statistic[1,],test.statistic[2,]),ncol = 3,nrow = R,byrow = F)
   graph.matrix <- rbind(graph.matrix,M)
  
 }
  
 #Index for graph and data.frame to create a ggplot
  index.1 <- rep(c('Mann Whitney U Statistic','T statistic'),each = nrow(graph.matrix))
  graph.data <- data.frame(d = c(graph.matrix[,1],graph.matrix[,1]),
                           value = c(graph.matrix[,2],graph.matrix[,3]),
                           Index = index.1)
  
  #A function to calculate the vertical line at critical point
  my.intercept <- function(x){
    ifelse(x == 'Mann Whitney U Statistic',crit.value,qt(alpha,n1+n2-2,lower.tail = F))
  }
  
  #Initial graph without animation
  graph.1 <- ggplot(graph.data,aes(value,fill = Index)) +
    geom_histogram(aes(y = stat(count) / sum(count)),bins = 13,show.legend = F,col = 'black') +
    ggtitle('Mann Whitney U statistic & T statistic')+ facet_wrap(.~ Index,scales = 'free_x') +
    theme_bw(14) + geom_vline(aes(xintercept = my.intercept(index.1)),linetype = 'dashed')
  
  #Final graph with animation
  graph.2 <- graph.1 + labs(subtitle = 'Difference : {frame_time}',x = ' ',y = 'Density',caption = paste('Note: n1 =',n1,',n2 = ',n2,'& R = ',R)) +
    theme(plot.caption= element_text(size = 13,color = 'black')) + 
    ggtitle('  Simulated Distribution : \n Mann Whitney U statistic & T statistic') + transition_time(d) + ease_aes('linear')
  
  #Self Defined Theme
  mytheme <- theme(plot.subtitle = element_text(family = "mono",size = 13,
                                                face = "bold",hjust = 0.01),axis.title = element_text(family = "serif"),
                   axis.text = element_text(size = 10),plot.title = element_text(size= 20,family = "serif",colour = "red", hjust = -0.01),
                   plot.caption = element_text(size = 12,hjust = 0.5))
  
  #Adjusting our animated graph to our desired requirement
  animate(graph.2 + mytheme,duration = 20,height = 800,width = 800)
 
}

visualize1(20,16,seq(0,4,by = 0.2),1000,0.05,17/30)
