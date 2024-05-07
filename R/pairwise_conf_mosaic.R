library(dplyr)
library(matrixStats)
library(egg)
library(PlackettLuce)
BradTerrTiles <- function(data, conf.int = 0.95, bootstrap = TRUE, reps = 100, point.est.yn = FALSE, plots.yn = TRUE){
  if (bootstrap == TRUE){
    n <- nrow(data)
    bootstrap_samples <- t(replicate(reps, {
      bootstrap <- data[sample(1:n,n,replace=T),]
      pl <- PlackettLuce(as.rankings(bootstrap, verbose = FALSE), verbose = FALSE) #as.rankings used here
      coef(pl,log=FALSE)
    }))
  } else {
    bootstrap_samples <- data
  }
  
  boot <- data.frame(bootstrap_samples)
  mns <- colMedians(data.matrix(boot), na.rm = TRUE)
  boot <- boot[,order(mns)]
  names <- colnames(boot) #Names in order by median
  
  BradTerrLow <- c()
  for(i in 1:length(names)){
    for(j in 1:length(names)){
      BradTerrLow <- append(BradTerrLow, quantile(boot[[names[i]]]/(boot[[names[i]]] + boot[[names[j]]]),(1-conf.int)/2))
    }
  }
  x<-rep(1:length(names), each = length(names))
  y<-rep(1:length(names), times = length(names))
  
  df1 <- data.frame(BradTerrLow,x,y)
  p1 <- ggplot(data=df1, aes(x=as.factor(x), y=as.factor(y), fill=BradTerrLow)) +
    geom_raster() +
    scale_fill_gradient2(midpoint = 0.5, limits = c(0,1))
  
  BradTerrMid <- c()
  for(i in 1:length(names)){
    for(j in 1:length(names)){
      BradTerrMid <- append(BradTerrMid, quantile(boot[[names[i]]]/(boot[[names[i]]] + boot[[names[j]]]), .5))
    }
  }
  df2 <- data.frame(BradTerrMid,x,y)
  p2<- ggplot(data=df2, aes(x=as.factor(x), y=as.factor(y), fill=BradTerrMid)) +
    geom_raster() +
    scale_fill_gradient2(midpoint = 0.5, limits = c(0,1))
  
  BradTerrHigh <- c()
  for(i in 1:length(names)){
    for(j in 1:length(names)){
      BradTerrHigh <- append(BradTerrHigh, quantile(boot[[names[i]]]/(boot[[names[i]]] + boot[[names[j]]]), 1-(1-conf.int)/2))
    }
  }
  #j = y axis, incremented first. i = x axis. probability that name x beats name y = fill
  df3 <- data.frame(BradTerrHigh,x,y)
  
  p3 <- ggplot(data=df3, aes(x=as.factor(x), y=as.factor(y), fill=BradTerrHigh)) +
    geom_raster() +
    scale_fill_gradient2(midpoint = 0.5, limits = c(0,1))
  
  if (plots.yn == TRUE) {
    #egg
    if(point.est.yn == FALSE){
      return(ggarrange(p1 + 
                         theme_minimal() +
                         theme(axis.title.y=element_blank(),
                               axis.title.x=element_blank(),
                               axis.ticks.y=element_blank(),
                               legend.position="none",
                               plot.margin = margin(l = 1)) +
                         labs(title = paste(((1-conf.int)/2)*100,"% Quantile",sep="")),
                       p2 + 
                         theme_minimal() +
                         theme(axis.title.y=element_blank(),
                               axis.title.x=element_blank(),
                               axis.text.y=element_blank(),
                               axis.ticks.y=element_blank(),
                               plot.margin = margin(r = 1, l = 1),
                               legend.position="bottom") +
                         labs(fill = "Pairwise comparison probability", title = "Median"),
                       p3 + 
                         theme_minimal() +
                         theme(legend.position="none",
                               axis.title.x=element_blank(),
                               axis.title.y=element_blank(),
                               axis.text.y=element_blank(),
                               axis.ticks.y=element_blank(),
                               plot.margin = margin(r = 1)) +
                         labs(title = paste((1-((1-conf.int)/2))*100,"% Quantile",sep="")),
                       nrow = 1
      )
      )
    } else {
      return(
              p2 + 
              theme_minimal() +
              theme(axis.title.y=element_blank(),
                    axis.title.x=element_blank(),
                    plot.margin = margin(r = 1, l = 1),
                    legend.position="bottom") +
              labs(fill = "Pairwise comparison probability")
            )
    }
  } else {
    if(point.est.yn == FALSE){
      return(list(names, df1, df2, df3))
    } else {
      return(list(names, df2))
    }
  }
}