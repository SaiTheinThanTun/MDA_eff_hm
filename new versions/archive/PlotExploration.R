# library(rasterVis)
# #library(hplot)
# 
# toPlot <- v12m #t(v12m)
# r_ <- raster(ncol=ncol(toPlot),nrow=nrow(toPlot))
# for(i in 1:nrow(toPlot)){
#   for(j in 1:ncol(toPlot)){
#     r_[i,j] <- toPlot[i,j]
#   }
# }
# 
# r_ <- ratify(r_)
# 
# rat_ <- levels(r_)[[1]]
# rat_$elimination <- c('no village','1 village')
# #rat_$class <- c('A1', 'B2', 'C3')
# levels(r_) <- rat_
# rasterVis::levelplot(r_, att='elimination',col.regions= c("black","red"))
# 
# #another test
# levelplot(t(v12m), cuts=1,col.regions=new.palette, xlab="% of homogeniety", ylab="% of MDA coverage in village 2", main="No. of villages reaching below elimination threshold")
# 
# r <- raster(ncol=3, nrow=3)
# r[] <- 1:ncell(r)
# as.raster(r)
# 
# #example
# r <- raster(nrow=10, ncol=10)
# r[] = 1
# r[51:100] = 3
# r[3:6, 1:5] = 5
# r <- ratify(r)
# 
# rat <- levels(r)[[1]]
# rat$landcover <- c('Pine', 'Oak', 'Meadow')
# rat$class <- c('A1', 'B2', 'C3')
# levels(r) <- rat
# 
# levelplot(r, col.regions=c('palegreen', 'midnightblue', 'indianred1'))
# 
# 

#on i9
library(ggplot2)
library(reshape)

result <- readRDS("results_homo_cov_start0/results_loop_6_2019-03-22 215223.rds")
#result <- readRDS("results_homo_cov_start0/results_loop_5_2019-03-22 202216.rds")
#result <- readRDS("results_homo_cov_start0_seas/results_loop_1_2019-03-22 230011.rds")
#result <- readRDS("results_homo_cov_start0_seas/results_loop_5_2019-03-23 075927.rds")
cmda_1 <- cmda_1Loop[loop]#80 #90

#find where the invalid result is
#tmp <- sapply(result, function(x){(nrow(x)<193)})
#tmp2[loop] <- sum(tmp)
#which(tmp==TRUE)

#result[[1501]]

village1 <- sapply(result, function(x){
  sum(x[MDAstart:(MDAstart+successwithin),1])>0
})
village2 <- sapply(result, function(x){
  sum(x[MDAstart:(MDAstart+successwithin),2])>0
})

#putting into matrix
v1m <- matrix(as.numeric(village1),nrow=81,ncol=101, byrow=TRUE)
#heatmap(v1m, Rowv=NA, Colv = NA)
#v1md <- as.data.frame(as.numeric(village1),nrow=80,ncol=100, byrow=TRUE)
levelplot(t(v1m))

v2m <- matrix(as.numeric(village2),nrow=81,ncol=101, byrow=TRUE)
#heatmap(v2m, Rowv=NA, Colv = NA)
levelplot(t(v2m))

v12m <- matrix(as.numeric(village1),nrow=81,ncol=101, byrow=TRUE)+matrix(as.numeric(village2),nrow=81,ncol=101, byrow=TRUE)
#heatmap(v12m, Rowv=NA, Colv = NA, col=heat.colors(3))
#write.csv(v12m,'results_homo_cov/v12m.csv')

tmp <- melt(t(v12m))


ggplot(data=tmp, aes(x=X1, y=X2))+
  geom_tile(aes(fill=factor(value)))+
  ggtitle(paste0("No. of villages getting below elimination threshold\nMDA coverage in village1: ",cmda_1Loop))+
  xlab("% of homogeniety")+ylab("% of MDA coverage in village 2")+
  theme(legend.position = "bottom")+
  scale_fill_manual(name="# of village(s)", labels=c("zero", "one village", "two village"),values=c("#999999", "#E69F00", "#56B4E9"))
  #scale_fill_discrete(name="# of village(s)", labels=c("zero", "one village", "two village",values=c("#999999", "#E69F00", "#56B4E9")))
  #scale_fill_manual(name="# of village(s)", labels=c("zero", "one village", "two village"),values=c("#999999", "#E69F00", "#56B4E9"))
  




