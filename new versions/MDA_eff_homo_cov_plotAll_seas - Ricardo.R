#MDA_eff_homo_cov_forloop.R created only the RDS save files
#since one of the results doesn't have adequate rows
#each RDS file is read in and plotted individually

#to change the coloring to better represent achievement in separate village
#to think about low & high relative prevalence for additional 6 plots

mac <- 0
if(mac){
  path <- "~/OneDrive - Nexus365/MORU/Projects/TCE_MDA effect/MDA_eff_hm/"
} else { path <- "D:/OneDrive - Nexus365/MORU/Projects/TCE_MDA effect/MDA_eff_hm/"}

setwd(path)

library(deSolve)
library(shiny)
library(TSA)
library(Rcpp)
library(stringr)
#library(lattice)
library(ggplot2)
library(reshape)
library(grid)
#sourceCpp("functions/modGMS.cpp")
source("functions_latest/no longer app.R")
tm_1 <- 9
timeVector <- read.csv('parameters/times.csv')
MDAstart <- which(timeVector==(2018+tm_1/12))

#change 1
cmda_1Loop <- rep(seq(70, by=10, to=90) ,3)

#fixed color across categories in different dataset
col_tmp <- data.frame(a=c(0,1,2,3), b=c("Neither patch","Patch 1", "Patch 2", "Both patches"))
myColors <- c("#999999", "#E69F00", "#56B4E9", "#00008B")
names(myColors) <- col_tmp$b #levels(col_tmp$b)
colScale <- scale_fill_manual(name = "# of patch",values = myColors)

toPlotList <- list() #to store data to facet plot
# loopArray <- cmda_1Loop/10
incidenceLoop <- rep(c("Identical", "Lower", "Higher"),each=3)
#change 2
for(loop in 1:length(cmda_1Loop)){
  #1:3 Identical
  #4:6 Lower
  #7:9 Higher
  result <- readRDS(paste("Ricardo/facet/results_loop_", loop,".rds", sep="")) #facet
  

  cmda_1 <- cmda_1Loop[loop]#80 #90
  
  
  village1 <- sapply(result, function(x){
    (x[,2]<1)*1+(x[,2]>=1)*0
    
  })
  village2 <- sapply(result, function(x){
    (x[,4]<1)*2+(x[,4]>=1)*0
    
  })
  
  #putting into matrix
  #change 3
  v1m <- matrix(as.numeric(village1),nrow=100,ncol=101, byrow=TRUE)
  v2m <- matrix(as.numeric(village2),nrow=100,ncol=101, byrow=TRUE)
  v12m <- matrix(as.numeric(village1),nrow=100,ncol=101, byrow=TRUE)+matrix(as.numeric(village2),nrow=100,ncol=101, byrow=TRUE)
  # v1m <- matrix(as.numeric(village1),nrow=81,ncol=101, byrow=TRUE)
  # v2m <- matrix(as.numeric(village2),nrow=81,ncol=101, byrow=TRUE)
  # v12m <- matrix(as.numeric(village1),nrow=81,ncol=101, byrow=TRUE)+matrix(as.numeric(village2),nrow=81,ncol=101, byrow=TRUE)
  
  
  toPlot <- melt(t(v12m))
  toPlot$value <- factor(toPlot$value, levels=c(0,1,2,3), labels=c("Neither patch","Patch 1", "Patch 2", "Both patches"))
  toPlot$P1Coverage <- cmda_1Loop[loop]
  toPlot$P2Incidence <- incidenceLoop[loop]
  
  toPlotList[[loop]] <- toPlot
}


#Figure 2: Panel plot####
toPlotFinal <- do.call(rbind,toPlotList)

dat_text <- data.frame(
  label = c("D", "E", "F","G","H"," I","A","B","C"),
  P2Incidence = incidenceLoop,
  P1Coverage   = cmda_1Loop  #c(70, 80, 90)
  #tCol=c(rep("black",4),rep("white",2),"black",rep("white",2))
)


#change 3
# png(paste('Ricardo/results_homo_cov_start0_seas/_newHomogen/homogeniety_MDAcoverage_',cmda_1Loop[loop],"_",gsub("\\:","",Sys.time()),'.png',sep=''),height= 1600, width=1800, units= "px", res=300)
# png(paste('Ricardo/results_homo_cov_start0_seas_village2highAPI/_newHomogen/homogeniety_MDAcoverage_hiAPI',cmda_1Loop[loop],"_",gsub("\\:","",Sys.time()),'.png',sep=''),height= 1600, width=1800, units= "px", res=300) #highAPI
png(paste('Ricardo/facet/',"Fig2_facet","_",gsub("\\:","",Sys.time()),'.png',sep=''),height= 1800, width=1800, units= "px", res=300) #lowAPI
#png(paste('results_homo_cov_start0/newPlot_exactlyAt1Yr/homogeniety_MDAcoverage_',cmda_1Loop[loop],"_",gsub("\\:","",Sys.time()),'.png',sep=''),height= 1600, width=1800, units= "px", res=300)
p <- ggplot(data=toPlotFinal, aes(x=X1, y=X2))+
  facet_grid(P2Incidence~P1Coverage)+
  geom_tile(aes(fill=(value)))+
  ggtitle(paste0("                            MDA coverage in patch 1"))+
  xlab("% of connectedness")+ylab("% of MDA coverage in patch 2")+
  theme(legend.position = "bottom")+ colScale+
  theme(strip.text.y = element_text(angle=90))+
  geom_label(
    data    = dat_text,
    mapping = aes(x=10,y=90, label=label),
    label.padding = unit(0.15, "lines"),label.r = unit(0.08, "lines")
  )+
  theme(plot.margin = unit(c(0,2,0,0), "lines"))
print(p)
grid.text("Incidence in patch 2 compared to patch 1",x = unit(0.95, "npc"), y = unit(0.50, "npc"), rot=90)
#grid.text("X",x = unit(0.59, "npc"), y = unit(0.58, "npc"))
grid.points(x = unit(0.60, "npc"), y = unit(0.57, "npc"), pch=8, gp=gpar(col="red", cex=.5))

dev.off()

# geom_text(
#   data    = dat_text,
#   mapping = aes(x=10,y=90, label=label)
# )

#Figure 3: How to read the phase plot####
loop <- 4
result <- readRDS(paste("Ricardo/facet/results_loop_", loop,".rds", sep="")) #facet


cmda_1 <- cmda_1Loop[loop]#80 #90


village1 <- sapply(result, function(x){
  (x[,2]<1)*1+(x[,2]>=1)*0
  
})
village2 <- sapply(result, function(x){
  (x[,4]<1)*2+(x[,4]>=1)*0
  
})

#putting into matrix
#change 3
v1m <- matrix(as.numeric(village1),nrow=100,ncol=101, byrow=TRUE)
v2m <- matrix(as.numeric(village2),nrow=100,ncol=101, byrow=TRUE)

#add 5 rows to v1m, 10 rows to v2m
v1m_tmp <- rbind(matrix(0,nrow = 5,ncol=101),v1m[-(96:100),])
v2m_tmp <- rbind(matrix(0,nrow = 10,ncol=101),v2m[-(91:100),])

#v12m <- matrix(as.numeric(village1),nrow=100,ncol=101, byrow=TRUE)+matrix(as.numeric(village2),nrow=100,ncol=101, byrow=TRUE)
v12m <- v1m_tmp+v2m_tmp

toPlot <- melt(t(v12m))
toPlot$value <- factor(toPlot$value, levels=c(0,1,2,3), labels=c("Neither patch","Patch 1", "Patch 2", "Both patches"))
toPlot$P1Coverage <- cmda_1Loop[loop]
toPlot$P2Incidence <- incidenceLoop[loop]

annotag <- data.frame(
  x=  c(50,85,85,25,50),
  y=  c(22,30,70,70,8),
  lab=c("a","b","c","d","e")
)

png(paste('Ricardo/facet/',"Fig3_Explanatory","_",gsub("\\:","",Sys.time()),'.png',sep=''),height= 1500, width=1500, units= "px", res=300)
print(
ggplot(data=toPlot, aes(x=X1, y=X2))+
  geom_tile(aes(fill=(value)))+
  #ggtitle(paste0("No. of patch with less than 1 case/1000 at 1 year after MDA"))+
  xlab("% of connectedness")+ylab("% of MDA coverage in patch 2")+
  theme(legend.position = "bottom")+ colScale+
  geom_hline(yintercept=14, color='red', size=1.3)+
  geom_label(data=annotag,aes(x=x,y=y,label=lab),label.r = unit(0.08, "lines"))
  # geom_text(data=annotag,aes(x=x,y=y,label=lab))
)
dev.off()

#Figure 4: Assembly effect over increasing MDA coverage in patch 1, Pannel plot####
#setwd("C:/Users/andro/OneDrive - Nexus365/MORU/Projects/TCE_MDA effect/MDA_eff_hm/")

#for the same incidence/disease intensities####


cmda_1Loop <- seq(10, by=10, to=90) 
pos_assembly <- NA
neg_assembly <- NA
for(i in 1:length(cmda_1Loop)){
  result <- readRDS(paste0("Ricardo/results_homo_cov_start0_seas/results_loop_",i,".rds"))
  
  village1 <- sapply(result, function(x){
    (x[,2]<1)*1+(x[,2]>=1)*0
    
  })
  village2 <- sapply(result, function(x){
    (x[,4]<1)*2+(x[,4]>=1)*0
    
  })
  
  #putting into matrix
  #change 3
  v1m <- matrix(as.numeric(village1),nrow=100,ncol=101, byrow=TRUE)
  v2m <- matrix(as.numeric(village2),nrow=100,ncol=101, byrow=TRUE)
  v12m <- matrix(as.numeric(village1),nrow=100,ncol=101, byrow=TRUE)+matrix(as.numeric(village2),nrow=100,ncol=101, byrow=TRUE)
  #positive assembly effect
  pos_denominator <- nrow(v12m)*ncol(v12m)
  if(length(which(v12m[,1]==2))==0 & length(which(v12m[,1]==3))==0){
    pos_u_bound <- nrow(v12m)
  } else{
    pos_u_bound <- min(which(v12m[,1]%in%c(2,3)))-1
  }
  if(length(which(v12m[,ncol(v12m)]==3))==0){
    pos_l_bound <- 1
  } else{
    pos_l_bound <- min(which(v12m[,ncol(v12m)]==3))
  }
  
  pos_numerator <- sum(v12m[pos_l_bound:pos_u_bound,]==3)
  pos_assembly[i] <- pos_numerator/pos_denominator
  
  if(length(which(v12m[,ncol(v12m)]==3))!=0){
    if(pos_u_bound < min(which(v12m[,ncol(v12m)]==3))){
      pos_assembly[i] <- 0
      # pos_l_bound <- pos_u_bound
    } 
  }
  
  #negative assembly effect
  neg_denominator <- nrow(v12m)*ncol(v12m)
  if(length(which(v12m[,1]==2))==0 & length(which(v12m[,1]==3))==0){
    neg_l_bound <- 1
  } else{
    neg_l_bound <- min(c(which(v12m[,1]==3),(which(v12m[,1]==2))))
  }
  if(length(which(v12m[,ncol(v12m)]==2))==0){
    neg_u_bound <- nrow(v12m)
  } else{
    neg_u_bound <- min(which(v12m[,ncol(v12m)]==2))-1  
  }
  
  neg_numerator <- sum(v12m[neg_l_bound:neg_u_bound,]%in%c(0,1))
  neg_assembly[i] <- neg_numerator/neg_denominator
}

#plot the results for same incidence####
png(paste('Ricardo/facet/Fig4_assemblyEffects_',gsub("\\:","",Sys.time()),'.png',sep=''),height= 1600, width=2800, units= "px", res=300)
par(mfrow=c(1,2))
plot(cmda_1Loop, pos_assembly-neg_assembly, type = 'l', main = "A. Assembly effects between two patches \nwith identical disease incidence", xlab = "MDA coverage in the other patch", ylab = "Assembly effect")
abline(h=0)
abline(v=78, lty=2)
legend(10,-.015,lty=2,legend="Baseline intervention threshold")

#hotspot vs non-hotspot####
cmda_1Loop <- seq(10, by=10, to=90) 
pos_assembly <- NA
for(i in 1:length(cmda_1Loop)){
  result <- readRDS(paste0("Ricardo/results_village2lowAPI_reverseAxis/results_loop_",i,".rds"))
  
  
  #reciprocated
  village1 <- sapply(result, function(x){
    (x[,2]<1)*1+(x[,2]>=1)*0
    
  })
  village2 <- sapply(result, function(x){
    (x[,4]<1)*2+(x[,4]>=1)*0
    
  })
  
  #putting into matrix
  #change 3
  v1m <- matrix(as.numeric(village1),nrow=100,ncol=101, byrow=TRUE)
  v2m <- matrix(as.numeric(village2),nrow=100,ncol=101, byrow=TRUE)
  v12m <- matrix(as.numeric(village1),nrow=100,ncol=101, byrow=TRUE)+matrix(as.numeric(village2),nrow=100,ncol=101, byrow=TRUE)
  #positive assembly effect
  pos_denominator <- nrow(v12m)*ncol(v12m)
  if(length(which(v12m[,1]==3))==0){
    pos_u_bound <- nrow(v12m)
  } else{
    pos_u_bound <- min(which(v12m[,1]==3))-1
  }
  if(length(which(v12m[,ncol(v12m)]==3))==0){
    pos_l_bound <- 1
  } else{
    pos_l_bound <- min(c(which(v12m[,ncol(v12m)]==3),which(v12m[,ncol(v12m)]==2)))
  }
  
  pos_numerator <- sum(v12m[pos_l_bound:pos_u_bound,]%in%c(2,3))
  pos_assembly[i] <- pos_numerator/pos_denominator
  
}

neg_assembly <- NA
for(i in 1:length(cmda_1Loop)){
  result <- readRDS(paste0("Ricardo/results_homo_cov_start0_seas_village2lowAPI/results_loop_",i,".rds"))
  
  village1 <- sapply(result, function(x){
    (x[,2]<1)*1+(x[,2]>=1)*0
    
  })
  village2 <- sapply(result, function(x){
    (x[,4]<1)*2+(x[,4]>=1)*0
    
  })
  
  #putting into matrix
  #change 3
  v1m <- matrix(as.numeric(village1),nrow=100,ncol=101, byrow=TRUE)
  v2m <- matrix(as.numeric(village2),nrow=100,ncol=101, byrow=TRUE)
  v12m <- matrix(as.numeric(village1),nrow=100,ncol=101, byrow=TRUE)+matrix(as.numeric(village2),nrow=100,ncol=101, byrow=TRUE)
  
  
  #negative assembly effect
  neg_denominator <- nrow(v12m)*ncol(v12m)
  if(length(which(v12m[,1]==2))==0 & length(which(v12m[,1]==3))==0){
    neg_l_bound <- 1
  } else{
    neg_l_bound <- min(c(which(v12m[,1]==3),(which(v12m[,1]==2))))
  }
  if(length(which(v12m[,ncol(v12m)]==2))==0 & length(which(v12m[,ncol(v12m)]==3))==0){
    neg_u_bound <- nrow(v12m)
  } else{
    neg_u_bound <- min(c(which(v12m[,ncol(v12m)]==2), which(v12m[,ncol(v12m)]==3)))-1  
  }
  
  neg_numerator <- sum(v12m[neg_l_bound:neg_u_bound,]%in%c(0,1))
  neg_assembly[i] <- neg_numerator/neg_denominator
}

#plot the results for hotspots####
plot(cmda_1Loop, pos_assembly, ylim = c(min(-neg_assembly),max(pos_assembly)), type = 'l',col="red", main = "B. Assembly effects between\n a hotspot and a non-hotspot", xlab = "MDA coverage in the other patch", ylab = "Assembly effect")
lines(cmda_1Loop, -neg_assembly, col='blue')
abline(h=0)
legend(55,-.22, legend = c("Hotspot","Non-hotspot"), col=c("red","blue"), lty = 1, cex=.7)

dev.off()