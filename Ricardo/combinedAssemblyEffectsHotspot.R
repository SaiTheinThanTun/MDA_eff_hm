#definitive version for figure 4
#20200117
#Sai Thein Than Tun
library(grid)

mac <- 0
if(mac){
  path <- "~/OneDrive - Nexus365/MORU/Projects/TCE_MDA effect/MDA_eff_hm/"
} else { path <- "D:/OneDrive - Nexus365/MORU/Projects/TCE_MDA effect/MDA_eff_hm/"}

setwd(path)

cmda_1Loop <- seq(10, by=10, to=90) 
pos_assembly <- NA
posAssemblyEffect <- NA
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
  #positive assembly effect####
  pos_denominator <- nrow(v12m)*ncol(v12m)
  #ncol is connectedness from 0 to 100%, ie 101 columns
  #nrow is coverage from 0 to 99%, ie 100 rows
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

  posAssemblyEffect[i] <- (length(which(v12m %in% c(2,3))) - (100-79)*101)/101
}

neg_assembly <- NA
negAssemblyEffect <- NA
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
  
  
  #negative assembly effect ####
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
  
  negAssemblyEffect[i] <- (length(which(v12m %in% c(2,3))) - (100-5)*101)/101
}




#for the same incidence/disease intensities####
setwd(path)

cmda_1Loop <- seq(10, by=10, to=90) 
pos_assembly <- NA
neg_assembly <- NA
assemblyEffect <- NA
assemblyEffect2 <- NA
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
  # v1m <- matrix(as.numeric(village1),nrow=81,ncol=101, byrow=TRUE)
  # v2m <- matrix(as.numeric(village2),nrow=81,ncol=101, byrow=TRUE)
  # v12m <- matrix(as.numeric(village1),nrow=81,ncol=101, byrow=TRUE)+matrix(as.numeric(village2),nrow=81,ncol=101, byrow=TRUE)
  #positive assembly effect####
  #pos_denominator <- nrow(v12m)*ncol(v12m)
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
  pos_denominator <- 78*101
  pos_assembly[i] <- pos_numerator/pos_denominator
  
  if(length(which(v12m[,ncol(v12m)]==3))!=0){
  if(pos_u_bound < min(which(v12m[,ncol(v12m)]==3))){
    pos_assembly[i] <- 0
    # pos_l_bound <- pos_u_bound
  } 
  }
  
  #negative assembly effect ####
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
  
  assemblyEffect[i] <- (length(which(v12m %in% c(2,3))) - (100-79)*101)/101
  assemblyEffect2[i] <- (length(which(v12m %in% c(2,3))) - (100-79)*101)
  # assemblyEffect[i] <- length(which(v12m %in% c(2,3)))/101 - (100-79)
}

#plot the results
#fig 4A and 4B combined
png(paste('Ricardo/facet/Fig4_assemblyEffects_fused_',gsub("\\:","",Sys.time()),'.png',sep=''),height= 1600, width=1800, units= "px", res=300)

plot(cmda_1Loop, posAssemblyEffect, ylim = c(min(negAssemblyEffect),max(posAssemblyEffect)), type = 'p',pch=3,col="red", xlab = "MDA coverage in patch 1", ylab = "Assembly effect in patch 2")
points(cmda_1Loop, negAssemblyEffect, col='blue', pch=1)
points(cmda_1Loop, assemblyEffect, pch=16)
abline(h=0)
#65,-20
legend("bottomright", legend = c("Higher (hotspot)","Identical","Lower (non-hotspot)"), col=c("red", "black","blue"), pch = c(3,16,1), cex=.7, title="Incidence in patch 2\ncompared to patch 1", box.lty = 0)
grid.text("D",x = unit(0.718, "npc"), y = unit(0.595, "npc"))
grid.text("E",x = unit(0.815, "npc"), y = unit(0.68, "npc"))
grid.text("F",x = unit(0.915, "npc"), y = unit(0.7, "npc"))

grid.text("G",x = unit(0.718, "npc"), y = unit(0.41, "npc"))
grid.text("H",x = unit(0.815, "npc"), y = unit(0.47, "npc"))
grid.text("I",x = unit(0.915, "npc"), y = unit(0.525, "npc"))

dev.off()