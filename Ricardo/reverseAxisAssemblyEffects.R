#extract the positive and negative assembly effects (areas) from the scenario analyses
#& make 2 plots
#x: MDA coverage in village1: from 10 to 90
#y: assembly effect (fig1: pos and neg separate, fig2: overall)
#20190527
mac <- 1
if(mac){
  path <- "~/OneDrive - Nexus365/MORU/Projects/TCE_MDA effect/MDA_eff_hm/"
} else { path <- "D:/OneDrive - Nexus365/MORU/Projects/TCE_MDA effect/MDA_eff_hm/"}

setwd(path)

cmda_1Loop <- seq(10, by=10, to=90) 
pos_assembly <- NA
# neg_assembly <- NA
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
  # v1m <- matrix(as.numeric(village1),nrow=81,ncol=101, byrow=TRUE)
  # v2m <- matrix(as.numeric(village2),nrow=81,ncol=101, byrow=TRUE)
  # v12m <- matrix(as.numeric(village1),nrow=81,ncol=101, byrow=TRUE)+matrix(as.numeric(village2),nrow=81,ncol=101, byrow=TRUE)
  #positive assembly effect####
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
  
  #negative assembly effect ####
  # neg_denominator <- nrow(v12m)*ncol(v12m)
  # if(length(which(v12m[,1]==2))==0){
  #   neg_l_bound <- 1
  # } else{
  #   neg_l_bound <- min(which(v12m[,1]==2))
  # }
  # if(length(which(v12m[,ncol(v12m)]==2))==0){
  #   neg_u_bound <- nrow(v12m)
  # } else{
  #   neg_u_bound <- min(which(v12m[,ncol(v12m)]==2))-1  
  # }
  # 
  # neg_numerator <- sum(v12m[neg_l_bound:neg_u_bound,]==0)
  # neg_assembly[i] <- neg_numerator/neg_denominator
}

#plot the results
#fig 1
png(paste('Ricardo/results_village2lowAPI_reverseAxis/assemblyEffects/assemblyEffects_v1_',gsub("\\:","",Sys.time()),'.png',sep=''),height= 1600, width=1800, units= "px", res=300)
plot(cmda_1Loop, pos_assembly, ylim = c(0,max(pos_assembly)), type = 'l',col="blue", main = "Total area of assembly effects in village 1", xlab = "MDA coverage in village 2", ylab = "Assembly effect")
# lines(cmda_1Loop, neg_assembly, col='red')
# legend(50,.7, legend = c("Positive","Negative"), col=c("blue","red"), lty = 1)
dev.off()
#fig 2
# png(paste('results_homo_cov_start0_seas/newPlot_OneYrInc/netAssemblyEffects_',gsub("\\:","",Sys.time()),'.png',sep=''),height= 1600, width=1800, units= "px", res=300)
# plot(cmda_1Loop, pos_assembly-neg_assembly, type = 'l', main = "Net assembly effect", xlab = "MDA coverage in village 1", ylab = "Assembly effect")
# abline(h=0)
# dev.off()

