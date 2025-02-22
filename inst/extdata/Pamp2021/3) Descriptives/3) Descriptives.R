# remove the old stuff
rm(list=ls())
# load the data

# load the statnet packages

library("statnet")
library("parallel")
library("ggplot2")
library("RColorBrewer")

## Define a function

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('..')

load("1) Data/Data.RData")
country_list[,1]<-as.character(country_list[,1])


for (i in 1:69){
  rownames(amk[[i]])<-country_list[,1]
  colnames(amk[[i]])<-country_list[,1]
}

rS<-c()
for (i in 1:69){

  rS<-cbind(rS,rowSums(amk[[i]]))
}

#rS<-rS[which(rowSums(EX)>0),]

# Most important Exporters
names<-rownames(rS)
names[order(rowSums(rS),decreasing = T)][1:6]

rS<-rowSums(rS)

rS<-rS/sum(rS)
table<-rS[order(rS,decreasing = T)]

pdf("3) Descriptives/barplots_sender_receiver.pdf",width = 12,height = 5)
par(mfrow=c(1,2))
par(mar=c(9,3,2,2))
barplot(table[1:35],axes = T,las = 2,ylim=c(0,0.35))


####

cS<-c()
for (i in 1:69){
  
  cS<-cbind(cS,colSums(amk[[i]]))
}

#cS<-cS[which(rowSums(EX)>0),]

# Most important Exporters
names<-rownames(cS)
names[order(rowSums(cS),decreasing = T)][1:6]

rS<-rowSums(cS)

rS<-rS/sum(rS)
table<-rS[order(rS,decreasing = T)]


barplot(table[1:35],axes = T,las = 2,ylim=c(0,0.35))


dev.off()

############ Now we need a plot with the development of the aggregated TIV
tiv_all<-c()
for ( i in 1:69){
  tiv_all<-c(tiv_all,sum(amk[[i]]))
  }



d <- data.frame(year=rep(1950:2018,each=1),TIV=c(tiv_all))

p1<-ggplot(d, aes(x=year,y=TIV)) +geom_area()
p1



density<-c()
for ( i in 1:69){
  net_bin<-amk[[i]]
  net_bin[net_bin>0]<-1
  density<-c(density,sum(net_bin)/(dim(net_bin)[1]*(dim(net_bin)[1]-1)))
}


d <- data.frame(year=rep(1950:2018,each=1),Density=c(density))

p2<-ggplot(d, aes(x=year,y=density)) +geom_line()
p2
pdf("3) Descriptives/aggregated_density.pdf",width = 12,height = 5)
multiplot(p1,p2,cols=2)
dev.off()



##########


outdeg<-matrix(0,nrow=224,ncol=69)
export<-matrix(0,nrow=224,ncol=69)
indeg<-matrix(0,nrow=224,ncol=69)
import<-matrix(0,nrow=224,ncol=69)
for (i in 1:69){
  select<-amk[[i]][1:224,1:224]
  select_bin<-select
  select_bin[select_bin>0]<-1
  
  outdeg[,i]<-rowSums(select_bin)
  indeg[,i]<-colSums(select_bin)
  
  
  import[,i]<-colSums(select)
  export[,i]<-rowSums(select)
  
}


out_line<-c(outdeg)
export_line<-c(export)

in_line<-c(indeg)
import_line<-c(import)

mean_Ex<-c()
q_0.5<-c()

for (i in 1:max(out_line)){
  m<-mean(export_line[out_line==i])
  q_0.5<-c(q_0.5,quantile(export_line[out_line==i],0.5))

  mean_Ex<-cbind(mean_Ex,m)
}

mean_Im<-c()
q_0.5_Im<-c()
for (i in 1:max(in_line)){
  m<-mean(import_line[in_line==i])
  q_0.5_Im<-c(q_0.5_Im,quantile(import_line[in_line==i],0.5))
  
  mean_Im<-cbind(mean_Im,m)
}


pdf("3) Descriptives/degree_dist.pdf",width = 12,height = 5)
par(mfrow=c(1,2))
plot(out_line,export_line,log="xy",xlab = "Logarithmic Outdegree",ylab="Export TIV value",main="Outdegree vs. TIV value, logarithmic scale",axis.cex=2,label.cex=2,main.cex=2,col="lightgray",ylim=c(0.01,15000),xlim=c(1,69))
points(1:max(out_line),mean_Ex,lwd=1.5,pch=1,type = "b")
points(1:max(out_line),q_0.5,lwd=1.5,pch=2,type = "b")

plot(in_line,import_line,log="xy",xlab = "Logarithmic Indegree",ylab="Import TIV value",main="Indegree vs. TIV value, logarithmic scale",axis.cex=2,label.cex=2,main.cex=2,col="lightgray",ylim=c(0.01,15000),xlim=c(1,69))
points(1:max(in_line),mean_Im,lwd=1.5,pch=1,type="b")
points(1:max(in_line),q_0.5_Im,lwd=1.5,pch=2,type="b")
dev.off()




for (i in 1:69){
  rownames(amk[[i]])<-country_list[,5]
  colnames(amk[[i]])<-country_list[,5]
}


pdf("3) Descriptives/net1950.pdf",width = 15,height = 15)

t=1950
net<-network(amk[[t-1949]])
net_bin<-amk[[t-1949]]
net_bin[net_bin>0]<-1
cex_nodes<-log(1+sqrt(rowSums(net_bin)))+0.8
plot(net,displayisolates=F,displaylabels=T,label.cex=0.8,vertex.col="gray",label.pos = 5,vertex.cex=cex_nodes)

dev.off()
pdf("3) Descriptives/net1980.pdf",width = 15,height = 15)

t=1980
net<-network(amk[[t-1949]])
net_bin<-amk[[t-1949]]
net_bin[net_bin>0]<-1
cex_nodes<-log(1+sqrt(rowSums(net_bin)))+0.8
plot(net,displayisolates=F,displaylabels=T,label.cex=0.8,vertex.col="gray",label.pos = 5,vertex.cex=cex_nodes)

dev.off()
pdf("3) Descriptives/net1990.pdf",width = 15,height = 15)

t=1990
net<-network(amk[[t-1949]])
net_bin<-amk[[t-1949]]
net_bin[net_bin>0]<-1
cex_nodes<-log(1+sqrt(rowSums(net_bin)))+0.8
plot(net,displayisolates=F,displaylabels=T,label.cex=0.8,vertex.col="gray",label.pos = 5,vertex.cex=cex_nodes)

dev.off()
pdf("3) Descriptives/net2017.pdf",width = 15,height = 15)

t=2017
net<-network(amk[[t-1949]])
net_bin<-amk[[t-1949]]
net_bin[net_bin>0]<-1
cex_nodes<-log(1+sqrt(rowSums(net_bin)))+0.8
plot(net,displayisolates=F,displaylabels=T,label.cex=0.8,vertex.col="gray",label.pos = 5,vertex.cex=cex_nodes)
dev.off()



# Showing the connectin among the degree and the average exports/imports

outdeg<-c()
indeg<-c()
mean_import<-c()
mean_export<-c()
outline<-c()
inline<-c()
for (t in 1:69){
  val<-amk[[t]][1:224,1:224]
  bin<-val
  bin[bin>0]<-1
  outdeg<-c(outdeg,rowSums(bin))
  indeg<-c(indeg,colSums(bin))
  outline<-c(outline,rowSums(val))
  inline<-c(inline,colSums(val))
  
  for (i in 1:224){
    for (j in 1:224){
      if (val[i,j]>0){
        mean_export<-cbind(mean_export,c(val[i,j],sum(bin[i,])))
        mean_import<-cbind(mean_import,c(val[i,j],sum(bin[,j])))
      }
    } 
  }

  
  
}





pdf("3) Descriptives/average_vs_degree.pdf",width = 12,height = 12)
par(mfrow=c(2,2))

plot(mean_export[1,]~mean_export[2,],log="y",xlab = "Outdegree(i)",ylab="Export TIV value",cex.axis=2,cex.lab=1.5,cex.main=2)
plot(mean_export[1,]~mean_import[2,],log="y",xlab = "Indegree(j)",ylab="Export TIV value",cex.axis=2,cex.lab=1.5,cex.main=2)

plot(outline/outdeg~outdeg,xlab = "Outdegree(i)",ylab="Average Export TIV value",cex.axis=2,cex.lab=1.5,cex.main=2)
plot(outline/outdeg~indeg,xlab = "Indegree(i)",ylab="Average Export TIV value",cex.axis=2,cex.lab=1.5,cex.main=2)


dev.off()








