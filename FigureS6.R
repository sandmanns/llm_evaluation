library(png)
library(openxlsx)
dir<-"C:/Paper/Figures/"

input<-read.xlsx(paste0(dir,"SupplementaryDataS1.xlsx"),sheet=1)

input<-input[,c(3,1,2,4,
                11,24,
                14,
                17,27,
                8,21,
                31,40,
                33,38,
                36)]
names(input)[5:16]<-c("DiagnosisDeepR","TreatmentDeepR",
                      "DiagnosisDeepV",
                      "DiagnosisGemini","TreatmentGemini",
                      "DiagnosisGPT4o","TreatmentGPT4o",
                      "Diagnosis3","Treatment3",
                      "Diagnosis4","Treatment4",
                      "DiagnosisG")


#############
##Diagnosis##
#############

##Bubbleplot GPT-4o vs GPT-4, 3.5
d_both<-table(input[,c("Treatment4","TreatmentGPT4o")])
d_both<-0.2+d_both/max(d_both)
d_both[d_both==0.2]<-0
png(paste0(dir,"FigureS6_Treatment_GPT4o_GPT4.png"),width=700,height=700)
par(mar=c(3,3,1,1),mgp=c(4.5,1.5,0))
plot(NULL,xlim=c(1,5),ylim=c(1,5),xlab="",ylab="",cex.lab=3,
     cex.main=3,cex.axis = 2.5,las=1)
abline(a=0,b=1,lty=2,col="grey40",lwd=3)
for(i in as.numeric(rownames(d_both))){
  for(j in as.numeric(colnames(d_both))){
    temp<-seq(d_both[which(as.numeric(rownames(d_both))==i),
                     which(as.numeric(colnames(d_both))==j)],0,length.out=20)
    for(k in 1:length(temp)){
      points(i,j,cex=10*temp[k],col=rgb(0.12,0.56,1,0.2),pch=16)
    }
  }
}
#wilcox.test(input$TreatmentGPT4o,input$Treatment4,
#            paired = T,alternative = "greater")$p.value
text(x=4.2,y=1.3,expression(p[unadj]~"=0.0110"),xpd=F,font=2,cex=2.5)
dev.off()

d_both<-table(input[,c("Treatment3","TreatmentGPT4o")])
d_both<-0.2+d_both/max(d_both)
d_both[d_both==0.2]<-0
png(paste0(dir,"FigureS6_Treatment_GPT4o_GPT3.png"),width=700,height=700)
par(mar=c(3,3,1,1),mgp=c(4.5,1.5,0))
plot(NULL,xlim=c(1,5),ylim=c(1,5),xlab="",ylab="",cex.lab=3,
     cex.main=3,cex.axis = 2.5,las=1)
abline(a=0,b=1,lty=2,col="grey40",lwd=3)
for(i in as.numeric(rownames(d_both))){
  for(j in as.numeric(colnames(d_both))){
    temp<-seq(d_both[which(as.numeric(rownames(d_both))==i),
                     which(as.numeric(colnames(d_both))==j)],0,length.out=20)
    for(k in 1:length(temp)){
      points(i,j,cex=10*temp[k],col=rgb(0.12,0.56,1,0.2),pch=16)
    }
  }
}
#wilcox.test(input$TreatmentGPT4o,input$Treatment3,
#            paired = T,alternative = "greater")$p.value
text(x=4.2,y=1.3,expression(p[unadj]~"=0.0001"),xpd=F,font=2,cex=2.5)
dev.off()



##Bubbleplot DeepSeek-R1 vs GPT-4, 3.5
d_both<-table(input[,c("Treatment4","TreatmentDeepR")])
d_both<-0.2+d_both/max(d_both)
d_both[d_both==0.2]<-0
png(paste0(dir,"FigureS6_Treatment_DeepSeekR1_GPT4.png"),width=700,height=700)
par(mar=c(3,3,1,1),mgp=c(4.5,1.5,0))
plot(NULL,xlim=c(1,5),ylim=c(1,5),xlab="",ylab="",cex.lab=3,
     cex.main=3,cex.axis = 2.5,las=1)
abline(a=0,b=1,lty=2,col="grey40",lwd=3)
for(i in as.numeric(rownames(d_both))){
  for(j in as.numeric(colnames(d_both))){
    temp<-seq(d_both[which(as.numeric(rownames(d_both))==i),
                     which(as.numeric(colnames(d_both))==j)],0,length.out=20)
    for(k in 1:length(temp)){
      points(i,j,cex=10*temp[k],col=rgb(0.12,0.56,1,0.2),pch=16)
    }
  }
}
#wilcox.test(input$TreatmentDeepR,input$Treatment4,
#            paired = T,alternative = "greater")$p.value
text(x=4.2,y=1.3,expression(p[unadj]~"=0.1346"),xpd=F,font=2,cex=2.5)
dev.off()

d_both<-table(input[,c("Treatment3","TreatmentDeepR")])
d_both<-0.2+d_both/max(d_both)
d_both[d_both==0.2]<-0
png(paste0(dir,"FigureS6_Treatment_DeepSeekR1_GPT3.png"),width=700,height=700)
par(mar=c(3,3,1,1),mgp=c(4.5,1.5,0))
plot(NULL,xlim=c(1,5),ylim=c(1,5),xlab="",ylab="",cex.lab=3,
     cex.main=3,cex.axis = 2.5,las=1)
abline(a=0,b=1,lty=2,col="grey40",lwd=3)
for(i in as.numeric(rownames(d_both))){
  for(j in as.numeric(colnames(d_both))){
    temp<-seq(d_both[which(as.numeric(rownames(d_both))==i),
                     which(as.numeric(colnames(d_both))==j)],0,length.out=20)
    for(k in 1:length(temp)){
      points(i,j,cex=10*temp[k],col=rgb(0.12,0.56,1,0.2),pch=16)
    }
  }
}
#wilcox.test(input$TreatmentDeepR,input$Treatment3,
#            paired = T,alternative = "greater")$p.value
text(x=4.2,y=1.3,expression(p[unadj]~"=0.0041"),xpd=F,font=2,cex=2.5)
dev.off()



##Bubbleplot Gem2FTE vs GPT-4, 3.5
d_both<-table(input[,c("Treatment4","TreatmentGemini")])
d_both<-0.2+d_both/max(d_both)
d_both[d_both==0.2]<-0
png(paste0(dir,"FigureS6_Treatment_Gem2FTE_GPT4.png"),width=700,height=700)
par(mar=c(3,3,1,1),mgp=c(4.5,1.5,0))
plot(NULL,xlim=c(1,5),ylim=c(1,5),xlab="",ylab="",cex.lab=3,
     cex.main=3,cex.axis = 2.5,las=1)
abline(a=0,b=1,lty=2,col="grey40",lwd=3)
for(i in as.numeric(rownames(d_both))){
  for(j in as.numeric(colnames(d_both))){
    temp<-seq(d_both[which(as.numeric(rownames(d_both))==i),
                     which(as.numeric(colnames(d_both))==j)],0,length.out=20)
    for(k in 1:length(temp)){
      points(i,j,cex=10*temp[k],col=rgb(0.12,0.56,1,0.2),pch=16)
    }
  }
}
#wilcox.test(input$TreatmentGemini,input$Treatment4,
#            paired = T,alternative = "greater")$p.value
text(x=4.2,y=1.3,expression(p[unadj]~"=0.7959"),xpd=F,font=2,cex=2.5)
dev.off()

d_both<-table(input[,c("Treatment3","TreatmentGemini")])
d_both<-0.2+d_both/max(d_both)
d_both[d_both==0.2]<-0
png(paste0(dir,"FigureS6_Treatment_Gem2FTE_GPT3.png"),width=700,height=700)
par(mar=c(3,3,1,1),mgp=c(4.5,1.5,0))
plot(NULL,xlim=c(1,5),ylim=c(1,5),xlab="",ylab="",cex.lab=3,
     cex.main=3,cex.axis = 2.5,las=1)
abline(a=0,b=1,lty=2,col="grey40",lwd=3)
for(i in as.numeric(rownames(d_both))){
  for(j in as.numeric(colnames(d_both))){
    temp<-seq(d_both[which(as.numeric(rownames(d_both))==i),
                     which(as.numeric(colnames(d_both))==j)],0,length.out=20)
    for(k in 1:length(temp)){
      points(i,j,cex=10*temp[k],col=rgb(0.12,0.56,1,0.2),pch=16)
    }
  }
}
wilcox.test(input$TreatmentGemini,input$Treatment3,
            paired = T,alternative = "greater")$p.value
text(x=4.2,y=1.3,expression(p[unadj]~"=0.1923"),xpd=F,font=2,cex=2.5)
dev.off()





###############
##Combine all##
###############

plot1<-readPNG(paste0(dir,"FigureS6_Treatment_GPT4o_GPT4.png"))
plot2<-readPNG(paste0(dir,"FigureS6_Treatment_GPT4o_GPT3.png"))

plot4<-readPNG(paste0(dir,"FigureS6_Treatment_DeepSeekR1_GPT4.png"))
plot5<-readPNG(paste0(dir,"FigureS6_Treatment_DeepSeekR1_GPT3.png"))

plot10<-readPNG(paste0(dir,"FigureS6_Treatment_Gem2FTE_GPT4.png"))
plot11<-readPNG(paste0(dir,"FigureS6_Treatment_Gem2FTE_GPT3.png"))


png(paste0(dir,"FigureS6.png"),width = 1500,height = 2200)
layout(matrix(c(rep(c(1,rep(2,7),rep(3,7)),7),
                rep(c(4,rep(5,7),rep(6,7)),7),
                rep(c(7,rep(8,7),rep(9,7)),7),
                c(10,rep(11,7),rep(12,7))),nrow = 22,byrow = T))
par(mar=c(0,0,2,0))
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
text(0.5,0.5,"GPT-4o",font=2,cex=6,srt=90)

plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
rasterImage(plot1,0,0,1,1)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
rasterImage(plot2,0,0,1,1)

plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
text(0.5,0.5,"DeepSeek-R1",font=2,cex=6,srt=90)

plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
rasterImage(plot4,0,0,1,1)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
rasterImage(plot5,0,0,1,1)

plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
text(0.5,0.5,"Gem2FTE",font=2,cex=6,srt=90)

plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
rasterImage(plot10,0,0,1,1)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
rasterImage(plot11,0,0,1,1)

plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
text(0.5,0.5,"GPT-4",font=2,cex=6)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
text(0.5,0.5,"GPT-3.5",font=2,cex=6)
dev.off()






