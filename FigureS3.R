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

##Bubbleplot GPT-4o vs GPT-4, 3.5, Google
d_both<-table(input[,c("Diagnosis4","DiagnosisGPT4o")])
d_both<-0.2+d_both/max(d_both)
d_both[d_both==0.2]<-0
png(paste0(dir,"FigureS3_Diagnosis_GPT4o_GPT4.png"),width=700,height=700)
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
#wilcox.test(input$DiagnosisGPT4o,input$Diagnosis4,
#            paired = T,alternative = "greater")$p.value
text(x=4.2,y=1.3,expression(p[unadj]~"=2.06"%.%10^-10),xpd=F,font=2,cex=2.5)
dev.off()

d_both<-table(input[,c("Diagnosis3","DiagnosisGPT4o")])
d_both<-0.2+d_both/max(d_both)
d_both[d_both==0.2]<-0
png(paste0(dir,"FigureS3_Diagnosis_GPT4o_GPT3.png"),width=700,height=700)
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
#wilcox.test(input$DiagnosisGPT4o,input$Diagnosis3,
#            paired = T,alternative = "greater")$p.value
text(x=4.2,y=1.3,expression(p[unadj]~"=2.49"%.%10^-12),xpd=F,font=2,cex=2.5)
dev.off()

d_both<-table(input[,c("DiagnosisG","DiagnosisGPT4o")])
d_both<-0.2+d_both/max(d_both)
d_both[d_both==0.2]<-0
png(paste0(dir,"FigureS3_Diagnosis_GPT4o_Google.png"),width=700,height=700)
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
#wilcox.test(input$DiagnosisGPT4o,input$DiagnosisG,
#            paired = T,alternative = "greater")$p.value
text(x=4.2,y=1.3,expression(p[unadj]~"=5.69"%.%10^-14),xpd=F,font=2,cex=2.5)
dev.off()



##Bubbleplot DeepSeek-R1 vs GPT-4, 3.5, Google
d_both<-table(input[,c("Diagnosis4","DiagnosisDeepR")])
d_both<-0.2+d_both/max(d_both)
d_both[d_both==0.2]<-0
png(paste0(dir,"FigureS3_Diagnosis_DeepSeekR1_GPT4.png"),width=700,height=700)
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
#wilcox.test(input$DiagnosisDeepR,input$Diagnosis4,
#            paired = T,alternative = "greater")$p.value
text(x=4.2,y=1.3,expression(p[unadj]~"=1.18"%.%10^-9),xpd=F,font=2,cex=2.5)
dev.off()

d_both<-table(input[,c("Diagnosis3","DiagnosisDeepR")])
d_both<-0.2+d_both/max(d_both)
d_both[d_both==0.2]<-0
png(paste0(dir,"FigureS3_Diagnosis_DeepSeekR1_GPT3.png"),width=700,height=700)
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
#wilcox.test(input$DiagnosisDeepR,input$Diagnosis3,
#            paired = T,alternative = "greater")$p.value
text(x=4.2,y=1.3,expression(p[unadj]~"=1.11"%.%10^-11),xpd=F,font=2,cex=2.5)
dev.off()

d_both<-table(input[,c("DiagnosisG","DiagnosisDeepR")])
d_both<-0.2+d_both/max(d_both)
d_both[d_both==0.2]<-0
png(paste0(dir,"FigureS3_Diagnosis_DeepSeekR1_Google.png"),width=700,height=700)
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
#wilcox.test(input$DiagnosisDeepR,input$DiagnosisG,
#            paired = T,alternative = "greater")$p.value
text(x=4.2,y=1.3,expression(p[unadj]~"=7.08"%.%10^-15),xpd=F,font=2,cex=2.5)
dev.off()



##Bubbleplot DeepSeek-V3 vs GPT-4, 3.5, Google
d_both<-table(input[,c("Diagnosis4","DiagnosisDeepV")])
d_both<-0.2+d_both/max(d_both)
d_both[d_both==0.2]<-0
png(paste0(dir,"FigureS3_Diagnosis_DeepSeekV3_GPT4.png"),width=700,height=700)
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
wilcox.test(input$DiagnosisDeepV,input$Diagnosis4,
            paired = T,alternative = "greater")$p.value
text(x=4.2,y=1.3,expression(p[unadj]~"=5.77"%.%10^-10),xpd=F,font=2,cex=2.5)
dev.off()

d_both<-table(input[,c("Diagnosis3","DiagnosisDeepV")])
d_both<-0.2+d_both/max(d_both)
d_both[d_both==0.2]<-0
png(paste0(dir,"FigureS3_Diagnosis_DeepSeekV3_GPT3.png"),width=700,height=700)
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
#wilcox.test(input$DiagnosisDeepV,input$Diagnosis3,
#            paired = T,alternative = "greater")$p.value
text(x=4.2,y=1.3,expression(p[unadj]~"=1.05"%.%10^-11),xpd=F,font=2,cex=2.5)
dev.off()

d_both<-table(input[,c("DiagnosisG","DiagnosisDeepV")])
d_both<-0.2+d_both/max(d_both)
d_both[d_both==0.2]<-0
png(paste0(dir,"FigureS3_Diagnosis_DeepSeekV3_Google.png"),width=700,height=700)
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
#wilcox.test(input$DiagnosisDeepV,input$DiagnosisG,
#            paired = T,alternative = "greater")$p.value
text(x=4.2,y=1.3,expression(p[unadj]~"=4.44"%.%10^-14),xpd=F,font=2,cex=2.5)
dev.off()



##Bubbleplot DeepSeek-V3 vs GPT-4, 3.5, Google
d_both<-table(input[,c("Diagnosis4","DiagnosisGemini")])
d_both<-0.2+d_both/max(d_both)
d_both[d_both==0.2]<-0
png(paste0(dir,"FigureS3_Diagnosis_Gem2FTE_GPT4.png"),width=700,height=700)
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
#wilcox.test(input$DiagnosisGemini,input$Diagnosis4,
#            paired = T,alternative = "greater")$p.value
text(x=4.2,y=1.3,expression(p[unadj]~"=4.04"%.%10^-3),xpd=F,font=2,cex=2.5)
dev.off()

d_both<-table(input[,c("Diagnosis3","DiagnosisGemini")])
d_both<-0.2+d_both/max(d_both)
d_both[d_both==0.2]<-0
png(paste0(dir,"FigureS3_Diagnosis_Gem2FTE_GPT3.png"),width=700,height=700)
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
#wilcox.test(input$DiagnosisGemini,input$Diagnosis3,
#             paired = T,alternative = "greater")$p.value
text(x=4.2,y=1.3,expression(p[unadj]~"=2.86"%.%10^-6),xpd=F,font=2,cex=2.5)
dev.off()

d_both<-table(input[,c("DiagnosisG","DiagnosisGemini")])
d_both<-0.2+d_both/max(d_both)
d_both[d_both==0.2]<-0
png(paste0(dir,"FigureS3_Diagnosis_Gem2FTE_Google.png"),width=700,height=700)
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
#wilcox.test(input$DiagnosisGemini,input$DiagnosisG,
#            paired = T,alternative = "greater")$p.value
text(x=4.2,y=1.3,expression(p[unadj]~"=2.06"%.%10^-8),xpd=F,font=2,cex=2.5)
dev.off()




###############
##Combine all##
###############

plot1<-readPNG(paste0(dir,"FigureS3_Diagnosis_GPT4o_GPT4.png"))
plot2<-readPNG(paste0(dir,"FigureS3_Diagnosis_GPT4o_GPT3.png"))
plot3<-readPNG(paste0(dir,"FigureS3_Diagnosis_GPT4o_Google.png"))

plot4<-readPNG(paste0(dir,"FigureS3_Diagnosis_DeepSeekR1_GPT4.png"))
plot5<-readPNG(paste0(dir,"FigureS3_Diagnosis_DeepSeekR1_GPT3.png"))
plot6<-readPNG(paste0(dir,"FigureS3_Diagnosis_DeepSeekR1_Google.png"))

plot7<-readPNG(paste0(dir,"FigureS3_Diagnosis_DeepSeekV3_GPT4.png"))
plot8<-readPNG(paste0(dir,"FigureS3_Diagnosis_DeepSeekV3_GPT3.png"))
plot9<-readPNG(paste0(dir,"FigureS3_Diagnosis_DeepSeekV3_Google.png"))

plot10<-readPNG(paste0(dir,"FigureS3_Diagnosis_Gem2FTE_GPT4.png"))
plot11<-readPNG(paste0(dir,"FigureS3_Diagnosis_Gem2FTE_GPT3.png"))
plot12<-readPNG(paste0(dir,"FigureS3_Diagnosis_Gem2FTE_Google.png"))


png(paste0(dir,"FigureS3.png"),width = 2200,height = 2900)
layout(matrix(c(rep(c(1,rep(2,7),rep(3,7),rep(4,7)),7),
                rep(c(5,rep(6,7),rep(7,7),rep(8,7)),7),
                rep(c(9,rep(10,7),rep(11,7),rep(12,7)),7),
                rep(c(13,rep(14,7),rep(15,7),rep(16,7)),7),
                c(17,rep(18,7),rep(19,7),rep(20,7))),nrow = 29,byrow = T))
par(mar=c(0,0,2,0))
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
text(0.5,0.5,"GPT-4o",font=2,cex=6,srt=90)

plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
rasterImage(plot1,0,0,1,1)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
rasterImage(plot2,0,0,1,1)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
rasterImage(plot3,0,0,1,1)

plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
text(0.5,0.5,"DeepSeek-R1",font=2,cex=6,srt=90)

plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
rasterImage(plot4,0,0,1,1)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
rasterImage(plot5,0,0,1,1)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
rasterImage(plot6,0,0,1,1)

plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
text(0.5,0.5,"DeepSeek-V3",font=2,cex=6,srt=90)

plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
rasterImage(plot7,0,0,1,1)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
rasterImage(plot8,0,0,1,1)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
rasterImage(plot9,0,0,1,1)

plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
text(0.5,0.5,"Gem2FTE",font=2,cex=6,srt=90)

plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
rasterImage(plot10,0,0,1,1)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
rasterImage(plot11,0,0,1,1)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
rasterImage(plot12,0,0,1,1)

plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
text(0.5,0.5,"GPT-4",font=2,cex=6)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
text(0.5,0.5,"GPT-3.5",font=2,cex=6)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
text(0.5,0.5,"Google",font=2,cex=6)
dev.off()






