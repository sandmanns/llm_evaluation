library(openxlsx)
library(vioplot)
library(png)
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




png(paste0(dir,"Violin_Diagnosis_2.png"),width=2100,height=700)
par(mar=c(4,7,1,1),mgp=c(4.5,1.5,0))
vioplot(x = as.matrix(input$DiagnosisGPT4o),
        xlim=c(0.5,7.5),
        col=c("royalblue3"),ylab="",
        drawRect=F,cex.lab=3,at=1,
        cex.main=3.5,cex.axis = 2,las=1,xaxt="n",ylim=c(0,5),main="")
points(jitter(rep(1,110),amount = 0.1),input$DiagnosisGPT4o[1:110],pch=16,cex=2)
points(jitter(rep(1,15),amount = 0.1),input$DiagnosisGPT4o[111:125],pch=16,cex=3,col="red")

vioplot(x = as.matrix(input$DiagnosisDeepR),add=T,
        col=c("deepskyblue3"),drawRect=F,at=2)
points(jitter(rep(2,110),amount = 0.1),input$DiagnosisDeepR[1:110],pch=16,cex=2)
points(jitter(rep(2,15),amount = 0.1),input$DiagnosisDeepR[111:125],pch=16,cex=3,col="red")

vioplot(x = as.matrix(input$DiagnosisDeepV),add=T,
        col=c("deepskyblue1"),drawRect=F,at=3)
points(jitter(rep(3,110),amount = 0.1),input$DiagnosisDeepV[1:110],pch=16,cex=2)
points(jitter(rep(3,15),amount = 0.1),input$DiagnosisGPT4o[111:125],pch=16,cex=3,col="red")

vioplot(x = as.matrix(input$DiagnosisGemini),add=T,
        col=c("lightblue1"),drawRect=F,at=4)
points(jitter(rep(4,110),amount = 0.1),input$DiagnosisGemini[1:110],pch=16,cex=2)
points(jitter(rep(4,15),amount = 0.1),input$DiagnosisGemini[111:125],pch=16,cex=3,col="red")

vioplot(x = as.matrix(input$Diagnosis4),add=T,
        col=c("seagreen4"),drawRect=F,at=5)
points(jitter(rep(5,110),amount = 0.1),input$Diagnosis4[1:110],pch=16,cex=2)

vioplot(x = as.matrix(input$Diagnosis3),add=T,
        col=c("seagreen2"),drawRect=F,at=6)
points(jitter(rep(6,110),amount = 0.1),input$Diagnosis3[1:110],pch=16,cex=2)

vioplot(x = as.matrix(input$DiagnosisG),add=T,
        col=c("olivedrab2"),drawRect=F,at=7)
points(jitter(rep(7,110),amount = 0.1),input$DiagnosisG[1:110],pch=16,cex=2)


points(x=c(1,2),y=c(2.5,2.5),type="l",lwd=3)
points(x=c(1,1),y=c(2.5,2.55),type="l",lwd=3)
points(x=c(2,2),y=c(2.5,2.55),type="l",lwd=3)
text(1.5,2.35,"n.s.",cex=2)

points(x=c(2,3),y=c(1.9,1.9),type="l",lwd=3)
points(x=c(2,2),y=c(1.9,1.95),type="l",lwd=3)
points(x=c(3,3),y=c(1.9,1.95),type="l",lwd=3)
text(2.5,1.75,"n.s.",cex=2)

points(x=c(2,4),y=c(1.2,1.2),type="l",lwd=3)
points(x=c(2,2),y=c(1.2,1.25),type="l",lwd=3)
points(x=c(4,4),y=c(1.2,1.25),type="l",lwd=3)
text(3,1.05,"***",font=2,cex=3)

points(x=c(1,4),y=c(0.5,0.5),type="l",lwd=3)
points(x=c(1,1),y=c(0.5,0.55),type="l",lwd=3)
points(x=c(4,4),y=c(0.5,0.55),type="l",lwd=3)
text(2.5,0.35,"***",font=2,cex=3)

axis(1,at=c(1,2,3,4,5,6,7),labels=c("GPT-4o","DeepSeek-R1","DeepSeek-V3","Gemi2FTE",
                                    "GPT-4","GPT-3.5","Google"),cex.axis=2.2,tick=F)
text(x=-0.04,y=2.4,paste0("Likert Scale"),xpd=T,font=2,cex=3,srt=90)
text(x=-0.04,y=5,"E)",xpd=T,font=2,cex=4.5)
dev.off()






##Bubbleplot DeepSeek-R1 vs GPT-4o
d_both<-table(input[,c("DiagnosisDeepR","DiagnosisGPT4o")])
d_both<-0.2+d_both/max(d_both)
d_both[d_both==0.2]<-0
png(paste0(dir,"Diagnosis_DeepSeekR1_GPT4o.png"),width=700,height=700)
par(mar=c(7,9,1,1),mgp=c(4.5,1.5,0))
plot(NULL,xlim=c(1,5),ylim=c(1,5),xlab="DeepSeek-R1",ylab="GPT-4o",cex.lab=3,
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
min(round(wilcox.test(input[,"DiagnosisGPT4o"],input[,"DiagnosisDeepR"],paired = T,alternative = "greater")$p.value*4,digits=4),1)
text(x=4.2,y=1.3,paste0("p=0.3085"),xpd=F,font=2,cex=3)
text(x=0.08,y=5,"A)",xpd=T,font=2,cex=4.5)
dev.off()


##Bubbleplot Gemini vs GPT-4o
d_both<-table(input[,c("DiagnosisGemini","DiagnosisGPT4o")])
d_both<-0.2+d_both/max(d_both)
d_both[d_both==0.2]<-0
png(paste0(dir,"Diagnosis_Gemini2FlashThinking_GPT4o.png"),width=700,height=700)
par(mar=c(7,9,1,1),mgp=c(4.5,1.5,0))
plot(NULL,xlim=c(1,5),ylim=c(1,5),xlab="Gemi2FTE",ylab="GPT-4o",cex.lab=3,
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
min(round(wilcox.test(input$DiagnosisGPT4o,input$DiagnosisGemini,paired = T,alternative = "greater")$p.value*4,digits=4),1)
text(x=4.2,y=1.3,expression(bold("p=7.89"%.%"10"^-"6")),xpd=T,font=2,cex=3)
text(x=0.08,y=5,"B)",xpd=T,font=2,cex=4.5)
dev.off()


##Bubbleplot Gemini vs DeepSeek-R1
d_both<-table(input[,c("DiagnosisGemini","DiagnosisDeepR")])
d_both<-0.2+d_both/max(d_both)
d_both[d_both==0.2]<-0
png(paste0(dir,"Diagnosis_Gemini2FlashThinking_DeepSeekR1.png"),width=700,height=700)
par(mar=c(7,9,1,1),mgp=c(4.5,1.5,0))
plot(NULL,xlim=c(1,5),ylim=c(1,5),xlab="Gemi2FTE",ylab="DeepSeek-R1",cex.lab=3,
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
min(round(wilcox.test(input$DiagnosisDeepR,input$DiagnosisGemini,paired = T,alternative = "greater")$p.value*4,digits=4),1)
text(x=4.2,y=1.3,expression(bold("p=5.73"%.%"10"^-"5")),xpd=F,font=2,cex=3)
text(x=0.08,y=5,"C)",xpd=T,font=2,cex=4.5)
dev.off()


##Bubbleplot DeepSeek-V3 vs DeepSeek-R1
d_both<-table(input[,c("DiagnosisDeepV","DiagnosisDeepR")])
d_both<-0.2+d_both/max(d_both)
d_both[d_both==0.2]<-0
png(paste0(dir,"Diagnosis_DeepSeekV3_DeepSeekR1.png"),width=700,height=700)
par(mar=c(7,9,1,1),mgp=c(4.5,1.5,0))
plot(NULL,xlim=c(1,5),ylim=c(1,5),xlab="DeepSeek-V3",ylab="DeepSeek-R1",cex.lab=3,
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
min(round(wilcox.test(input$DiagnosisDeepR,input$DiagnosisDeepV,paired = T,alternative = "greater")$p.value*4,digits=4),1)
text(x=4.2,y=1.3,paste0("p=1"),xpd=T,font=2,cex=3)
text(x=0.08,y=5,"D)",xpd=T,font=2,cex=4.5)
dev.off()






##Cumplot GPT-3.5
d1<-c(table(input[,"DiagnosisGPT4o"]))
d2<-c(table(input[,"DiagnosisDeepR"]))
d3<-c(table(input[,"DiagnosisDeepV"]))
d4<-c(table(input[,"DiagnosisGemini"]))

d5<-c(table(input[,"Diagnosis4"]))

png(paste0(dir,"Cumplot_Diagnosis.png"),width=700,height=700)
par(mar=c(7,9,1,1),mgp=c(4.5,1.5,0))
plot(NULL,xlim=c(1,5),ylim=c(0,105),yaxs="i",xlab="Likert Scale",ylab="Cumulative Frequency [%]",
     cex.lab=3,cex.main=3.5,cex.axis = 2.5,las=1,main="")
cum<-0
for(i in 1:(length(d1))){
  points(x=c(as.numeric(names(d1[i])),as.numeric(names(d1[i]))),
         y=c(cum,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=4,col="royalblue4")
  points(x=c(as.numeric(names(d1[i])),as.numeric(names(d1[i+1]))),
         y=c(cum+(d1[i]/sum(d1))*100,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=4,col="royalblue4")
  if(i==length(d1)){
    points(x=c(as.numeric(names(d1[i])),6),
           y=c(cum+(d1[i]/sum(d1))*100,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=4,col="royalblue4")
  }
  cum<-cum+(d1[i]/sum(d1))*100
}

cum<-0
for(i in 1:(length(d2))){
  points(x=c(as.numeric(names(d2[i])),as.numeric(names(d2[i]))),
         y=c(cum,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=4,col="deepskyblue3")
  points(x=c(as.numeric(names(d2[i])),as.numeric(names(d2[i+1]))),
         y=c(cum+(d2[i]/sum(d2))*100,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=4,col="deepskyblue3")
  if(i==length(d2)){
    points(x=c(as.numeric(names(d2[i])),6),
           y=c(cum+(d2[i]/sum(d2))*100,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=4,col="deepskyblue3")
  }
  cum<-cum+(d2[i]/sum(d2))*100
}

cum<-0
for(i in 1:(length(d3))){
  points(x=c(as.numeric(names(d3[i])),as.numeric(names(d3[i]))),
         y=c(cum,cum+(d3[i]/sum(d3))*100),pch=3,type="l",lwd=4,col="deepskyblue1")
  points(x=c(as.numeric(names(d3[i])),as.numeric(names(d3[i+1]))),
         y=c(cum+(d3[i]/sum(d3))*100,cum+(d3[i]/sum(d3))*100),pch=3,type="l",lwd=4,col="deepskyblue1")
  if(i==length(d3)){
    points(x=c(as.numeric(names(d3[i])),6),
           y=c(cum+(d3[i]/sum(d3))*100,cum+(d3[i]/sum(d3))*100),pch=3,type="l",lwd=4,col="deepskyblue1")
  }
  cum<-cum+(d3[i]/sum(d3))*100
}

cum<-0
for(i in 1:(length(d4))){
  points(x=c(as.numeric(names(d4[i])),as.numeric(names(d4[i]))),
         y=c(cum,cum+(d4[i]/sum(d4))*100),pch=3,type="l",lwd=4,col="lightskyblue1")
  points(x=c(as.numeric(names(d4[i])),as.numeric(names(d4[i+1]))),
         y=c(cum+(d4[i]/sum(d4))*100,cum+(d4[i]/sum(d4))*100),pch=3,type="l",lwd=4,col="lightskyblue1")
  if(i==length(d4)){
    points(x=c(as.numeric(names(d4[i])),6),
           y=c(cum+(d4[i]/sum(d4))*100,cum+(d4[i]/sum(d4))*100),pch=3,type="l",lwd=4,col="lightskyblue1")
  }
  cum<-cum+(d4[i]/sum(d4))*100
}

cum<-0
for(i in 1:(length(d5))){
  points(x=c(as.numeric(names(d5[i])),as.numeric(names(d5[i]))),
         y=c(cum,cum+(d5[i]/sum(d5))*100),pch=3,type="l",lwd=4,col="seagreen4",lty=2)
  points(x=c(as.numeric(names(d5[i])),as.numeric(names(d5[i+1]))),
         y=c(cum+(d5[i]/sum(d5))*100,cum+(d5[i]/sum(d5))*100),pch=3,type="l",lwd=4,col="seagreen4",lty=2)
  if(i==length(d5)){
    points(x=c(as.numeric(names(d5[i])),6),
           y=c(cum+(d5[i]/sum(d5))*100,cum+(d5[i]/sum(d5))*100),pch=3,type="l",lwd=4,col="seagreen4",lty=2)
  }
  cum<-cum+(d5[i]/sum(d5))*100
}

legend("topleft",legend=c("GPT-4o","DeepSeek-R1","DeepSeek-V3","Gemi2FTE",
                          "GPT-4"),
       col=c("royalblue4","deepskyblue3","deepskyblue1","lightskyblue1",
             "seagreen4"),border = NA,cex=2,
       lty=c(1,1,1,1,2),lwd=c(7,7,7,7,4))
text(x=0.05,y=101,"F)",xpd=T,font=2,cex=4.5)
dev.off()









plot1<-readPNG(paste0(dir,"Diagnosis_DeepSeekR1_GPT4o.png"))
plot2<-readPNG(paste0(dir,"Diagnosis_Gemini2FlashThinking_GPT4o.png"))
plot3<-readPNG(paste0(dir,"Diagnosis_Gemini2FlashThinking_DeepSeekR1.png"))
plot4<-readPNG(paste0(dir,"Diagnosis_DeepSeekV3_DeepSeekR1.png"))
plot5<-readPNG(paste0(dir,"Violin_Diagnosis_2.png"))
plot6<-readPNG(paste0(dir,"Cumplot_Diagnosis.png"))


png(paste0(dir,"Figure1.png"),width = 2800,height = 1500)
layout(matrix(c(rep(7,28),
                rep(c(rep(1,7),rep(2,7),rep(3,7),rep(4,7)),7),
                rep(c(rep(5,21),rep(6,7)),7)),nrow = 15,byrow = T))
par(mar=c(0,0,0,0))

plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",xaxs="i",yaxs="i")
rasterImage(plot1,0.02,0.02,0.98,0.98)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",xaxs="i",yaxs="i")
rasterImage(plot2,0.02,0.02,0.98,0.98)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",xaxs="i",yaxs="i")
rasterImage(plot3,0.02,0.02,0.98,0.98)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",xaxs="i",yaxs="i")
rasterImage(plot4,0.02,0.02,0.98,0.98)

plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",xaxs="i",yaxs="i")
rasterImage(plot5,0.005,0.005,0.995,0.995)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",xaxs="i",yaxs="i")
rasterImage(plot6,0.02,0.02,0.98,0.98)

plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",xaxs="i",yaxs="i")
text(0.5,0.5,"Diagnosis",cex=10,font=2)
box(which="plot",lwd=7)
dev.off()

plot<-readPNG(paste0(dir,"Figure1.png"))
png(paste0(dir,"Figure1b.png"),width = 2800,height = 1500)
layout(matrix(c(1)))
par(mar=c(0.5,0.5,0.5,0.5))
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",xaxs="i",yaxs="i")
rasterImage(plot,0,0,1,1)
box(which="plot",lwd=10,col="black")
dev.off()

