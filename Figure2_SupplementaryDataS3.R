library(openxlsx)
library(vioplot)
dir<-"C:/Paper/Figures/"

input<-read.xlsx(paste0(dir,"Supplementary Data S1.xlsx"),sheet=1)

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


pdf(paste0(dir,"Figure2.pdf"),width = 7.08661,height = 4.72441,pointsize = 7)

layout(matrix(c(rep(c(rep(1,7),rep(2,7),rep(3,7)),7),
                rep(c(rep(4,14),rep(5,7)),7)),nrow = 14,byrow = T))
par(mar=c(0,0,0,0))


##Bubbleplot DeepSeek-R1 vs GPT-4o
d_both<-table(input[,c("TreatmentDeepR","TreatmentGPT4o")])
d_both<-0.2+d_both/max(d_both)
d_both[d_both==0.2]<-0
par(mar=c(4,5,1,1),mgp=c(2.5,1.2,0))
plot(NULL,xlim=c(1,5),ylim=c(1,5),xlab="DeepSeek-R1",ylab="GPT-4o",cex.lab=1,
     cex.axis = 1,las=1)
abline(a=0,b=1,lty=2,col="grey40",lwd=2)
for(i in as.numeric(rownames(d_both))){
  for(j in as.numeric(colnames(d_both))){
    temp<-seq(d_both[which(as.numeric(rownames(d_both))==i),
                     which(as.numeric(colnames(d_both))==j)],0,length.out=20)
    for(k in 1:length(temp)){
      points(i,j,cex=10*temp[k],col=rgb(0.12,0.56,1,0.2),pch=16)
    }
  }
}
min(round(wilcox.test(input[,"TreatmentGPT4o"],
                      input[,"TreatmentDeepR"],
                      paired = T,alternative = "greater")$p.value*3,digits=4),1)
text(x=4.2,y=1.3,paste0("p=0.1522"),xpd=F,font=2,cex=1)
text(x=0.25,y=5,"A",xpd=T,font=2,cex=2)


##Bubbleplot Gemini vs GPT-4o
d_both<-table(input[,c("TreatmentGemini","TreatmentGPT4o")])
d_both<-0.2+d_both/max(d_both)
d_both[d_both==0.2]<-0
par(mar=c(4,5,1,1),mgp=c(2.5,1.2,0))
plot(NULL,xlim=c(1,5),ylim=c(1,5),xlab="Gemi2FTE",ylab="GPT-4o",cex.lab=1,
     cex.axis = 1,las=1)
abline(a=0,b=1,lty=2,col="grey40",lwd=2)
for(i in as.numeric(rownames(d_both))){
  for(j in as.numeric(colnames(d_both))){
    temp<-seq(d_both[which(as.numeric(rownames(d_both))==i),
                     which(as.numeric(colnames(d_both))==j)],0,length.out=20)
    for(k in 1:length(temp)){
      points(i,j,cex=10*temp[k],col=rgb(0.12,0.56,1,0.2),pch=16)
    }
  }
}
min(round(wilcox.test(input$TreatmentGPT4o,input$TreatmentGemini,paired = T,alternative = "greater")$p.value*3,digits=4),1)
text(x=4.2,y=1.3,expression(bold("p=0.0016")),xpd=T,font=2,cex=1)
text(x=0.25,y=5,"B",xpd=T,font=2,cex=2)


##Bubbleplot Gemini vs DeepSeek-R1
d_both<-table(input[,c("TreatmentGemini","TreatmentDeepR")])
d_both<-0.2+d_both/max(d_both)
d_both[d_both==0.2]<-0
par(mar=c(4,5,1,1),mgp=c(2.5,1.2,0))
plot(NULL,xlim=c(1,5),ylim=c(1,5),xlab="Gemi2FTE",ylab="DeepSeek-R1",cex.lab=1,
     cex.axis = 1,las=1)
abline(a=0,b=1,lty=2,col="grey40",lwd=2)
for(i in as.numeric(rownames(d_both))){
  for(j in as.numeric(colnames(d_both))){
    temp<-seq(d_both[which(as.numeric(rownames(d_both))==i),
                     which(as.numeric(colnames(d_both))==j)],0,length.out=20)
    for(k in 1:length(temp)){
      points(i,j,cex=10*temp[k],col=rgb(0.12,0.56,1,0.2),pch=16)
    }
  }
}
min(round(wilcox.test(input$TreatmentDeepR,input$TreatmentGemini,paired = T,alternative = "greater")$p.value*3,digits=4),1)
text(x=4.2,y=1.3,expression(bold("p=0.0235")),xpd=F,font=2,cex=1)
text(x=0.25,y=5,"C",xpd=T,font=2,cex=2)


##Violinplots
par(mar=c(4,5,1,1),mgp=c(4.5,1.5,0))
vioplot(x = as.matrix(input$TreatmentGPT4o),
        xlim=c(0.5,5.5),
        col=c("royalblue3"),ylab="",
        drawRect=F,cex.lab=1,at=1,
        cex.main=1,cex.axis = 1,las=1,xaxt="n",ylim=c(0,5),main="")
points(jitter(rep(1,110),amount = 0.1),input$TreatmentGPT4o[1:110],pch=16,cex=1)
points(jitter(rep(1,15),amount = 0.1),input$TreatmentGPT4o[111:125],pch=16,cex=1.3,col="red")

vioplot(x = as.matrix(input$TreatmentDeepR),add=T,
        col=c("deepskyblue3"),drawRect=F,at=2)
points(jitter(rep(2,110),amount = 0.1),input$TreatmentDeepR[1:110],pch=16,cex=1)
points(jitter(rep(2,15),amount = 0.1),input$TreatmentDeepR[111:125],pch=16,cex=1.3,col="red")

vioplot(x = as.matrix(input$TreatmentGemini),add=T,
        col=c("lightblue1"),drawRect=F,at=3)
points(jitter(rep(3,110),amount = 0.1),input$TreatmentGemini[1:110],pch=16,cex=1)
points(jitter(rep(3,15),amount = 0.1),input$TreatmentGemini[111:125],pch=16,cex=1.3,col="red")

vioplot(x = as.matrix(input$Diagnosis4),add=T,
        col=c("seagreen4"),drawRect=F,at=4)
points(jitter(rep(4,110),amount = 0.1),input$Diagnosis4[1:110],pch=16,cex=1)

vioplot(x = as.matrix(input$Diagnosis3),add=T,
        col=c("seagreen2"),drawRect=F,at=5)
points(jitter(rep(5,110),amount = 0.1),input$Diagnosis3[1:110],pch=16,cex=1)

points(x=c(1,2),y=c(2,2),type="l",lwd=1)
points(x=c(1,1),y=c(2,2.05),type="l",lwd=1)
points(x=c(2,2),y=c(2,2.05),type="l",lwd=1)
text(1.5,1.85,"n.s.",font=2,cex=1)

points(x=c(2,3),y=c(0.8,0.8),type="l",lwd=1)
points(x=c(2,2),y=c(0.8,0.85),type="l",lwd=1)
points(x=c(3,3),y=c(0.8,0.85),type="l",lwd=1)
text(2.5,0.65,"*",font=2,cex=1.5)

points(x=c(1,3),y=c(0.2,0.2),type="l",lwd=1)
points(x=c(1,1),y=c(0.2,0.25),type="l",lwd=1)
points(x=c(3,3),y=c(0.2,0.25),type="l",lwd=1)
text(2,0.05,"*",font=2,cex=1.5)

axis(1,at=c(1,2,3,4,5),labels=c("GPT-4o","DeepSeek-R1","Gemi2FTE",
                                "GPT-4","GPT-3.5"),cex.axis=1,tick=F)
text(x=-0.03,y=2.4,paste0("Likert score"),xpd=T,font=1,cex=1,srt=90)
text(x=-0.01,y=5,"D",xpd=T,font=2,cex=2)

legend("bottomright",legend=c("old cases (n=110)","newly added cases (n=15)"),
       pch=c(16,16),col=c("black","red"),cex=1)


##Cumplot GPT-3.5
d1<-c(table(input[,"TreatmentGPT4o"]))
d2<-c(table(input[,"TreatmentDeepR"]))
d4<-c(table(input[,"TreatmentGemini"]))
d5<-c(table(input[,"Treatment4"]))

par(mar=c(4,5,1,1),mgp=c(2.5,1.2,0))
plot(NULL,xlim=c(1,5),ylim=c(0,105),yaxs="i",xlab="Likert score",ylab="Cumulative Frequency [%]",
     cex.lab=1,cex.main=1,cex.axis = 1,las=1,main="")
cum<-0
for(i in 1:(length(d1))){
  points(x=c(as.numeric(names(d1[i])),as.numeric(names(d1[i]))),
         y=c(cum,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=2,col="royalblue4")
  points(x=c(as.numeric(names(d1[i])),as.numeric(names(d1[i+1]))),
         y=c(cum+(d1[i]/sum(d1))*100,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=2,col="royalblue4")
  if(i==length(d1)){
    points(x=c(as.numeric(names(d1[i])),6),
           y=c(cum+(d1[i]/sum(d1))*100,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=2,col="royalblue4")
  }
  cum<-cum+(d1[i]/sum(d1))*100
}

cum<-0
for(i in 1:(length(d2))){
  points(x=c(as.numeric(names(d2[i])),as.numeric(names(d2[i]))),
         y=c(cum,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=2,col="deepskyblue3")
  points(x=c(as.numeric(names(d2[i])),as.numeric(names(d2[i+1]))),
         y=c(cum+(d2[i]/sum(d2))*100,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=2,col="deepskyblue3")
  if(i==length(d2)){
    points(x=c(as.numeric(names(d2[i])),6),
           y=c(cum+(d2[i]/sum(d2))*100,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=2,col="deepskyblue3")
  }
  cum<-cum+(d2[i]/sum(d2))*100
}

cum<-0
for(i in 1:(length(d4))){
  points(x=c(as.numeric(names(d4[i])),as.numeric(names(d4[i]))),
         y=c(cum,cum+(d4[i]/sum(d4))*100),pch=3,type="l",lwd=2,col="lightskyblue1")
  points(x=c(as.numeric(names(d4[i])),as.numeric(names(d4[i+1]))),
         y=c(cum+(d4[i]/sum(d4))*100,cum+(d4[i]/sum(d4))*100),pch=3,type="l",lwd=2,col="lightskyblue1")
  if(i==length(d4)){
    points(x=c(as.numeric(names(d4[i])),6),
           y=c(cum+(d4[i]/sum(d4))*100,cum+(d4[i]/sum(d4))*100),pch=3,type="l",lwd=2,col="lightskyblue1")
  }
  cum<-cum+(d4[i]/sum(d4))*100
}

cum<-0
for(i in 1:(length(d5))){
  points(x=c(as.numeric(names(d5[i])),as.numeric(names(d5[i]))),
         y=c(cum,cum+(d5[i]/sum(d5))*100),pch=3,type="l",lwd=1.5,col="seagreen4",lty=2)
  points(x=c(as.numeric(names(d5[i])),as.numeric(names(d5[i+1]))),
         y=c(cum+(d5[i]/sum(d5))*100,cum+(d5[i]/sum(d5))*100),pch=3,type="l",lwd=1.5,col="seagreen4",lty=2)
  if(i==length(d5)){
    points(x=c(as.numeric(names(d5[i])),6),
           y=c(cum+(d5[i]/sum(d5))*100,cum+(d5[i]/sum(d5))*100),pch=3,type="l",lwd=1.5,col="seagreen4",lty=2)
  }
  cum<-cum+(d5[i]/sum(d5))*100
}

legend("topleft",legend=c("GPT-4o","DeepSeek-R1","Gemi2FTE",
                          "GPT-4"),
       col=c("royalblue4","deepskyblue3","lightskyblue1",
             "seagreen4"),border = NA,cex=1,
       lty=c(1,1,1,2),lwd=c(3,3,3,1))
text(x=0.2,y=101,"E",xpd=T,font=2,cex=2)

dev.off()


