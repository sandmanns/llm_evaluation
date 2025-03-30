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


#####################
#Diagnosis

##GPT-4o
d1<-c(table(input[1:110,"DiagnosisGPT4o"]))
d2<-c(table(input[111:125,"DiagnosisGPT4o"]))

png(paste0(dir,"FigureS2_Diagnosis_GPT4o.png"),width=700,height=700)
par(mar=c(7,9,1,1),mgp=c(4.5,1.5,0))
plot(NULL,xlim=c(1,5),ylim=c(0,105),yaxs="i",xlab="Likert score",ylab="Cumulative Frequency [%]",
     cex.lab=3,cex.main=3.5,cex.axis = 2.5,las=1,main="")
cum<-0
for(i in 1:(length(d1))){
  points(x=c(as.numeric(names(d1[i])),as.numeric(names(d1[i]))),
         y=c(cum,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="deepskyblue3",lty=1)
  points(x=c(as.numeric(names(d1[i])),as.numeric(names(d1[i+1]))),
         y=c(cum+(d1[i]/sum(d1))*100,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="deepskyblue3",lty=1)
  if(i==length(d1)){
    points(x=c(as.numeric(names(d1[i])),6),
           y=c(cum+(d1[i]/sum(d1))*100,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="deepskyblue3",lty=1)
  }
  cum<-cum+(d1[i]/sum(d1))*100
}

cum<-0
for(i in 1:(length(d2))){
  points(x=c(as.numeric(names(d2[i])),as.numeric(names(d2[i]))),
         y=c(cum,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="red",lty=1)
  points(x=c(as.numeric(names(d2[i])),as.numeric(names(d2[i+1]))),
         y=c(cum+(d2[i]/sum(d2))*100,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="red",lty=1)
  if(i==length(d2)){
    points(x=c(as.numeric(names(d2[i])),6),
           y=c(cum+(d2[i]/sum(d2))*100,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="red",lty=1)
  }
  cum<-cum+(d2[i]/sum(d2))*100
}
legend("topleft",legend=c("old cases (n=110)","new cases (n=15)"),
       fill=c("deepskyblue3","red"),border = NA,cex=3)
dev.off()



##DeepSeek-R1
d1<-c(table(input[1:110,"DiagnosisDeepR"]))
d2<-c(table(input[111:125,"DiagnosisDeepR"]))

png(paste0(dir,"FigureS2_Diagnosis_DeepSeekR1.png"),width=700,height=700)
par(mar=c(7,9,1,1),mgp=c(4.5,1.5,0))
plot(NULL,xlim=c(1,5),ylim=c(0,105),yaxs="i",xlab="Likert score",ylab="Cumulative Frequency [%]",
     cex.lab=3,cex.main=3.5,cex.axis = 2.5,las=1,main="")
cum<-0
for(i in 1:(length(d1))){
  points(x=c(as.numeric(names(d1[i])),as.numeric(names(d1[i]))),
         y=c(cum,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="deepskyblue3",lty=1)
  points(x=c(as.numeric(names(d1[i])),as.numeric(names(d1[i+1]))),
         y=c(cum+(d1[i]/sum(d1))*100,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="deepskyblue3",lty=1)
  if(i==length(d1)){
    points(x=c(as.numeric(names(d1[i])),6),
           y=c(cum+(d1[i]/sum(d1))*100,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="deepskyblue3",lty=1)
  }
  cum<-cum+(d1[i]/sum(d1))*100
}

cum<-0
for(i in 1:(length(d2))){
  points(x=c(as.numeric(names(d2[i])),as.numeric(names(d2[i]))),
         y=c(cum,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="red",lty=1)
  points(x=c(as.numeric(names(d2[i])),as.numeric(names(d2[i+1]))),
         y=c(cum+(d2[i]/sum(d2))*100,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="red",lty=1)
  if(i==length(d2)){
    points(x=c(as.numeric(names(d2[i])),6),
           y=c(cum+(d2[i]/sum(d2))*100,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="red",lty=1)
  }
  cum<-cum+(d2[i]/sum(d2))*100
}
dev.off()



##DeepSeek-V3
d1<-c(table(input[1:110,"DiagnosisDeepV"]))
d2<-c(table(input[111:125,"DiagnosisDeepV"]))

png(paste0(dir,"FigureS2_Diagnosis_DeepSeekV3.png"),width=700,height=700)
par(mar=c(7,9,1,1),mgp=c(4.5,1.5,0))
plot(NULL,xlim=c(1,5),ylim=c(0,105),yaxs="i",xlab="Likert score",ylab="Cumulative Frequency [%]",
     cex.lab=3,cex.main=3.5,cex.axis = 2.5,las=1,main="")
cum<-0
for(i in 1:(length(d1))){
  points(x=c(as.numeric(names(d1[i])),as.numeric(names(d1[i]))),
         y=c(cum,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="deepskyblue3",lty=1)
  points(x=c(as.numeric(names(d1[i])),as.numeric(names(d1[i+1]))),
         y=c(cum+(d1[i]/sum(d1))*100,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="deepskyblue3",lty=1)
  if(i==length(d1)){
    points(x=c(as.numeric(names(d1[i])),6),
           y=c(cum+(d1[i]/sum(d1))*100,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="deepskyblue3",lty=1)
  }
  cum<-cum+(d1[i]/sum(d1))*100
}

cum<-0
for(i in 1:(length(d2))){
  points(x=c(as.numeric(names(d2[i])),as.numeric(names(d2[i]))),
         y=c(cum,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="red",lty=1)
  points(x=c(as.numeric(names(d2[i])),as.numeric(names(d2[i+1]))),
         y=c(cum+(d2[i]/sum(d2))*100,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="red",lty=1)
  if(i==length(d2)){
    points(x=c(as.numeric(names(d2[i])),6),
           y=c(cum+(d2[i]/sum(d2))*100,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="red",lty=1)
  }
  cum<-cum+(d2[i]/sum(d2))*100
}
dev.off()



##Gem2-FTE
d1<-c(table(input[1:110,"DiagnosisGemini"]))
d2<-c(table(input[111:125,"DiagnosisGemini"]))

png(paste0(dir,"FigureS2_Diagnosis_Gem2FTE.png"),width=700,height=700)
par(mar=c(7,9,1,1),mgp=c(4.5,1.5,0))
plot(NULL,xlim=c(1,5),ylim=c(0,105),yaxs="i",xlab="Likert score",ylab="Cumulative Frequency [%]",
     cex.lab=3,cex.main=3.5,cex.axis = 2.5,las=1,main="")
cum<-0
for(i in 1:(length(d1))){
  points(x=c(as.numeric(names(d1[i])),as.numeric(names(d1[i]))),
         y=c(cum,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="deepskyblue3",lty=1)
  points(x=c(as.numeric(names(d1[i])),as.numeric(names(d1[i+1]))),
         y=c(cum+(d1[i]/sum(d1))*100,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="deepskyblue3",lty=1)
  if(i==length(d1)){
    points(x=c(as.numeric(names(d1[i])),6),
           y=c(cum+(d1[i]/sum(d1))*100,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="deepskyblue3",lty=1)
  }
  cum<-cum+(d1[i]/sum(d1))*100
}

cum<-0
for(i in 1:(length(d2))){
  points(x=c(as.numeric(names(d2[i])),as.numeric(names(d2[i]))),
         y=c(cum,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="red",lty=1)
  points(x=c(as.numeric(names(d2[i])),as.numeric(names(d2[i+1]))),
         y=c(cum+(d2[i]/sum(d2))*100,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="red",lty=1)
  if(i==length(d2)){
    points(x=c(as.numeric(names(d2[i])),6),
           y=c(cum+(d2[i]/sum(d2))*100,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="red",lty=1)
  }
  cum<-cum+(d2[i]/sum(d2))*100
}
dev.off()





####################
##Treatment

##GPT-4o
d1<-c(table(input[1:110,"TreatmentGPT4o"]))
d2<-c(table(input[111:125,"TreatmentGPT4o"]))

png(paste0(dir,"FigureS2_Treatment_GPT4o.png"),width=700,height=700)
par(mar=c(7,9,1,1),mgp=c(4.5,1.5,0))
plot(NULL,xlim=c(1,5),ylim=c(0,105),yaxs="i",xlab="Likert score",ylab="Cumulative Frequency [%]",
     cex.lab=3,cex.main=3.5,cex.axis = 2.5,las=1,main="")
cum<-0
for(i in 1:(length(d1))){
  points(x=c(as.numeric(names(d1[i])),as.numeric(names(d1[i]))),
         y=c(cum,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="deepskyblue3",lty=1)
  points(x=c(as.numeric(names(d1[i])),as.numeric(names(d1[i+1]))),
         y=c(cum+(d1[i]/sum(d1))*100,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="deepskyblue3",lty=1)
  if(i==length(d1)){
    points(x=c(as.numeric(names(d1[i])),6),
           y=c(cum+(d1[i]/sum(d1))*100,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="deepskyblue3",lty=1)
  }
  cum<-cum+(d1[i]/sum(d1))*100
}

cum<-0
for(i in 1:(length(d2))){
  points(x=c(as.numeric(names(d2[i])),as.numeric(names(d2[i]))),
         y=c(cum,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="red",lty=1)
  points(x=c(as.numeric(names(d2[i])),as.numeric(names(d2[i+1]))),
         y=c(cum+(d2[i]/sum(d2))*100,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="red",lty=1)
  if(i==length(d2)){
    points(x=c(as.numeric(names(d2[i])),6),
           y=c(cum+(d2[i]/sum(d2))*100,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="red",lty=1)
  }
  cum<-cum+(d2[i]/sum(d2))*100
}
dev.off()



##DeepSeek-R1
d1<-c(table(input[1:110,"TreatmentDeepR"]))
d2<-c(table(input[111:125,"TreatmentDeepR"]))

png(paste0(dir,"FigureS2_Treatment_DeepSeekR1.png"),width=700,height=700)
par(mar=c(7,9,1,1),mgp=c(4.5,1.5,0))
plot(NULL,xlim=c(1,5),ylim=c(0,105),yaxs="i",xlab="Likert score",ylab="Cumulative Frequency [%]",
     cex.lab=3,cex.main=3.5,cex.axis = 2.5,las=1,main="")
cum<-0
for(i in 1:(length(d1))){
  points(x=c(as.numeric(names(d1[i])),as.numeric(names(d1[i]))),
         y=c(cum,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="deepskyblue3",lty=1)
  points(x=c(as.numeric(names(d1[i])),as.numeric(names(d1[i+1]))),
         y=c(cum+(d1[i]/sum(d1))*100,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="deepskyblue3",lty=1)
  if(i==length(d1)){
    points(x=c(as.numeric(names(d1[i])),6),
           y=c(cum+(d1[i]/sum(d1))*100,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="deepskyblue3",lty=1)
  }
  cum<-cum+(d1[i]/sum(d1))*100
}

cum<-0
for(i in 1:(length(d2))){
  points(x=c(as.numeric(names(d2[i])),as.numeric(names(d2[i]))),
         y=c(cum,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="red",lty=1)
  points(x=c(as.numeric(names(d2[i])),as.numeric(names(d2[i+1]))),
         y=c(cum+(d2[i]/sum(d2))*100,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="red",lty=1)
  if(i==length(d2)){
    points(x=c(as.numeric(names(d2[i])),6),
           y=c(cum+(d2[i]/sum(d2))*100,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="red",lty=1)
  }
  cum<-cum+(d2[i]/sum(d2))*100
}
dev.off()



##Gem2-FTE
d1<-c(table(input[1:110,"TreatmentGemini"]))
d2<-c(table(input[111:125,"TreatmentGemini"]))

png(paste0(dir,"FigureS2_Treatment_Gem2FTE.png"),width=700,height=700)
par(mar=c(7,9,1,1),mgp=c(4.5,1.5,0))
plot(NULL,xlim=c(1,5),ylim=c(0,105),yaxs="i",xlab="Likert score",ylab="Cumulative Frequency [%]",
     cex.lab=3,cex.main=3.5,cex.axis = 2.5,las=1,main="")
cum<-0
for(i in 1:(length(d1))){
  points(x=c(as.numeric(names(d1[i])),as.numeric(names(d1[i]))),
         y=c(cum,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="deepskyblue3",lty=1)
  points(x=c(as.numeric(names(d1[i])),as.numeric(names(d1[i+1]))),
         y=c(cum+(d1[i]/sum(d1))*100,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="deepskyblue3",lty=1)
  if(i==length(d1)){
    points(x=c(as.numeric(names(d1[i])),6),
           y=c(cum+(d1[i]/sum(d1))*100,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="deepskyblue3",lty=1)
  }
  cum<-cum+(d1[i]/sum(d1))*100
}

cum<-0
for(i in 1:(length(d2))){
  points(x=c(as.numeric(names(d2[i])),as.numeric(names(d2[i]))),
         y=c(cum,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="red",lty=1)
  points(x=c(as.numeric(names(d2[i])),as.numeric(names(d2[i+1]))),
         y=c(cum+(d2[i]/sum(d2))*100,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="red",lty=1)
  if(i==length(d2)){
    points(x=c(as.numeric(names(d2[i])),6),
           y=c(cum+(d2[i]/sum(d2))*100,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="red",lty=1)
  }
  cum<-cum+(d2[i]/sum(d2))*100
}
dev.off()




###############
##Combine all##
###############

plot1<-readPNG(paste0(dir,"FigureS2_Diagnosis_GPT4o.png"))
plot2<-readPNG(paste0(dir,"FigureS2_Diagnosis_DeepSeekR1.png"))
plot3<-readPNG(paste0(dir,"FigureS2_Diagnosis_DeepSeekV3.png"))
plot4<-readPNG(paste0(dir,"FigureS2_Diagnosis_Gem2FTE.png"))

plot5<-readPNG(paste0(dir,"FigureS2_Treatment_GPT4o.png"))
plot6<-readPNG(paste0(dir,"FigureS2_Treatment_DeepSeekR1.png"))
plot7<-readPNG(paste0(dir,"FigureS2_Treatment_Gem2FTE.png"))

png(paste0(dir,"FigureS2.png"),width = 2900,height = 1500)
layout(matrix(c(9,rep(10,7),rep(11,7),rep(12,7),rep(13,7),
                rep(c(14,rep(1,7),rep(2,7),rep(3,7),rep(4,7)),7),
                rep(c(15,rep(5,7),rep(6,7),rep(7,7),rep(8,7)),7)),ncol = 29,byrow = T))

par(mar=c(0,0,0,0))
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",
     xaxs="i",yaxs="i",xlab="",ylab="",main="",cex.main=6)
rasterImage(plot1,0,0,1,1)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",
     xaxs="i",yaxs="i",xlab="",ylab="",main="",cex.main=6)
rasterImage(plot2,0,0,1,1)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",
     xaxs="i",yaxs="i",xlab="",ylab="",main="",cex.main=6)
rasterImage(plot4,0,0,1,1)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",
     xaxs="i",yaxs="i",xlab="",ylab="",main="",cex.main=6)
rasterImage(plot3,0,0,1,1)

plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",
     xaxs="i",yaxs="i",xlab="",ylab="",main="",cex.main=6)
rasterImage(plot5,0,0,1,1)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",
     xaxs="i",yaxs="i",xlab="",ylab="",main="",cex.main=6)
rasterImage(plot6,0,0,1,1)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",
     xaxs="i",yaxs="i",xlab="",ylab="",main="",cex.main=6)
rasterImage(plot7,0,0,1,1)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",
     xaxs="i",yaxs="i",xlab="",ylab="",main="",cex.main=6)

plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",
     xaxs="i",yaxs="i",xlab="",ylab="")

plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",
     xaxs="i",yaxs="i",xlab="",ylab="")
text(0.55,0.5,"GPT-4o",cex=6,font=2)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",
     xaxs="i",yaxs="i",xlab="",ylab="")
text(0.55,0.5,"DeepSeek-R1",cex=6,font=2)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",
     xaxs="i",yaxs="i",xlab="",ylab="")
text(0.55,0.5,"Gem2FTE",cex=6,font=2)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",
     xaxs="i",yaxs="i",xlab="",ylab="")
text(0.55,0.5,"DeepSeek-V3",cex=6,font=2)

plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",
     xaxs="i",yaxs="i",xlab="",ylab="")
text(0.5,0.55,"Diagnosis",cex=6,font=2,srt=90)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",
     xaxs="i",yaxs="i",xlab="",ylab="")
text(0.5,0.55,"Treatment",cex=6,font=2,srt=90)
dev.off()



