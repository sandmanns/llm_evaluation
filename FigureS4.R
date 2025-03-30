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

##Gynecology
d1<-c(table(input$DiagnosisGPT4o[input$Clinical.specialty=="Gynecology"]))
d2<-c(table(input$DiagnosisDeepR[input$Clinical.specialty=="Gynecology"]))
d3<-c(table(input$DiagnosisDeepV[input$Clinical.specialty=="Gynecology"]))
d4<-c(table(input$DiagnosisGemini[input$Clinical.specialty=="Gynecology"]))

png(paste0(dir,"FigureS4_Diagnosis_Gynecology.png"),width=700,height=700)
par(mar=c(7,9,1,1),mgp=c(4.5,1.5,0))
plot(NULL,xlim=c(1,5),ylim=c(0,105),yaxs="i",xlab="Likert score",ylab="Cumulative Frequency [%]",
     cex.lab=3,cex.main=3.5,cex.axis = 2.5,las=1,main="")
cum<-0
for(i in 1:(length(d1))){
  points(x=c(as.numeric(names(d1[i])),as.numeric(names(d1[i]))),
         y=c(cum,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="royalblue3")
  points(x=c(as.numeric(names(d1[i])),as.numeric(names(d1[i+1]))),
         y=c(cum+(d1[i]/sum(d1))*100,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="royalblue3")
  if(i==length(d1)){
    points(x=c(as.numeric(names(d1[i])),6),
           y=c(cum+(d1[i]/sum(d1))*100,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="royalblue3")
  }
  cum<-cum+(d1[i]/sum(d1))*100
}
cum<-0
for(i in 1:(length(d2))){
  points(x=c(as.numeric(names(d2[i])),as.numeric(names(d2[i]))),
         y=c(cum,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="deepskyblue3")
  points(x=c(as.numeric(names(d2[i])),as.numeric(names(d2[i+1]))),
         y=c(cum+(d2[i]/sum(d2))*100,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="deepskyblue3")
  if(i==length(d2)){
    points(x=c(as.numeric(names(d2[i])),6),
           y=c(cum+(d2[i]/sum(d2))*100,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="deepskyblue3")
  }
  cum<-cum+(d2[i]/sum(d2))*100
}
cum<-0
for(i in 1:(length(d3))){
  points(x=c(as.numeric(names(d3[i])),as.numeric(names(d3[i]))),
         y=c(cum,cum+(d3[i]/sum(d3))*100),pch=3,type="l",lwd=7,col="deepskyblue1")
  points(x=c(as.numeric(names(d3[i])),as.numeric(names(d3[i+1]))),
         y=c(cum+(d3[i]/sum(d3))*100,cum+(d3[i]/sum(d3))*100),pch=3,type="l",lwd=7,col="deepskyblue1")
  if(i==length(d3)){
    points(x=c(as.numeric(names(d3[i])),6),
           y=c(cum+(d3[i]/sum(d3))*100,cum+(d3[i]/sum(d3))*100),pch=3,type="l",lwd=7,col="deepskyblue1")
  }
  cum<-cum+(d3[i]/sum(d3))*100
}
cum<-0
for(i in 1:(length(d4))){
  points(x=c(as.numeric(names(d4[i])),as.numeric(names(d4[i]))),
         y=c(cum,cum+(d4[i]/sum(d4))*100),pch=3,type="l",lwd=7,col="lightblue1")
  points(x=c(as.numeric(names(d4[i])),as.numeric(names(d4[i+1]))),
         y=c(cum+(d4[i]/sum(d4))*100,cum+(d4[i]/sum(d4))*100),pch=3,type="l",lwd=7,col="lightblue1")
  if(i==length(d4)){
    points(x=c(as.numeric(names(d4[i])),6),
           y=c(cum+(d4[i]/sum(d4))*100,cum+(d4[i]/sum(d4))*100),pch=3,type="l",lwd=7,col="lightblue1")
  }
  cum<-cum+(d4[i]/sum(d4))*100
}
legend("topleft",legend=c("GPT-4o","DeepSeek-R1","DeepSeek-V3","Gem2FTE"),
       fill=c("royalblue3","deepskyblue3","deepskyblue1","lightblue1"),border = NA,cex=2.5)
dev.off()



##Internal Medicine
d1<-c(table(input$DiagnosisGPT4o[input$Clinical.specialty=="Internal Medicine"]))
d2<-c(table(input$DiagnosisDeepR[input$Clinical.specialty=="Internal Medicine"]))
d3<-c(table(input$DiagnosisDeepV[input$Clinical.specialty=="Internal Medicine"]))
d4<-c(table(input$DiagnosisGemini[input$Clinical.specialty=="Internal Medicine"]))

png(paste0(dir,"FigureS4_Diagnosis_InternalMedicine.png"),width=700,height=700)
par(mar=c(7,9,1,1),mgp=c(4.5,1.5,0))
plot(NULL,xlim=c(1,5),ylim=c(0,105),yaxs="i",xlab="Likert score",ylab="Cumulative Frequency [%]",
     cex.lab=3,cex.main=3.5,cex.axis = 2.5,las=1,main="")
cum<-0
for(i in 1:(length(d1))){
  points(x=c(as.numeric(names(d1[i])),as.numeric(names(d1[i]))),
         y=c(cum,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="royalblue3")
  points(x=c(as.numeric(names(d1[i])),as.numeric(names(d1[i+1]))),
         y=c(cum+(d1[i]/sum(d1))*100,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="royalblue3")
  if(i==length(d1)){
    points(x=c(as.numeric(names(d1[i])),6),
           y=c(cum+(d1[i]/sum(d1))*100,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="royalblue3")
  }
  cum<-cum+(d1[i]/sum(d1))*100
}
cum<-0
for(i in 1:(length(d2))){
  points(x=c(as.numeric(names(d2[i])),as.numeric(names(d2[i]))),
         y=c(cum,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="deepskyblue3")
  points(x=c(as.numeric(names(d2[i])),as.numeric(names(d2[i+1]))),
         y=c(cum+(d2[i]/sum(d2))*100,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="deepskyblue3")
  if(i==length(d2)){
    points(x=c(as.numeric(names(d2[i])),6),
           y=c(cum+(d2[i]/sum(d2))*100,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="deepskyblue3")
  }
  cum<-cum+(d2[i]/sum(d2))*100
}
cum<-0
for(i in 1:(length(d3))){
  points(x=c(as.numeric(names(d3[i])),as.numeric(names(d3[i]))),
         y=c(cum,cum+(d3[i]/sum(d3))*100),pch=3,type="l",lwd=7,col="deepskyblue1")
  points(x=c(as.numeric(names(d3[i])),as.numeric(names(d3[i+1]))),
         y=c(cum+(d3[i]/sum(d3))*100,cum+(d3[i]/sum(d3))*100),pch=3,type="l",lwd=7,col="deepskyblue1")
  if(i==length(d3)){
    points(x=c(as.numeric(names(d3[i])),6),
           y=c(cum+(d3[i]/sum(d3))*100,cum+(d3[i]/sum(d3))*100),pch=3,type="l",lwd=7,col="deepskyblue1")
  }
  cum<-cum+(d3[i]/sum(d3))*100
}
cum<-0
for(i in 1:(length(d4))){
  points(x=c(as.numeric(names(d4[i])),as.numeric(names(d4[i]))),
         y=c(cum,cum+(d4[i]/sum(d4))*100),pch=3,type="l",lwd=7,col="lightblue1")
  points(x=c(as.numeric(names(d4[i])),as.numeric(names(d4[i+1]))),
         y=c(cum+(d4[i]/sum(d4))*100,cum+(d4[i]/sum(d4))*100),pch=3,type="l",lwd=7,col="lightblue1")
  if(i==length(d4)){
    points(x=c(as.numeric(names(d4[i])),6),
           y=c(cum+(d4[i]/sum(d4))*100,cum+(d4[i]/sum(d4))*100),pch=3,type="l",lwd=7,col="lightblue1")
  }
  cum<-cum+(d4[i]/sum(d4))*100
}
dev.off()



##Neurology
d1<-c(table(input$DiagnosisGPT4o[input$Clinical.specialty=="Neurology"]))
d2<-c(table(input$DiagnosisDeepR[input$Clinical.specialty=="Neurology"]))
d3<-c(table(input$DiagnosisDeepV[input$Clinical.specialty=="Neurology"]))
d4<-c(table(input$DiagnosisGemini[input$Clinical.specialty=="Neurology"]))

png(paste0(dir,"FigureS4_Diagnosis_Neurology.png"),width=700,height=700)
par(mar=c(7,9,1,1),mgp=c(4.5,1.5,0))
plot(NULL,xlim=c(1,5),ylim=c(0,105),yaxs="i",xlab="Likert score",ylab="Cumulative Frequency [%]",
     cex.lab=3,cex.main=3.5,cex.axis = 2.5,las=1,main="")
cum<-0
for(i in 1:(length(d1))){
  points(x=c(as.numeric(names(d1[i])),as.numeric(names(d1[i]))),
         y=c(cum,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="royalblue3")
  points(x=c(as.numeric(names(d1[i])),as.numeric(names(d1[i+1]))),
         y=c(cum+(d1[i]/sum(d1))*100,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="royalblue3")
  if(i==length(d1)){
    points(x=c(as.numeric(names(d1[i])),6),
           y=c(cum+(d1[i]/sum(d1))*100,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="royalblue3")
  }
  cum<-cum+(d1[i]/sum(d1))*100
}
cum<-0
for(i in 1:(length(d2))){
  points(x=c(as.numeric(names(d2[i])),as.numeric(names(d2[i]))),
         y=c(cum,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="deepskyblue3")
  points(x=c(as.numeric(names(d2[i])),as.numeric(names(d2[i+1]))),
         y=c(cum+(d2[i]/sum(d2))*100,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="deepskyblue3")
  if(i==length(d2)){
    points(x=c(as.numeric(names(d2[i])),6),
           y=c(cum+(d2[i]/sum(d2))*100,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="deepskyblue3")
  }
  cum<-cum+(d2[i]/sum(d2))*100
}
cum<-0
for(i in 1:(length(d3))){
  points(x=c(as.numeric(names(d3[i])),as.numeric(names(d3[i]))),
         y=c(cum,cum+(d3[i]/sum(d3))*100),pch=3,type="l",lwd=7,col="deepskyblue1")
  points(x=c(as.numeric(names(d3[i])),as.numeric(names(d3[i+1]))),
         y=c(cum+(d3[i]/sum(d3))*100,cum+(d3[i]/sum(d3))*100),pch=3,type="l",lwd=7,col="deepskyblue1")
  if(i==length(d3)){
    points(x=c(as.numeric(names(d3[i])),6),
           y=c(cum+(d3[i]/sum(d3))*100,cum+(d3[i]/sum(d3))*100),pch=3,type="l",lwd=7,col="deepskyblue1")
  }
  cum<-cum+(d3[i]/sum(d3))*100
}
cum<-0
for(i in 1:(length(d4))){
  points(x=c(as.numeric(names(d4[i])),as.numeric(names(d4[i]))),
         y=c(cum,cum+(d4[i]/sum(d4))*100),pch=3,type="l",lwd=7,col="lightblue1")
  points(x=c(as.numeric(names(d4[i])),as.numeric(names(d4[i+1]))),
         y=c(cum+(d4[i]/sum(d4))*100,cum+(d4[i]/sum(d4))*100),pch=3,type="l",lwd=7,col="lightblue1")
  if(i==length(d4)){
    points(x=c(as.numeric(names(d4[i])),6),
           y=c(cum+(d4[i]/sum(d4))*100,cum+(d4[i]/sum(d4))*100),pch=3,type="l",lwd=7,col="lightblue1")
  }
  cum<-cum+(d4[i]/sum(d4))*100
}
dev.off()



##Pediatrics
d1<-c(table(input$DiagnosisGPT4o[input$Clinical.specialty=="Pediatrics"]))
d2<-c(table(input$DiagnosisDeepR[input$Clinical.specialty=="Pediatrics"]))
d3<-c(table(input$DiagnosisDeepV[input$Clinical.specialty=="Pediatrics"]))
d4<-c(table(input$DiagnosisGemini[input$Clinical.specialty=="Pediatrics"]))

png(paste0(dir,"FigureS4_Diagnosis_Pediatrics.png"),width=700,height=700)
par(mar=c(7,9,1,1),mgp=c(4.5,1.5,0))
plot(NULL,xlim=c(1,5),ylim=c(0,105),yaxs="i",xlab="Likert score",ylab="Cumulative Frequency [%]",
     cex.lab=3,cex.main=3.5,cex.axis = 2.5,las=1,main="")
cum<-0
for(i in 1:(length(d1))){
  points(x=c(as.numeric(names(d1[i])),as.numeric(names(d1[i]))),
         y=c(cum,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="royalblue3")
  points(x=c(as.numeric(names(d1[i])),as.numeric(names(d1[i+1]))),
         y=c(cum+(d1[i]/sum(d1))*100,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="royalblue3")
  if(i==length(d1)){
    points(x=c(as.numeric(names(d1[i])),6),
           y=c(cum+(d1[i]/sum(d1))*100,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="royalblue3")
  }
  cum<-cum+(d1[i]/sum(d1))*100
}
cum<-0
for(i in 1:(length(d2))){
  points(x=c(as.numeric(names(d2[i])),as.numeric(names(d2[i]))),
         y=c(cum,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="deepskyblue3")
  points(x=c(as.numeric(names(d2[i])),as.numeric(names(d2[i+1]))),
         y=c(cum+(d2[i]/sum(d2))*100,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="deepskyblue3")
  if(i==length(d2)){
    points(x=c(as.numeric(names(d2[i])),6),
           y=c(cum+(d2[i]/sum(d2))*100,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="deepskyblue3")
  }
  cum<-cum+(d2[i]/sum(d2))*100
}
cum<-0
for(i in 1:(length(d3))){
  points(x=c(as.numeric(names(d3[i])),as.numeric(names(d3[i]))),
         y=c(cum,cum+(d3[i]/sum(d3))*100),pch=3,type="l",lwd=7,col="deepskyblue1")
  points(x=c(as.numeric(names(d3[i])),as.numeric(names(d3[i+1]))),
         y=c(cum+(d3[i]/sum(d3))*100,cum+(d3[i]/sum(d3))*100),pch=3,type="l",lwd=7,col="deepskyblue1")
  if(i==length(d3)){
    points(x=c(as.numeric(names(d3[i])),6),
           y=c(cum+(d3[i]/sum(d3))*100,cum+(d3[i]/sum(d3))*100),pch=3,type="l",lwd=7,col="deepskyblue1")
  }
  cum<-cum+(d3[i]/sum(d3))*100
}
cum<-0
for(i in 1:(length(d4))){
  points(x=c(as.numeric(names(d4[i])),as.numeric(names(d4[i]))),
         y=c(cum,cum+(d4[i]/sum(d4))*100),pch=3,type="l",lwd=7,col="lightblue1")
  points(x=c(as.numeric(names(d4[i])),as.numeric(names(d4[i+1]))),
         y=c(cum+(d4[i]/sum(d4))*100,cum+(d4[i]/sum(d4))*100),pch=3,type="l",lwd=7,col="lightblue1")
  if(i==length(d4)){
    points(x=c(as.numeric(names(d4[i])),6),
           y=c(cum+(d4[i]/sum(d4))*100,cum+(d4[i]/sum(d4))*100),pch=3,type="l",lwd=7,col="lightblue1")
  }
  cum<-cum+(d4[i]/sum(d4))*100
}
dev.off()



##Surgery
d1<-c(table(input$DiagnosisGPT4o[input$Clinical.specialty=="Surgery"]))
d2<-c(table(input$DiagnosisDeepR[input$Clinical.specialty=="Surgery"]))
d3<-c(table(input$DiagnosisDeepV[input$Clinical.specialty=="Surgery"]))
d4<-c(table(input$DiagnosisGemini[input$Clinical.specialty=="Surgery"]))

png(paste0(dir,"FigureS4_Diagnosis_Surgery.png"),width=700,height=700)
par(mar=c(7,9,1,1),mgp=c(4.5,1.5,0))
plot(NULL,xlim=c(1,5),ylim=c(0,105),yaxs="i",xlab="Likert score",ylab="Cumulative Frequency [%]",
     cex.lab=3,cex.main=3.5,cex.axis = 2.5,las=1,main="")
cum<-0
for(i in 1:(length(d1))){
  points(x=c(as.numeric(names(d1[i])),as.numeric(names(d1[i]))),
         y=c(cum,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="royalblue3")
  points(x=c(as.numeric(names(d1[i])),as.numeric(names(d1[i+1]))),
         y=c(cum+(d1[i]/sum(d1))*100,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="royalblue3")
  if(i==length(d1)){
    points(x=c(as.numeric(names(d1[i])),6),
           y=c(cum+(d1[i]/sum(d1))*100,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="royalblue3")
  }
  cum<-cum+(d1[i]/sum(d1))*100
}
cum<-0
for(i in 1:(length(d2))){
  points(x=c(as.numeric(names(d2[i])),as.numeric(names(d2[i]))),
         y=c(cum,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="deepskyblue3")
  points(x=c(as.numeric(names(d2[i])),as.numeric(names(d2[i+1]))),
         y=c(cum+(d2[i]/sum(d2))*100,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="deepskyblue3")
  if(i==length(d2)){
    points(x=c(as.numeric(names(d2[i])),6),
           y=c(cum+(d2[i]/sum(d2))*100,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="deepskyblue3")
  }
  cum<-cum+(d2[i]/sum(d2))*100
}
cum<-0
for(i in 1:(length(d3))){
  points(x=c(as.numeric(names(d3[i])),as.numeric(names(d3[i]))),
         y=c(cum,cum+(d3[i]/sum(d3))*100),pch=3,type="l",lwd=7,col="deepskyblue1")
  points(x=c(as.numeric(names(d3[i])),as.numeric(names(d3[i+1]))),
         y=c(cum+(d3[i]/sum(d3))*100,cum+(d3[i]/sum(d3))*100),pch=3,type="l",lwd=7,col="deepskyblue1")
  if(i==length(d3)){
    points(x=c(as.numeric(names(d3[i])),6),
           y=c(cum+(d3[i]/sum(d3))*100,cum+(d3[i]/sum(d3))*100),pch=3,type="l",lwd=7,col="deepskyblue1")
  }
  cum<-cum+(d3[i]/sum(d3))*100
}
cum<-0
for(i in 1:(length(d4))){
  points(x=c(as.numeric(names(d4[i])),as.numeric(names(d4[i]))),
         y=c(cum,cum+(d4[i]/sum(d4))*100),pch=3,type="l",lwd=7,col="lightblue1")
  points(x=c(as.numeric(names(d4[i])),as.numeric(names(d4[i+1]))),
         y=c(cum+(d4[i]/sum(d4))*100,cum+(d4[i]/sum(d4))*100),pch=3,type="l",lwd=7,col="lightblue1")
  if(i==length(d4)){
    points(x=c(as.numeric(names(d4[i])),6),
           y=c(cum+(d4[i]/sum(d4))*100,cum+(d4[i]/sum(d4))*100),pch=3,type="l",lwd=7,col="lightblue1")
  }
  cum<-cum+(d4[i]/sum(d4))*100
}
dev.off()





#############
##Treatment##
#############

##Gynecology
d1<-c(table(input$TreatmentGPT4o[input$Clinical.specialty=="Gynecology"]))
d2<-c(table(input$TreatmentDeepR[input$Clinical.specialty=="Gynecology"]))
d4<-c(table(input$TreatmentGemini[input$Clinical.specialty=="Gynecology"]))

png(paste0(dir,"FigureS4_Treatment_Gynecology.png"),width=700,height=700)
par(mar=c(7,9,1,1),mgp=c(4.5,1.5,0))
plot(NULL,xlim=c(1,5),ylim=c(0,105),yaxs="i",xlab="Likert score",ylab="Cumulative Frequency [%]",
     cex.lab=3,cex.main=3.5,cex.axis = 2.5,las=1,main="")
cum<-0
for(i in 1:(length(d1))){
  points(x=c(as.numeric(names(d1[i])),as.numeric(names(d1[i]))),
         y=c(cum,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="royalblue3")
  points(x=c(as.numeric(names(d1[i])),as.numeric(names(d1[i+1]))),
         y=c(cum+(d1[i]/sum(d1))*100,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="royalblue3")
  if(i==length(d1)){
    points(x=c(as.numeric(names(d1[i])),6),
           y=c(cum+(d1[i]/sum(d1))*100,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="royalblue3")
  }
  cum<-cum+(d1[i]/sum(d1))*100
}
cum<-0
for(i in 1:(length(d2))){
  points(x=c(as.numeric(names(d2[i])),as.numeric(names(d2[i]))),
         y=c(cum,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="deepskyblue3")
  points(x=c(as.numeric(names(d2[i])),as.numeric(names(d2[i+1]))),
         y=c(cum+(d2[i]/sum(d2))*100,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="deepskyblue3")
  if(i==length(d2)){
    points(x=c(as.numeric(names(d2[i])),6),
           y=c(cum+(d2[i]/sum(d2))*100,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="deepskyblue3")
  }
  cum<-cum+(d2[i]/sum(d2))*100
}
cum<-0
for(i in 1:(length(d4))){
  points(x=c(as.numeric(names(d4[i])),as.numeric(names(d4[i]))),
         y=c(cum,cum+(d4[i]/sum(d4))*100),pch=3,type="l",lwd=7,col="lightblue1")
  points(x=c(as.numeric(names(d4[i])),as.numeric(names(d4[i+1]))),
         y=c(cum+(d4[i]/sum(d4))*100,cum+(d4[i]/sum(d4))*100),pch=3,type="l",lwd=7,col="lightblue1")
  if(i==length(d4)){
    points(x=c(as.numeric(names(d4[i])),6),
           y=c(cum+(d4[i]/sum(d4))*100,cum+(d4[i]/sum(d4))*100),pch=3,type="l",lwd=7,col="lightblue1")
  }
  cum<-cum+(d4[i]/sum(d4))*100
}
legend("topleft",legend=c("GPT-4o","DeepSeek-R1","GemFTE"),
       fill=c("royalblue3","deepskyblue3","lightblue1"),border = NA,cex=2.5)
dev.off()



##Internal Medicine
d1<-c(table(input$TreatmentGPT4o[input$Clinical.specialty=="Internal Medicine"]))
d2<-c(table(input$TreatmentDeepR[input$Clinical.specialty=="Internal Medicine"]))
d4<-c(table(input$TreatmentGemini[input$Clinical.specialty=="Internal Medicine"]))

png(paste0(dir,"FigureS4_Treatment_InternalMedicine.png"),width=700,height=700)
par(mar=c(7,9,1,1),mgp=c(4.5,1.5,0))
plot(NULL,xlim=c(1,5),ylim=c(0,105),yaxs="i",xlab="Likert score",ylab="Cumulative Frequency [%]",
     cex.lab=3,cex.main=3.5,cex.axis = 2.5,las=1,main="")
cum<-0
for(i in 1:(length(d1))){
  points(x=c(as.numeric(names(d1[i])),as.numeric(names(d1[i]))),
         y=c(cum,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="royalblue3")
  points(x=c(as.numeric(names(d1[i])),as.numeric(names(d1[i+1]))),
         y=c(cum+(d1[i]/sum(d1))*100,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="royalblue3")
  if(i==length(d1)){
    points(x=c(as.numeric(names(d1[i])),6),
           y=c(cum+(d1[i]/sum(d1))*100,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="royalblue3")
  }
  cum<-cum+(d1[i]/sum(d1))*100
}
cum<-0
for(i in 1:(length(d2))){
  points(x=c(as.numeric(names(d2[i])),as.numeric(names(d2[i]))),
         y=c(cum,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="deepskyblue3")
  points(x=c(as.numeric(names(d2[i])),as.numeric(names(d2[i+1]))),
         y=c(cum+(d2[i]/sum(d2))*100,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="deepskyblue3")
  if(i==length(d2)){
    points(x=c(as.numeric(names(d2[i])),6),
           y=c(cum+(d2[i]/sum(d2))*100,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="deepskyblue3")
  }
  cum<-cum+(d2[i]/sum(d2))*100
}
cum<-0
for(i in 1:(length(d4))){
  points(x=c(as.numeric(names(d4[i])),as.numeric(names(d4[i]))),
         y=c(cum,cum+(d4[i]/sum(d4))*100),pch=3,type="l",lwd=7,col="lightblue1")
  points(x=c(as.numeric(names(d4[i])),as.numeric(names(d4[i+1]))),
         y=c(cum+(d4[i]/sum(d4))*100,cum+(d4[i]/sum(d4))*100),pch=3,type="l",lwd=7,col="lightblue1")
  if(i==length(d4)){
    points(x=c(as.numeric(names(d4[i])),6),
           y=c(cum+(d4[i]/sum(d4))*100,cum+(d4[i]/sum(d4))*100),pch=3,type="l",lwd=7,col="lightblue1")
  }
  cum<-cum+(d4[i]/sum(d4))*100
}
dev.off()



##Neurology
d1<-c(table(input$TreatmentGPT4o[input$Clinical.specialty=="Neurology"]))
d2<-c(table(input$TreatmentDeepR[input$Clinical.specialty=="Neurology"]))
d4<-c(table(input$TreatmentGemini[input$Clinical.specialty=="Neurology"]))

png(paste0(dir,"FigureS4_Treatment_Neurology.png"),width=700,height=700)
par(mar=c(7,9,1,1),mgp=c(4.5,1.5,0))
plot(NULL,xlim=c(1,5),ylim=c(0,105),yaxs="i",xlab="Likert score",ylab="Cumulative Frequency [%]",
     cex.lab=3,cex.main=3.5,cex.axis = 2.5,las=1,main="")
cum<-0
for(i in 1:(length(d1))){
  points(x=c(as.numeric(names(d1[i])),as.numeric(names(d1[i]))),
         y=c(cum,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="royalblue3")
  points(x=c(as.numeric(names(d1[i])),as.numeric(names(d1[i+1]))),
         y=c(cum+(d1[i]/sum(d1))*100,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="royalblue3")
  if(i==length(d1)){
    points(x=c(as.numeric(names(d1[i])),6),
           y=c(cum+(d1[i]/sum(d1))*100,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="royalblue3")
  }
  cum<-cum+(d1[i]/sum(d1))*100
}
cum<-0
for(i in 1:(length(d2))){
  points(x=c(as.numeric(names(d2[i])),as.numeric(names(d2[i]))),
         y=c(cum,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="deepskyblue3")
  points(x=c(as.numeric(names(d2[i])),as.numeric(names(d2[i+1]))),
         y=c(cum+(d2[i]/sum(d2))*100,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="deepskyblue3")
  if(i==length(d2)){
    points(x=c(as.numeric(names(d2[i])),6),
           y=c(cum+(d2[i]/sum(d2))*100,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="deepskyblue3")
  }
  cum<-cum+(d2[i]/sum(d2))*100
}
cum<-0
for(i in 1:(length(d4))){
  points(x=c(as.numeric(names(d4[i])),as.numeric(names(d4[i]))),
         y=c(cum,cum+(d4[i]/sum(d4))*100),pch=3,type="l",lwd=7,col="lightblue1")
  points(x=c(as.numeric(names(d4[i])),as.numeric(names(d4[i+1]))),
         y=c(cum+(d4[i]/sum(d4))*100,cum+(d4[i]/sum(d4))*100),pch=3,type="l",lwd=7,col="lightblue1")
  if(i==length(d4)){
    points(x=c(as.numeric(names(d4[i])),6),
           y=c(cum+(d4[i]/sum(d4))*100,cum+(d4[i]/sum(d4))*100),pch=3,type="l",lwd=7,col="lightblue1")
  }
  cum<-cum+(d4[i]/sum(d4))*100
}
dev.off()



##Pediatrics
d1<-c(table(input$TreatmentGPT4o[input$Clinical.specialty=="Pediatrics"]))
d2<-c(table(input$TreatmentDeepR[input$Clinical.specialty=="Pediatrics"]))
d4<-c(table(input$TreatmentGemini[input$Clinical.specialty=="Pediatrics"]))

png(paste0(dir,"FigureS4_Treatment_Pediatrics.png"),width=700,height=700)
par(mar=c(7,9,1,1),mgp=c(4.5,1.5,0))
plot(NULL,xlim=c(1,5),ylim=c(0,105),yaxs="i",xlab="Likert score",ylab="Cumulative Frequency [%]",
     cex.lab=3,cex.main=3.5,cex.axis = 2.5,las=1,main="")
cum<-0
for(i in 1:(length(d1))){
  points(x=c(as.numeric(names(d1[i])),as.numeric(names(d1[i]))),
         y=c(cum,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="royalblue3")
  points(x=c(as.numeric(names(d1[i])),as.numeric(names(d1[i+1]))),
         y=c(cum+(d1[i]/sum(d1))*100,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="royalblue3")
  if(i==length(d1)){
    points(x=c(as.numeric(names(d1[i])),6),
           y=c(cum+(d1[i]/sum(d1))*100,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="royalblue3")
  }
  cum<-cum+(d1[i]/sum(d1))*100
}
cum<-0
for(i in 1:(length(d2))){
  points(x=c(as.numeric(names(d2[i])),as.numeric(names(d2[i]))),
         y=c(cum,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="deepskyblue3")
  points(x=c(as.numeric(names(d2[i])),as.numeric(names(d2[i+1]))),
         y=c(cum+(d2[i]/sum(d2))*100,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="deepskyblue3")
  if(i==length(d2)){
    points(x=c(as.numeric(names(d2[i])),6),
           y=c(cum+(d2[i]/sum(d2))*100,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="deepskyblue3")
  }
  cum<-cum+(d2[i]/sum(d2))*100
}
cum<-0
for(i in 1:(length(d4))){
  points(x=c(as.numeric(names(d4[i])),as.numeric(names(d4[i]))),
         y=c(cum,cum+(d4[i]/sum(d4))*100),pch=3,type="l",lwd=7,col="lightblue1")
  points(x=c(as.numeric(names(d4[i])),as.numeric(names(d4[i+1]))),
         y=c(cum+(d4[i]/sum(d4))*100,cum+(d4[i]/sum(d4))*100),pch=3,type="l",lwd=7,col="lightblue1")
  if(i==length(d4)){
    points(x=c(as.numeric(names(d4[i])),6),
           y=c(cum+(d4[i]/sum(d4))*100,cum+(d4[i]/sum(d4))*100),pch=3,type="l",lwd=7,col="lightblue1")
  }
  cum<-cum+(d4[i]/sum(d4))*100
}
dev.off()



##Surgery
d1<-c(table(input$TreatmentGPT4o[input$Clinical.specialty=="Surgery"]))
d2<-c(table(input$TreatmentDeepR[input$Clinical.specialty=="Surgery"]))
d4<-c(table(input$TreatmentGemini[input$Clinical.specialty=="Surgery"]))

png(paste0(dir,"FigureS4_Treatment_Surgery.png"),width=700,height=700)
par(mar=c(7,9,1,1),mgp=c(4.5,1.5,0))
plot(NULL,xlim=c(1,5),ylim=c(0,105),yaxs="i",xlab="Likert score",ylab="Cumulative Frequency [%]",
     cex.lab=3,cex.main=3.5,cex.axis = 2.5,las=1,main="")
cum<-0
for(i in 1:(length(d1))){
  points(x=c(as.numeric(names(d1[i])),as.numeric(names(d1[i]))),
         y=c(cum,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="royalblue3")
  points(x=c(as.numeric(names(d1[i])),as.numeric(names(d1[i+1]))),
         y=c(cum+(d1[i]/sum(d1))*100,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="royalblue3")
  if(i==length(d1)){
    points(x=c(as.numeric(names(d1[i])),6),
           y=c(cum+(d1[i]/sum(d1))*100,cum+(d1[i]/sum(d1))*100),pch=3,type="l",lwd=7,col="royalblue3")
  }
  cum<-cum+(d1[i]/sum(d1))*100
}
cum<-0
for(i in 1:(length(d2))){
  points(x=c(as.numeric(names(d2[i])),as.numeric(names(d2[i]))),
         y=c(cum,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="deepskyblue3")
  points(x=c(as.numeric(names(d2[i])),as.numeric(names(d2[i+1]))),
         y=c(cum+(d2[i]/sum(d2))*100,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="deepskyblue3")
  if(i==length(d2)){
    points(x=c(as.numeric(names(d2[i])),6),
           y=c(cum+(d2[i]/sum(d2))*100,cum+(d2[i]/sum(d2))*100),pch=3,type="l",lwd=7,col="deepskyblue3")
  }
  cum<-cum+(d2[i]/sum(d2))*100
}
cum<-0
for(i in 1:(length(d4))){
  points(x=c(as.numeric(names(d4[i])),as.numeric(names(d4[i]))),
         y=c(cum,cum+(d4[i]/sum(d4))*100),pch=3,type="l",lwd=7,col="lightblue1")
  points(x=c(as.numeric(names(d4[i])),as.numeric(names(d4[i+1]))),
         y=c(cum+(d4[i]/sum(d4))*100,cum+(d4[i]/sum(d4))*100),pch=3,type="l",lwd=7,col="lightblue1")
  if(i==length(d4)){
    points(x=c(as.numeric(names(d4[i])),6),
           y=c(cum+(d4[i]/sum(d4))*100,cum+(d4[i]/sum(d4))*100),pch=3,type="l",lwd=7,col="lightblue1")
  }
  cum<-cum+(d4[i]/sum(d4))*100
}
dev.off()




###############
##Combine all##
###############

plot1<-readPNG(paste0(dir,"FigureS4_Diagnosis_Gynecology.png"))
plot2<-readPNG(paste0(dir,"FigureS4_Diagnosis_InternalMedicine.png"))
plot3<-readPNG(paste0(dir,"FigureS4_Diagnosis_Neurology.png"))
plot4<-readPNG(paste0(dir,"FigureS4_Diagnosis_Pediatrics.png"))
plot5<-readPNG(paste0(dir,"FigureS4_Diagnosis_Surgery.png"))

plot1c<-readPNG(paste0(dir,"FigureS4_Treatment_Gynecology.png"))
plot2c<-readPNG(paste0(dir,"FigureS4_Treatment_InternalMedicine.png"))
plot3c<-readPNG(paste0(dir,"FigureS4_Treatment_Neurology.png"))
plot4c<-readPNG(paste0(dir,"FigureS4_Treatment_Pediatrics.png"))
plot5c<-readPNG(paste0(dir,"FigureS4_Treatment_Surgery.png"))

png(paste0(dir,"FigureS4.png"),width = 1500,height = 3200)
layout(matrix(c(11,rep(12,5),rep(13,5),
                rep(c(14,rep(1,5),rep(2,5)),5),
                rep(c(15,rep(3,5),rep(4,5)),5),
                rep(c(16,rep(5,5),rep(6,5)),5),
                rep(c(17,rep(7,5),rep(8,5)),5),
                rep(c(18,rep(9,5),rep(10,5)),5)),ncol = 11,byrow = T))
par(mar=c(0,0,0,0))
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",
     xaxs="i",yaxs="i",xlab="",ylab="",main="",cex.main=6)
rasterImage(plot1,0,0,1,1)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",
     xaxs="i",yaxs="i",xlab="",ylab="",main="",cex.main=6)
rasterImage(plot1c,0,0,1,1)

plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",
     xaxs="i",yaxs="i",xlab="",ylab="",main="",cex.main=6)
rasterImage(plot2,0,0,1,1)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",
     xaxs="i",yaxs="i",xlab="",ylab="",main="",cex.main=6)
rasterImage(plot2c,0,0,1,1)

plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",
     xaxs="i",yaxs="i",xlab="",ylab="",main="",cex.main=6)
rasterImage(plot3,0,0,1,1)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",
     xaxs="i",yaxs="i",xlab="",ylab="",main="",cex.main=6)
rasterImage(plot3c,0,0,1,1)

plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",
     xaxs="i",yaxs="i",xlab="",ylab="",main="",cex.main=6)
rasterImage(plot4,0,0,1,1)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",
     xaxs="i",yaxs="i",xlab="",ylab="",main="",cex.main=6)
rasterImage(plot4c,0,0,1,1)

plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",
     xaxs="i",yaxs="i",xlab="",ylab="",main="",cex.main=6)
rasterImage(plot5,0,0,1,1)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",
     xaxs="i",yaxs="i",xlab="",ylab="",main="",cex.main=6)
rasterImage(plot5c,0,0,1,1)

plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",
     xaxs="i",yaxs="i",xlab="",ylab="")

plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",
     xaxs="i",yaxs="i",xlab="",ylab="")
text(0.55,0.5,"Diagnosis",cex=6,font=2)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",
     xaxs="i",yaxs="i",xlab="",ylab="")
text(0.55,0.5,"Treatment",cex=6,font=2)

plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",
     xaxs="i",yaxs="i",xlab="",ylab="")
text(0.5,0.55,"Gynecology",cex=6,font=2,srt=90)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",
     xaxs="i",yaxs="i",xlab="",ylab="")
text(0.5,0.55,"Internal Medicine",cex=6,font=2,srt=90)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",
     xaxs="i",yaxs="i",xlab="",ylab="")
text(0.5,0.55,"Neurology",cex=6,font=2,srt=90)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",
     xaxs="i",yaxs="i",xlab="",ylab="")
text(0.5,0.55,"Pediatrics",cex=6,font=2,srt=90)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",
     xaxs="i",yaxs="i",xlab="",ylab="")
text(0.5,0.55,"Surgery",cex=6,font=2,srt=90)
dev.off()

