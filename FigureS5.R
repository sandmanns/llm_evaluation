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


#############
##Diagnosis##
#############

##GPT-4o
png(paste0(dir,"FigureS5_Diagnosis_GPT4o.png"),width=800,height=800)
par(mar=c(4,7,1,1),mgp=c(4.5,1.5,0))
vioplot(x = as.matrix(cbind(input$DiagnosisGPT4o[input$Frequency=="Rare"])),
        xlim=c(0.5,3.5),
        col=c("lightskyblue1"),ylab="",
        drawRect=F,cex.lab=3,at=1,
        cex.main=3.5,cex.axis = 2.5,las=1,xaxt="n",ylim=c(0,5))
vioplot(x = as.matrix(input$DiagnosisGPT4o[input$Frequency=="Less Frequent"]),add=T,
        col=c("deepskyblue1"),drawRect=F,at=2)
vioplot(x = as.matrix(input$DiagnosisGPT4o[input$Frequency=="Frequent"]),add=T,
        col=c("royalblue4"),drawRect=F,at=3)
axis(1,at=c(1,2,3),labels=c("Rare","Less Frequent","Frequent"),cex.axis=2.8,tick=F)
text(x=0,y=2.4,paste0("Likert score"),xpd=T,font=2,cex=3,srt=90)
dev.off()


##DeepSeek-R1
png(paste0(dir,"FigureS5_Diagnosis_DeepSeekR1.png"),width=800,height=800)
par(mar=c(4,7,1,1),mgp=c(4.5,1.5,0))
vioplot(x = as.matrix(cbind(input$DiagnosisDeepR[input$Frequency=="Rare"])),
        xlim=c(0.5,3.5),
        col=c("lightskyblue1"),ylab="",
        drawRect=F,cex.lab=3,at=1,
        cex.main=3.5,cex.axis = 2.5,las=1,xaxt="n",ylim=c(0,5))
vioplot(x = as.matrix(input$DiagnosisDeepR[input$Frequency=="Less Frequent"]),add=T,
        col=c("deepskyblue1"),drawRect=F,at=2)
vioplot(x = as.matrix(input$DiagnosisDeepR[input$Frequency=="Frequent"]),add=T,
        col=c("royalblue4"),drawRect=F,at=3)
axis(1,at=c(1,2,3),labels=c("Rare","Less Frequent","Frequent"),cex.axis=2.8,tick=F)
text(x=0,y=2.4,paste0("Likert score"),xpd=T,font=2,cex=3,srt=90)
dev.off()


##DeepSeek-V3
png(paste0(dir,"FigureS5_Diagnosis_DeepSeekV3.png"),width=800,height=800)
par(mar=c(4,7,1,1),mgp=c(4.5,1.5,0))
vioplot(x = as.matrix(cbind(input$DiagnosisDeepV[input$Frequency=="Rare"])),
        xlim=c(0.5,3.5),
        col=c("lightskyblue1"),ylab="",
        drawRect=F,cex.lab=3,at=1,
        cex.main=3.5,cex.axis = 2.5,las=1,xaxt="n",ylim=c(0,5))
vioplot(x = as.matrix(input$DiagnosisDeepV[input$Frequency=="Less Frequent"]),add=T,
        col=c("deepskyblue1"),drawRect=F,at=2)
vioplot(x = as.matrix(input$DiagnosisDeepV[input$Frequency=="Frequent"]),add=T,
        col=c("royalblue4"),drawRect=F,at=3)
axis(1,at=c(1,2,3),labels=c("Rare","Less Frequent","Frequent"),cex.axis=2.8,tick=F)
text(x=0,y=2.4,paste0("Likert score"),xpd=T,font=2,cex=3,srt=90)
dev.off()


##Gem2FTE
png(paste0(dir,"FigureS5_Diagnosis_Gem2FTE.png"),width=800,height=800)
par(mar=c(4,7,1,1),mgp=c(4.5,1.5,0))
vioplot(x = as.matrix(cbind(input$DiagnosisGemini[input$Frequency=="Rare"])),
        xlim=c(0.5,3.5),
        col=c("lightskyblue1"),ylab="",
        drawRect=F,cex.lab=3,at=1,
        cex.main=3.5,cex.axis = 2.5,las=1,xaxt="n",ylim=c(0,5))
vioplot(x = as.matrix(input$DiagnosisGemini[input$Frequency=="Less Frequent"]),add=T,
        col=c("deepskyblue1"),drawRect=F,at=2)
vioplot(x = as.matrix(input$DiagnosisGemini[input$Frequency=="Frequent"]),add=T,
        col=c("royalblue4"),drawRect=F,at=3)
axis(1,at=c(1,2,3),labels=c("Rare","Less Frequent","Frequent"),cex.axis=2.8,tick=F)
text(x=0,y=2.4,paste0("Likert score"),xpd=T,font=2,cex=3,srt=90)
dev.off()




#############
##Treatment##
#############

##GPT-4o
png(paste0(dir,"FigureS5_Treatment_GPT4o.png"),width=800,height=800)
par(mar=c(4,7,1,1),mgp=c(4.5,1.5,0))
vioplot(x = as.matrix(cbind(input$TreatmentGPT4o[input$Frequency=="Rare"])),
        xlim=c(0.5,3.5),
        col=c("lightskyblue1"),ylab="",
        drawRect=F,cex.lab=3,at=1,
        cex.main=3.5,cex.axis = 2.5,las=1,xaxt="n",ylim=c(0,5))
vioplot(x = as.matrix(input$TreatmentGPT4o[input$Frequency=="Less Frequent"]),add=T,
        col=c("deepskyblue1"),drawRect=F,at=2)
vioplot(x = as.matrix(input$TreatmentGPT4o[input$Frequency=="Frequent"]),add=T,
        col=c("royalblue4"),drawRect=F,at=3)
axis(1,at=c(1,2,3),labels=c("Rare","Less Frequent","Frequent"),cex.axis=2.8,tick=F)
text(x=0,y=2.4,paste0("Likert score"),xpd=T,font=2,cex=3,srt=90)
dev.off()


##DeepSeek-R1
png(paste0(dir,"FigureS5_Treatment_DeepSeekR1.png"),width=800,height=800)
par(mar=c(4,7,1,1),mgp=c(4.5,1.5,0))
vioplot(x = as.matrix(cbind(input$TreatmentDeepR[input$Frequency=="Rare"])),
        xlim=c(0.5,3.5),
        col=c("lightskyblue1"),ylab="",
        drawRect=F,cex.lab=3,at=1,
        cex.main=3.5,cex.axis = 2.5,las=1,xaxt="n",ylim=c(0,5))
vioplot(x = as.matrix(input$TreatmentDeepR[input$Frequency=="Less Frequent"]),add=T,
        col=c("deepskyblue1"),drawRect=F,at=2)
vioplot(x = as.matrix(input$TreatmentDeepR[input$Frequency=="Frequent"]),add=T,
        col=c("royalblue4"),drawRect=F,at=3)
axis(1,at=c(1,2,3),labels=c("Rare","Less Frequent","Frequent"),cex.axis=2.8,tick=F)
text(x=0,y=2.4,paste0("Likert score"),xpd=T,font=2,cex=3,srt=90)
dev.off()


##Gem2FTE
png(paste0(dir,"FigureS5_Treatment_Gem2FTE.png"),width=800,height=800)
par(mar=c(4,7,1,1),mgp=c(4.5,1.5,0))
vioplot(x = as.matrix(cbind(input$TreatmentGemini[input$Frequency=="Rare"])),
        xlim=c(0.5,3.5),
        col=c("lightskyblue1"),ylab="",
        drawRect=F,cex.lab=3,at=1,
        cex.main=3.5,cex.axis = 2.5,las=1,xaxt="n",ylim=c(0,5))
vioplot(x = as.matrix(input$TreatmentGemini[input$Frequency=="Less Frequent"]),add=T,
        col=c("deepskyblue1"),drawRect=F,at=2)
vioplot(x = as.matrix(input$TreatmentGemini[input$Frequency=="Frequent"]),add=T,
        col=c("royalblue4"),drawRect=F,at=3)
axis(1,at=c(1,2,3),labels=c("Rare","Less Frequent","Frequent"),cex.axis=2.8,tick=F)
text(x=0,y=2.4,paste0("Likert score"),xpd=T,font=2,cex=3,srt=90)
dev.off()




###############
##Combine all##
###############

plot1<-readPNG(paste0(dir,"FigureS5_Diagnosis_GPT4o.png"))
plot2<-readPNG(paste0(dir,"FigureS5_Diagnosis_DeepSeekR1.png"))
plot3<-readPNG(paste0(dir,"FigureS5_Diagnosis_DeepSeekV3.png"))
plot4<-readPNG(paste0(dir,"FigureS5_Diagnosis_Gem2FTE.png"))
plot5<-readPNG(paste0(dir,"FigureS5_Treatment_GPT4o.png"))
plot6<-readPNG(paste0(dir,"FigureS5_Treatment_DeepSeekR1.png"))
plot7<-readPNG(paste0(dir,"FigureS5_Treatment_Gem2FTE.png"))


png(paste0(dir,"FigureS5.png"),width = 3300,height = 1700)
layout(matrix(c(1,rep(2,8),rep(3,8),rep(4,8),rep(5,8),
              rep(c(6,rep(7,8),rep(8,8),rep(9,8),rep(10,8)),8),
              rep(c(11,rep(12,8),rep(13,8),rep(14,8),rep(15,8)),8)),nrow = 17,byrow = T))
par(mar=rep(0,4))
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
text(0.55,0.5,"GPT-4o",font=2,cex=6)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
text(0.55,0.5,"DeepSeek-R1",font=2,cex=6)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
text(0.55,0.5,"Gem2FTE",font=2,cex=6)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
text(0.55,0.5,"DeepSeek-V3",font=2,cex=6)

plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
text(0.5,0.5,"Diagnosis",font=2,cex=6,srt=90)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
rasterImage(plot1,0,0,1,1)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
rasterImage(plot2,0,0,1,1)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
rasterImage(plot4,0,0,1,1)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
rasterImage(plot3,0,0,1,1)

plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
text(0.5,0.5,"Treatment",font=2,cex=6,srt=90)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
rasterImage(plot5,0,0,1,1)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
rasterImage(plot6,0,0,1,1)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
rasterImage(plot7,0,0,1,1)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
dev.off()
