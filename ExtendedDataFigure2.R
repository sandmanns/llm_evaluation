library(openxlsx)
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

breaks2<-seq(0.99,5.49,0.5)



pdf(paste0(dir,"ExtendedDataFigure2.pdf"),width = 3.46457,height = 4.4,pointsize = 7)
layout(matrix(c(1:8),ncol = 2,byrow = F))

##Diagnosis
par(mar=c(2.5,4.5,1.5,1),mgp=c(1.5,0.6,0))
hist(x=input$DiagnosisGPT4o,breaks = breaks2,plot = T,col="royalblue3",border=NA,
     xaxt="n",xlab="Likert score",main="Diagnosis",yaxt="n",ylim=c(0,125),xaxs="i",
     yaxs="i",ylab="",xlim=c(1,5.5),cex.lab=0.9,cex.main=1)
axis(1,at=seq(1.25,5.25,0.5),labels=NA,lwd = 0.7,lwd.ticks = 0.7,cex.axis=0.8)
axis(1,at=seq(1.25,5.25,1),labels=seq(1,5,1),lwd = 0.7,lwd.ticks = 0.7,cex.axis=0.8)
#mean(input$DiagnosisGPT4o)
points(x=c(4.76,4.76)+0.25,y=c(0,125),type="l",lty=2,col="red",lwd=1.3)
axis(2,at=seq(0,125,length.out=5),labels = paste0(seq(0,100,25),"%"),
     las=2,lwd = 0.7,lwd.ticks = 0.7,cex.axis=0.9,font=2)
text(x=1.5,y=90,pos=4,"mean=4.76",cex=0.9,font=2)
text(x=0.05,y=62.5,"GPT-4o",xpd=T,font=2,cex=1,srt=90)


par(mar=c(2.5,4.5,1.5,1),mgp=c(1.5,0.6,0))
hist(x=input$DiagnosisDeepR,breaks = breaks2,plot = T,col="deepskyblue3",border=NA,
     xaxt="n",xlab="Likert score",main="",yaxt="n",ylim=c(0,125),xaxs="i",yaxs="i",ylab="",xlim=c(1,5.5),
     cex.lab=0.9)
axis(1,at=seq(1.25,5.25,0.5),labels=NA,lwd = 0.7,lwd.ticks = 0.7,cex.axis=0.8)
axis(1,at=seq(1.25,5.25,1),labels=seq(1,5,1),lwd = 0.7,lwd.ticks = 0.7,cex.axis=0.8)
#mean(input$DiagnosisDeepR)
points(x=c(4.70,4.70)+0.25,y=c(0,125),type="l",lty=2,col="red",lwd=1.3)
axis(2,at=seq(0,125,length.out=5),labels = paste0(seq(0,100,25),"%"),
     las=2,lwd = 0.7,lwd.ticks = 0.7,cex.axis=0.9,font=2)
text(x=1.5,y=90,pos=4,"mean=4.70",cex=0.9,font=2)
text(x=0.05,y=62.5,"DeepSeek-R1",xpd=T,font=2,cex=1,srt=90)


par(mar=c(2.5,4.5,1.5,1),mgp=c(1.5,0.6,0))
hist(x=input$DiagnosisDeepV,breaks = breaks2,plot = T,col="deepskyblue1",border=NA,
     xaxt="n",xlab="Likert score",main="",yaxt="n",ylim=c(0,125),xaxs="i",yaxs="i",ylab="",xlim=c(1,5.5),
     cex.lab=0.9)
axis(1,at=seq(1.25,5.25,0.5),labels=NA,lwd = 0.7,lwd.ticks = 0.7,cex.axis=0.8)
axis(1,at=seq(1.25,5.25,1),labels=seq(1,5,1),lwd = 0.7,lwd.ticks = 0.7,cex.axis=0.8)
#mean(input$DiagnosisDeepV)
points(x=c(4.69,4.69)+0.25,y=c(0,125),type="l",lty=2,col="red",lwd=1.3)
axis(2,at=seq(0,125,length.out=5),labels = paste0(seq(0,100,25),"%"),
     las=2,lwd = 0.7,lwd.ticks = 0.7,cex.axis=0.9,font=2)
text(x=1.5,y=90,pos=4,"mean=4.69",cex=0.9,font=2)
text(x=0.05,y=62.5,"Gem2FTE",xpd=T,font=2,cex=1,srt=90)


par(mar=c(2.5,4.5,1.5,1),mgp=c(1.5,0.6,0))
hist(x=input$DiagnosisGemini,breaks = breaks2,plot = T,col="lightskyblue1",border=NA,
     xaxt="n",xlab="Likert score",main="",yaxt="n",ylim=c(0,125),xaxs="i",yaxs="i",ylab="",xlim=c(1,5.5),
     cex.lab=0.9)
axis(1,at=seq(1.25,5.25,0.5),labels=NA,lwd = 0.7,lwd.ticks = 0.7,cex.axis=0.8)
axis(1,at=seq(1.25,5.25,1),labels=seq(1,5,1),lwd = 0.7,lwd.ticks = 0.7,cex.axis=0.8)
#mean(input$DiagnosisGemini)
points(x=c(4.43,4.43)+0.25,y=c(0,125),type="l",lty=2,col="red",lwd=1.3)
axis(2,at=seq(0,125,length.out=5),labels = paste0(seq(0,100,25),"%"),
     las=2,lwd = 0.7,lwd.ticks = 0.7,cex.axis=0.9,font=2)
text(x=1.5,y=90,pos=4,"mean=4.43",cex=0.9,font=2)
text(x=0.05,y=62.5,"DeepSeek-V3",xpd=T,font=2,cex=1,srt=90)


##Treatment
par(mar=c(2.5,4.5,1.5,1),mgp=c(1.5,0.6,0))
hist(x=input$TreatmentGPT4o,breaks = breaks2,plot = T,col="royalblue3",border=NA,
     xaxt="n",xlab="Likert score",main="Treatment",yaxt="n",ylim=c(0,125),xaxs="i",
     yaxs="i",ylab="",xlim=c(1,5.5),cex.lab=0.9,cex.main=1)
axis(1,at=seq(1.25,5.25,0.5),labels=NA,lwd = 0.7,lwd.ticks = 0.7,cex.axis=0.8)
axis(1,at=seq(1.25,5.25,1),labels=seq(1,5,1),lwd = 0.7,lwd.ticks = 0.7,cex.axis=0.8)
#mean(input$TreatmentGPT4o)
points(x=c(4.55,4.55)+0.25,y=c(0,125),type="l",lty=2,col="red",lwd=1.3)
axis(2,at=seq(0,125,length.out=5),labels = paste0(seq(0,100,25),"%"),
     las=2,lwd = 0.7,lwd.ticks = 0.7,cex.axis=0.9,font=2)
text(x=1.5,y=90,pos=4,"mean=4.55",cex=0.9,font=2)


par(mar=c(2.5,4.5,1.5,1),mgp=c(1.5,0.6,0))
hist(x=input$TreatmentDeepR,breaks = breaks2,plot = T,col="deepskyblue3",border=NA,
     xaxt="n",xlab="Likert score",main="",yaxt="n",ylim=c(0,125),xaxs="i",yaxs="i",ylab="",xlim=c(1,5.5),
     cex.lab=0.9)
axis(1,at=seq(1.25,5.25,0.5),labels=NA,lwd = 0.7,lwd.ticks = 0.7,cex.axis=0.8)
axis(1,at=seq(1.25,5.25,1),labels=seq(1,5,1),lwd = 0.7,lwd.ticks = 0.7,cex.axis=0.8)
#mean(input$TreatmentDeepR)
points(x=c(4.48,4.48)+0.25,y=c(0,125),type="l",lty=2,col="red",lwd=1.3)
axis(2,at=seq(0,125,length.out=5),labels = paste0(seq(0,100,25),"%"),
     las=2,lwd = 0.7,lwd.ticks = 0.7,cex.axis=0.9,font=2)
text(x=1.5,y=90,pos=4,"mean=4.48",cex=0.9,font=2)


par(mar=c(2.5,4.5,1.5,1),mgp=c(1.5,0.6,0))
hist(x=input$TreatmentGemini,breaks = breaks2,plot = T,col="lightskyblue1",border=NA,
     xaxt="n",xlab="Likert score",main="",yaxt="n",ylim=c(0,125),xaxs="i",yaxs="i",ylab="",xlim=c(1,5.5),
     cex.lab=0.9)
axis(1,at=seq(1.25,5.25,0.5),labels=NA,lwd = 0.7,lwd.ticks = 0.7,cex.axis=0.8)
axis(1,at=seq(1.25,5.25,1),labels=seq(1,5,1),lwd = 0.7,lwd.ticks = 0.7,cex.axis=0.8)
#mean(input$TreatmentGemini)
points(x=c(4.31,4.31)+0.25,y=c(0,125),type="l",lty=2,col="red",lwd=1.3)
axis(2,at=seq(0,125,length.out=5),labels = paste0(seq(0,100,25),"%"),
     las=2,lwd = 0.7,lwd.ticks = 0.7,cex.axis=0.9,font=2)
text(x=1.5,y=90,pos=4,"mean=4.31",cex=0.9,font=2)

dev.off()









