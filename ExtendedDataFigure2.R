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

breaks2<-seq(0.99,5.49,0.5)


##Diagnosis
png(paste0(dir,"ExtendedDataFigure_2a.png"),width=1000,height=700)
par(mar=c(8,8,1,1),mgp=c(5.5,2,0))
hist(x=input$DiagnosisGPT4o,breaks = breaks2,plot = T,col="royalblue3",border=NA,
           xaxt="n",xlab="Likert score",main="",yaxt="n",ylim=c(0,125),xaxs="i",yaxs="i",ylab="",xlim=c(1,5.5),
     cex.lab=4)
axis(1,at=seq(1.25,5.25,0.5),labels=NA,lwd = 3,lwd.ticks = 3,cex.axis=3)
axis(1,at=seq(1.25,5.25,1),labels=seq(1,5,1),lwd = 3,lwd.ticks = 3,cex.axis=3)
#mean(input$DiagnosisGPT4o)
points(x=c(4.76,4.76)+0.25,y=c(0,125),type="l",lty=2,col="red",lwd=8)
axis(2,at=seq(0,125,length.out=5),labels = paste0(seq(0,100,25),"%"),
     las=2,lwd = 3,lwd.ticks = 3,cex.axis=3,font=2)
text(x=1.5,y=90,pos=4,"mean=4.76",cex=4,font=2)
dev.off()


png(paste0(dir,"ExtendedDataFigure_2b.png"),width=1000,height=700)
par(mar=c(8,8,1,1),mgp=c(5.5,2,0))
hist(x=input$DiagnosisDeepR,breaks = breaks2,plot = T,col="deepskyblue3",border=NA,
     xaxt="n",xlab="Likert score",main="",yaxt="n",ylim=c(0,125),xaxs="i",yaxs="i",ylab="",xlim=c(1,5.5),
     cex.lab=4)
axis(1,at=seq(1.25,5.25,0.5),labels=NA,lwd = 3,lwd.ticks = 3,cex.axis=3)
axis(1,at=seq(1.25,5.25,1),labels=seq(1,5,1),lwd = 3,lwd.ticks = 3,cex.axis=3)
#mean(input$DiagnosisDeepR)
points(x=c(4.70,4.70)+0.25,y=c(0,125),type="l",lty=2,col="red",lwd=8)
axis(2,at=seq(0,125,length.out=5),labels = paste0(seq(0,100,25),"%"),
     las=2,lwd = 3,lwd.ticks = 3,cex.axis=3,font=2)
text(x=1.5,y=90,pos=4,"mean=4.70",cex=4,font=2)
dev.off()


png(paste0(dir,"ExtendedDataFigure_2c.png"),width=1000,height=700)
par(mar=c(8,8,1,1),mgp=c(5.5,2,0))
hist(x=input$DiagnosisDeepV,breaks = breaks2,plot = T,col="deepskyblue1",border=NA,
     xaxt="n",xlab="Likert score",main="",yaxt="n",ylim=c(0,125),xaxs="i",yaxs="i",ylab="",xlim=c(1,5.5),
     cex.lab=4)
axis(1,at=seq(1.25,5.25,0.5),labels=NA,lwd = 3,lwd.ticks = 3,cex.axis=3)
axis(1,at=seq(1.25,5.25,1),labels=seq(1,5,1),lwd = 3,lwd.ticks = 3,cex.axis=3)
#mean(input$DiagnosisDeepV)
points(x=c(4.69,4.69)+0.25,y=c(0,125),type="l",lty=2,col="red",lwd=8)
axis(2,at=seq(0,125,length.out=5),labels = paste0(seq(0,100,25),"%"),
     las=2,lwd = 3,lwd.ticks = 3,cex.axis=3,font=2)
text(x=1.5,y=90,pos=4,"mean=4.69",cex=4,font=2)
dev.off()


png(paste0(dir,"ExtendedDataFigure_2d.png"),width=1000,height=700)
par(mar=c(8,8,1,1),mgp=c(5.5,2,0))
hist(x=input$DiagnosisGemini,breaks = breaks2,plot = T,col="lightskyblue1",border=NA,
     xaxt="n",xlab="Likert score",main="",yaxt="n",ylim=c(0,125),xaxs="i",yaxs="i",ylab="",xlim=c(1,5.5),
     cex.lab=4)
axis(1,at=seq(1.25,5.25,0.5),labels=NA,lwd = 3,lwd.ticks = 3,cex.axis=3)
axis(1,at=seq(1.25,5.25,1),labels=seq(1,5,1),lwd = 3,lwd.ticks = 3,cex.axis=3)
#mean(input$DiagnosisGemini)
points(x=c(4.43,4.43)+0.25,y=c(0,125),type="l",lty=2,col="red",lwd=8)
axis(2,at=seq(0,125,length.out=5),labels = paste0(seq(0,100,25),"%"),
     las=2,lwd = 3,lwd.ticks = 3,cex.axis=3,font=2)
text(x=1.5,y=90,pos=4,"mean=4.43",cex=4,font=2)
dev.off()



##Treatment
png(paste0(dir,"ExtendedDataFigure_2e.png"),width=1000,height=700)
par(mar=c(8,8,1,1),mgp=c(5.5,2,0))
hist(x=input$TreatmentGPT4o,breaks = breaks2,plot = T,col="royalblue3",border=NA,
     xaxt="n",xlab="Likert score",main="",yaxt="n",ylim=c(0,125),xaxs="i",yaxs="i",ylab="",xlim=c(1,5.5),
     cex.lab=4)
axis(1,at=seq(1.25,5.25,0.5),labels=NA,lwd = 3,lwd.ticks = 3,cex.axis=3)
axis(1,at=seq(1.25,5.25,1),labels=seq(1,5,1),lwd = 3,lwd.ticks = 3,cex.axis=3)
#mean(input$TreatmentGPT4o)
points(x=c(4.55,4.55)+0.25,y=c(0,125),type="l",lty=2,col="red",lwd=8)
axis(2,at=seq(0,125,length.out=5),labels = paste0(seq(0,100,25),"%"),
     las=2,lwd = 3,lwd.ticks = 3,cex.axis=3,font=2)
text(x=1.5,y=90,pos=4,"mean=4.55",cex=4,font=2)
dev.off()


png(paste0(dir,"ExtendedDataFigure_2f.png"),width=1000,height=700)
par(mar=c(8,8,1,1),mgp=c(5.5,2,0))
hist(x=input$TreatmentDeepR,breaks = breaks2,plot = T,col="deepskyblue3",border=NA,
     xaxt="n",xlab="Likert score",main="",yaxt="n",ylim=c(0,125),xaxs="i",yaxs="i",ylab="",xlim=c(1,5.5),
     cex.lab=4)
axis(1,at=seq(1.25,5.25,0.5),labels=NA,lwd = 3,lwd.ticks = 3,cex.axis=3)
axis(1,at=seq(1.25,5.25,1),labels=seq(1,5,1),lwd = 3,lwd.ticks = 3,cex.axis=3)
#mean(input$TreatmentDeepR)
points(x=c(4.48,4.48)+0.25,y=c(0,125),type="l",lty=2,col="red",lwd=8)
axis(2,at=seq(0,125,length.out=5),labels = paste0(seq(0,100,25),"%"),
     las=2,lwd = 3,lwd.ticks = 3,cex.axis=3,font=2)
text(x=1.5,y=90,pos=4,"mean=4.48",cex=4,font=2)
dev.off()

png(paste0(dir,"ExtendedDataFigure_2g.png"),width=1000,height=700)
par(mar=c(8,8,1,1),mgp=c(5.5,2,0))
hist(x=input$TreatmentGemini,breaks = breaks2,plot = T,col="lightskyblue1",border=NA,
     xaxt="n",xlab="Likert score",main="",yaxt="n",ylim=c(0,125),xaxs="i",yaxs="i",ylab="",xlim=c(1,5.5),
     cex.lab=4)
axis(1,at=seq(1.25,5.25,0.5),labels=NA,lwd = 3,lwd.ticks = 3,cex.axis=3)
axis(1,at=seq(1.25,5.25,1),labels=seq(1,5,1),lwd = 3,lwd.ticks = 3,cex.axis=3)
#mean(input$TreatmentGemini)
points(x=c(4.31,4.31)+0.25,y=c(0,125),type="l",lty=2,col="red",lwd=8)
axis(2,at=seq(0,125,length.out=5),labels = paste0(seq(0,100,25),"%"),
     las=2,lwd = 3,lwd.ticks = 3,cex.axis=3,font=2)
text(x=1.5,y=90,pos=4,"mean=4.31",cex=4,font=2)
dev.off()




###############
##Combine all##
###############

plot1<-readPNG(paste0(dir,"ExtendedDataFigure_2a.png"))
plot2<-readPNG(paste0(dir,"ExtendedDataFigure_2b.png"))
plot3<-readPNG(paste0(dir,"ExtendedDataFigure_2c.png"))
plot4<-readPNG(paste0(dir,"ExtendedDataFigure_2d.png"))

plot5<-readPNG(paste0(dir,"ExtendedDataFigure_2e.png"))
plot6<-readPNG(paste0(dir,"ExtendedDataFigure_2f.png"))
plot7<-readPNG(paste0(dir,"ExtendedDataFigure_2g.png"))

png(paste0(dir,"ExtendedDataFigure_2.png"),width = 2100,height = 2900)
layout(matrix(c(9,rep(10,10),rep(11,10),
                rep(c(12,rep(1,10),rep(5,10)),7),
                rep(c(13,rep(2,10),rep(6,10)),7),
                rep(c(14,rep(3,10),rep(7,10)),7),
                rep(c(15,rep(4,10),rep(8,10)),7)),ncol = 21,byrow = T))

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
text(0.55,0.5,"Diagnosis",cex=10,font=2)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",
     xaxs="i",yaxs="i",xlab="",ylab="")
text(0.55,0.5,"Treatment",cex=10,font=2)

plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",
     xaxs="i",yaxs="i",xlab="",ylab="")
text(0.55,0.5,"GPT-4o",cex=8,font=2,srt=90)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",
     xaxs="i",yaxs="i",xlab="",ylab="")
text(0.55,0.5,"DeepSeek-R1",cex=8,font=2,srt=90)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",
     xaxs="i",yaxs="i",xlab="",ylab="")
text(0.5,0.55,"Gem2FTE",cex=8,font=2,srt=90)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",
     xaxs="i",yaxs="i",xlab="",ylab="")
text(0.5,0.55,"DeepSeek-V3",cex=8,font=2,srt=90)
dev.off()









