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

breaks1<-seq(0.499,5.499)


##Diagnosis
temp1<-hist(x=input$DiagnosisGPT4o,breaks = breaks1,plot = F)
temp2<-hist(x=input$DiagnosisDeepR,breaks = breaks1,plot = F)
temp3<-hist(x=input$DiagnosisDeepV,breaks = breaks1,plot = F)
temp4<-hist(x=input$DiagnosisGemini,breaks = breaks1,plot = F)

temp<-matrix(c(temp1$counts,temp2$counts,temp3$counts,temp4$counts),byrow = T,nrow = 4)/125

png(paste0(dir,"Figure_S1a.png"),width=1000,height=700)
par(mar=c(7,9,4,1),mgp=c(5.5,1.5,0))
barplot(temp,beside = T,col=c("royalblue3","deepskyblue3","deepskyblue1","lightskyblue1"),
        xlab="Likert score",
        cex.lab=3,cex.main=3.5,cex.axis = 2.5,las=1,main="Diagnosis",
        ylim=c(0,1),yaxt="n",ylab = "Frequency",
        names.arg = c("[1;1.5)","[1.5;2.5)","[2.5;3.5)","[3.5;4.5)","[4.5;5.0]"),cex.names=2.5)
axis(2,at=seq(0.1,1,0.2),cex.axis=2.5,las=2,labels=NA)
axis(2,at=seq(0,1,0.2),cex.axis=2.5,las=2)
legend("topleft",legend=c("GPT-4o","DeepSeek-R1","DeepSeek-V3","Gem2FTE"),
       fill=c("royalblue3","deepskyblue3","deepskyblue1","lightskyblue1"),border = NA,cex=2.5)
abline(h=0)
dev.off()







##Treatment
temp1<-hist(x=input$TreatmentGPT4o,breaks = breaks1,plot = F)
temp2<-hist(x=input$TreatmentDeepR,breaks = breaks1,plot = F)
temp3<-hist(x=input$TreatmentGemini,breaks = breaks1,plot = F)

temp<-matrix(c(temp1$counts,temp2$counts,temp3$counts),byrow = T,nrow = 3)/125

png(paste0(dir,"Figure_S1b.png"),width=900,height=700)
par(mar=c(7,9,4,1),mgp=c(5.5,1.5,0))
barplot(temp,beside = T,col=c("royalblue3","deepskyblue3","lightskyblue1"),
        xlab="Likert score",
        cex.lab=3,cex.main=3.5,cex.axis = 2.5,las=1,main="Treatment",
        ylim=c(0,1),yaxt="n",ylab = "Frequency",
        names.arg = c("[1;1.5)","[1.5;2.5)","[2.5;3.5)","[3.5;4.5)","[4.5;5.0]"),cex.names=2.5)
axis(2,at=seq(0.1,1,0.2),cex.axis=2.5,las=2,labels=NA)
axis(2,at=seq(0,1,0.2),cex.axis=2.5,las=2)
abline(h=0)
dev.off()




###############
##Combine all##
###############

plot1<-readPNG(paste0(dir,"Figure_S1a.png"))
plot2<-readPNG(paste0(dir,"Figure_S1b.png"))

png(paste0(dir,"Figure_S1.png"),width = 1900,height = 700)
layout(matrix(c(rep(1,10),rep(2,9)),nrow = 1,byrow = T))
par(mar=c(0,0,2,0))
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
rasterImage(plot1,0,0,1,1)
text(0.01,1.015,"A)",xpd=T,font=2,cex=6)

plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",bty="n")
rasterImage(plot2,0,0,1,1)
text(0.01,1.03,"B)",xpd=T,font=2,cex=6)
dev.off()


