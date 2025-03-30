#########################
##Diagnosis & Treatment##
#########################

##4 Tests Diagnosis
##3 Tests Treatment

power.calculation <- data.frame(SampleSize = seq(75,145),
                                PowerDiagnosis=0,
                                PowerTreatment=0)

gpt4o <-     c(0.00,0.00,0.00,0.30,0.70)
deepseekR1 <-c(0.00,0.00,0.10,0.30,0.60)
deepseekV3 <-c(0.00,0.05,0.25,0.20,0.50)
gemini <-    c(0.01,0.14,0.30,0.15,0.40)

for(i in 1:length(power.calculation[,1])){
  message("Sample Size ",power.calculation$SampleSize[i])
  p.output4<-0
  p.output3<-0
  sample_size<-power.calculation$SampleSize[i]
  for(j in 1:10000){
    ##GPT-4o vs DeepSeek-R1
    temp <- wilcox.test(sample(seq(1:5), size = sample_size, 
                               replace = T,
                               prob = gpt4o),
                        sample(seq(1:5), size = sample_size,
                               replace = T,
                               prob = deepseekR1),
                        paired = T, alternative = "g")$p.value
    p.output4 <- p.output4 + as.numeric(temp<=0.05/4) 
    p.output3 <- p.output3 + as.numeric(temp<=0.05/3)  
    
    ##GPT-4o vs Gem2FTE
    temp <- wilcox.test(sample(seq(1:5), size = sample_size, 
                               replace = T,
                               prob = gpt4o),
                        sample(seq(1:5), size = sample_size,
                               replace = T,
                               prob = gemini),
                        paired = T, alternative = "g")$p.value
    p.output4 <- p.output4 + as.numeric(temp<=0.05/4)  
    p.output3 <- p.output3 + as.numeric(temp<=0.05/3)  
    
    ##DeepSeek-R1 vs Gem2FTE
    temp <- wilcox.test(sample(seq(1:5), size = sample_size, 
                               replace = T,
                               prob = deepseekR1),
                        sample(seq(1:5), size = sample_size,
                               replace = T,
                               prob = gemini),
                        paired = T, alternative = "g")$p.value
    p.output4 <- p.output4 + as.numeric(temp<=0.05/4)  
    p.output3 <- p.output3 + as.numeric(temp<=0.05/3)  
    
    ##DeepSeek-R1 vs DeepSeek-V3
    temp <- wilcox.test(sample(seq(1:5), size = sample_size, 
                               replace = T,
                               prob = deepseekR1),
                        sample(seq(1:5), size = sample_size,
                               replace = T,
                               prob = deepseekV3),
                        paired = T, alternative = "g")$p.value
    p.output4 <- p.output4 + as.numeric(temp<=0.05/4)  
  }
  
  power.calculation$PowerDiagnosis[i] <- p.output4/40000
  power.calculation$PowerTreatment[i] <- p.output3/30000
}




#############
##Figure S7##
#############
dir<-"C:/Paper/Supplemental_Figures/"
png(filename=paste0(dir,"FigureS7.png"),width=1500,height=700)
par(mar=c(7,9,2,1),mgp=c(5.5,1.5,0))
plot(x=power.calculation$SampleSize,y=power.calculation$PowerDiagnosis,
     xlab="sample size",ylab="power",col="black",
     type="b",lwd=5,cex=2,cex.lab=1.5,ylim=c(0.65,1),cex.axis=2,cex.lab=3,las=1)
points(x=power.calculation$SampleSize,y=power.calculation$PowerTreatment,col="red",
       type="b",lwd=5,cex=2)

points(x=c(125,125),y=c(0.5,0.915),type="l",lty=2,col="grey60",lwd=4)
points(x=c(65,125),y=c(0.915,0.915),type="l",lty=2,col="grey60",lwd=4)
points(x=c(65,125),y=c(0.892,0.892),type="l",lty=2,col="grey60",lwd=4)

legend("bottomright",c("diagnosis (4 tests)","treatment (3 tests)"),fill=c("black","red"),
       bty="n",cex=2)
dev.off()


