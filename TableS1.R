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

#breaks1<-seq(0.999,5.999)
breaks1<-seq(0.499,5.499)


##Diagnosis
temp1<-hist(x=input$DiagnosisGPT4o,breaks = breaks1,plot = F)
temp2<-hist(x=input$DiagnosisDeepR,breaks = breaks1,plot = F)
temp3<-hist(x=input$DiagnosisDeepV,breaks = breaks1,plot = F)
temp4<-hist(x=input$DiagnosisGemini,breaks = breaks1,plot = F)

temp<-round(matrix(c(temp1$counts,temp2$counts,temp3$counts,temp4$counts),
                                 byrow = T,nrow = 4)/125,digits = 2)

output1<-data.frame(temp)
rownames(output1)<-paste0("Diagnosis",c("GPT-4o","DeepSeek-R1","DeepSeek-V3","Gem2FTE"))
colnames(output1)<-c("[1;1.5)","[1.5;2.5)","[2.5;3.5)","[3.5;4.5)","[4.5;5.0]")


##Treatment
temp1<-hist(x=input$TreatmentGPT4o,breaks = breaks1,plot = F)
temp2<-hist(x=input$TreatmentDeepR,breaks = breaks1,plot = F)
temp3<-hist(x=input$TreatmentGemini,breaks = breaks1,plot = F)

temp<-round(matrix(c(temp1$counts,temp2$counts,temp3$counts),
                   byrow = T,nrow = 3)/125,digits=2)


output2<-data.frame(temp)
rownames(output2)<-paste0("Treatment",c("GPT-4o","DeepSeek-R1","Gem2FTE"))
colnames(output2)<-c("[1;1.5)","[1.5;2.5)","[2.5;3.5)","[3.5;4.5)","[4.5;5.0]")

output<-rbind(output1,output2)

write.table(output,paste0(dir,"TableS1.txt"),sep="\t",quote=F)






