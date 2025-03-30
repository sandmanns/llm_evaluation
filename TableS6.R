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


table1<-data.frame(Category=c(rep("Diagnosis",12),
                              rep("Treatment",9)),
                   Group=c(rep("GPT-4o",3),rep("DeepSeek-R1",3),
                           rep("DeepSeek-V3",3),rep("Gemini-2.0 Flash Thinking",3),
                           rep("GPT-4o",3),rep("DeepSeek-R1",3),
                           rep("Gemini-2.0 Flash Thinking",3)),
                   x=rep(c("Rare","Rare","Less Frequent"),7),
                   y=rep(c("Less Frequent","Frequent","Frequent"),7),
                   p=NA)


group<-c(10,10,10,5,5,5,7,7,7,8,8,8,
         11,11,11,6,6,6,9,9,9)

for(i in 1:21){
  table1$p[i]<-wilcox.test(input[input$Frequency==table1$x[i],group[i]],
                           input[input$Frequency==table1$y[i],group[i]],
                           paired = F,alternative = "less")$p.value
}

table1$p<-ifelse(round(table1$p,digits=4)>0,round(table1$p,digits=4),table1$p)

write.table(table1,paste0(dir,"TableS6.txt"),sep="\t",quote=F,row.names = F)



