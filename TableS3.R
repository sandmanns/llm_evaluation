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


table1<-data.frame(Category=c(rep("Diagnosis",18),
                              rep("Treatment",9)),
                   x=c("GPT-4o","GPT-4o","DeepSeek-R1","DeepSeek-R1",
                       "GPT-4o","GPT-4o","GPT-4o","GPT-4o","DeepSeek-R1",
                       "DeepSeek-R1","DeepSeek-R1","DeepSeek-V3",
                       "DeepSeek-V3","DeepSeek-V3","DeepSeek-V3",
                       "Gemini-2.0 Flash Thinking",
                       "Gemini-2.0 Flash Thinking",
                       "Gemini-2.0 Flash Thinking",
                       "GPT-4o","GPT-4o","DeepSeek-R1","GPT-4o",
                       "GPT-4o","DeepSeek-R1","DeepSeek-R1",
                       "Gemini-2.0 Flash Thinking",
                       "Gemini-2.0 Flash Thinking"),
                   y=c("DeepSeek-R1","Gemini-2.0 Flash Thinking",
                       "Gemini-2.0 Flash Thinking","DeepSeek-V3",
                       "DeepSeek-V3","GPT-4","GPT-3.5","Google",
                       "GPT-4","GPT-3.5","Google",
                       "Gemini-2.0 Flash Thinking","GPT-4","GPT-3.5",
                       "Google","GPT-4","GPT-3.5","Google",
                       "DeepSeek-R1","Gemini-2.0 Flash Thinking",
                       "Gemini-2.0 Flash Thinking","GPT-4","GPT-3.5",
                       "GPT-4","GPT-3.5","GPT-4",
                       "GPT-3.5"),
                   p=NA,pAdj=NA)


x<-c(10,10,5,5,10,10,10,10,5,5,5,7,7,7,7,8,8,8,
     11,11,6,11,11,6,6,9,9)
y<-c(5,8,8,7,7,14,12,16,14,12,16,8,14,12,16,14,12,16,
     6,9,9,15,13,15,13,15,13)

for(i in 1:27){
  table1$p[i]<-wilcox.test(input[,x[i]],input[,y[i]],
                           paired = T,alternative = "greater")$p.value
}

table1$pAdj[1:4]<-apply(as.data.frame(table1$p[1:4]*4),1,function(x){min(x,1)})
table1$pAdj[19:21]<-apply(as.data.frame(table1$p[19:21]*3),1,function(x){min(x,1)})


table1$p<-ifelse(round(table1$p,digits=4)>0,round(table1$p,digits=4),table1$p)
table1$pAdj<-ifelse(round(table1$pAdj,digits=4)>0,round(table1$pAdj,digits=4),table1$pAdj)

write.table(table1,paste0(dir,"TableS3.txt"),sep="\t",quote=F,row.names = F)
