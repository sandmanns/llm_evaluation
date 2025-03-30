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

table1<-data.frame(Category=c(rep("Diagnosis",4),
                              rep("Treatment",3)),
                   Test=c("GPT-4o","DeepSeek-R1","DeepSeek-V3","Gem2FTE",
                          "GPT-4o","DeepSeek-R1","Gem2FTE"),
                   Median1=NA,Median2=NA,p=NA)

x<-c(10,5,7,8,11,6,9)


for(j in 1:length(x)){
  table1$Median1[j]<-paste0(median(input[1:110,x[j]]),
                            " [",quantile(input[1:110,x[j]])[2],
                            ";",quantile(input[1:110,x[j]])[4],"]")   
  table1$Median2[j]<-paste0(median(input[111:125,x[j]]),
                            " [",quantile(input[111:125,x[j]])[2],
                            ";",quantile(input[111:125,x[j]])[4],"]") 
  
 table1$p[j]<-round(wilcox.test(input[1:110,x[j]],input[111:125,x[j]],
                          paired = F,alternative = "greater")$p.value,digits=4)
   
}
  


write.table(table1,paste0(dir,"TableS2.txt"),sep="\t",quote=F,row.names = F)




