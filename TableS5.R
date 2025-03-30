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
                   Test=c("GPT-4o","DeepSeek-R1","DeepSeek-V3","Gemini-2.0",
                          "GPT-4o","DeepSeek-R1","Gemini-2.0"),
                   All=NA,Rare=NA,LessFrequent=NA,Frequent=NA)


########
##Diagnosis
table1$All[1]<-paste0(median(input$DiagnosisGPT4o),
                      " [",quantile(input$DiagnosisGPT4o)[2],
                      ";",quantile(input$DiagnosisGPT4o)[4],"]")

table1$Rare[1]<-paste0(median(input$DiagnosisGPT4o[input$Frequency=="Rare"]),
                      " [",quantile(input$DiagnosisGPT4o[input$Frequency=="Rare"])[2],
                      ";",quantile(input$DiagnosisGPT4o[input$Frequency=="Rare"])[4],"]")
table1$LessFrequent[1]<-paste0(median(input$DiagnosisGPT4o[input$Frequency=="Less Frequent"]),
                       " [",quantile(input$DiagnosisGPT4o[input$Frequency=="Less Frequent"])[2],
                       ";",quantile(input$DiagnosisGPT4o[input$Frequency=="Less Frequent"])[4],"]")
table1$Frequent[1]<-paste0(median(input$DiagnosisGPT4o[input$Frequency=="Frequent"]),
                       " [",quantile(input$DiagnosisGPT4o[input$Frequency=="Frequent"])[2],
                       ";",quantile(input$DiagnosisGPT4o[input$Frequency=="Frequent"])[4],"]")


table1$All[2]<-paste0(median(input$DiagnosisDeepR),
                      " [",quantile(input$DiagnosisDeepR)[2],
                      ";",quantile(input$DiagnosisDeepR)[4],"]")

table1$Rare[2]<-paste0(median(input$DiagnosisDeepR[input$Frequency=="Rare"]),
                       " [",quantile(input$DiagnosisDeepR[input$Frequency=="Rare"])[2],
                       ";",quantile(input$DiagnosisDeepR[input$Frequency=="Rare"])[4],"]")
table1$LessFrequent[2]<-paste0(median(input$DiagnosisDeepR[input$Frequency=="Less Frequent"]),
                               " [",quantile(input$DiagnosisDeepR[input$Frequency=="Less Frequent"])[2],
                               ";",quantile(input$DiagnosisDeepR[input$Frequency=="Less Frequent"])[4],"]")
table1$Frequent[2]<-paste0(median(input$DiagnosisDeepR[input$Frequency=="Frequent"]),
                           " [",quantile(input$DiagnosisDeepR[input$Frequency=="Frequent"])[2],
                           ";",quantile(input$DiagnosisDeepR[input$Frequency=="Frequent"])[4],"]")


table1$All[3]<-paste0(median(input$DiagnosisDeepV),
                      " [",quantile(input$DiagnosisDeepV)[2],
                      ";",quantile(input$DiagnosisDeepV)[4],"]")

table1$Rare[3]<-paste0(median(input$DiagnosisDeepV[input$Frequency=="Rare"]),
                       " [",quantile(input$DiagnosisDeepV[input$Frequency=="Rare"])[2],
                       ";",quantile(input$DiagnosisDeepV[input$Frequency=="Rare"])[4],"]")
table1$LessFrequent[3]<-paste0(median(input$DiagnosisDeepV[input$Frequency=="Less Frequent"]),
                               " [",quantile(input$DiagnosisDeepV[input$Frequency=="Less Frequent"])[2],
                               ";",quantile(input$DiagnosisDeepV[input$Frequency=="Less Frequent"])[4],"]")
table1$Frequent[3]<-paste0(median(input$DiagnosisDeepV[input$Frequency=="Frequent"]),
                           " [",quantile(input$DiagnosisDeepV[input$Frequency=="Frequent"])[2],
                           ";",quantile(input$DiagnosisDeepV[input$Frequency=="Frequent"])[4],"]")


table1$All[4]<-paste0(median(input$DiagnosisGemini),
                      " [",quantile(input$DiagnosisGemini)[2],
                      ";",quantile(input$DiagnosisGemini)[4],"]")

table1$Rare[4]<-paste0(median(input$DiagnosisGemini[input$Frequency=="Rare"]),
                       " [",quantile(input$DiagnosisGemini[input$Frequency=="Rare"])[2],
                       ";",quantile(input$DiagnosisGemini[input$Frequency=="Rare"])[4],"]")
table1$LessFrequent[4]<-paste0(median(input$DiagnosisGemini[input$Frequency=="Less Frequent"]),
                               " [",quantile(input$DiagnosisGemini[input$Frequency=="Less Frequent"])[2],
                               ";",quantile(input$DiagnosisGemini[input$Frequency=="Less Frequent"])[4],"]")
table1$Frequent[4]<-paste0(median(input$DiagnosisGemini[input$Frequency=="Frequent"]),
                           " [",quantile(input$DiagnosisGemini[input$Frequency=="Frequent"])[2],
                           ";",quantile(input$DiagnosisGemini[input$Frequency=="Frequent"])[4],"]")



######
##Treatment
table1$All[5]<-paste0(median(input$TreatmentGPT4o),
                      " [",quantile(input$TreatmentGPT4o)[2],
                      ";",quantile(input$TreatmentGPT4o)[4],"]")

table1$Rare[5]<-paste0(median(input$TreatmentGPT4o[input$Frequency=="Rare"]),
                       " [",quantile(input$TreatmentGPT4o[input$Frequency=="Rare"])[2],
                       ";",quantile(input$TreatmentGPT4o[input$Frequency=="Rare"])[4],"]")
table1$LessFrequent[5]<-paste0(median(input$TreatmentGPT4o[input$Frequency=="Less Frequent"]),
                               " [",quantile(input$TreatmentGPT4o[input$Frequency=="Less Frequent"])[2],
                               ";",quantile(input$TreatmentGPT4o[input$Frequency=="Less Frequent"])[4],"]")
table1$Frequent[5]<-paste0(median(input$TreatmentGPT4o[input$Frequency=="Frequent"]),
                           " [",quantile(input$TreatmentGPT4o[input$Frequency=="Frequent"])[2],
                           ";",quantile(input$TreatmentGPT4o[input$Frequency=="Frequent"])[4],"]")


table1$All[6]<-paste0(median(input$TreatmentDeepR),
                      " [",quantile(input$TreatmentDeepR)[2],
                      ";",quantile(input$TreatmentDeepR)[4],"]")

table1$Rare[6]<-paste0(median(input$TreatmentDeepR[input$Frequency=="Rare"]),
                       " [",quantile(input$TreatmentDeepR[input$Frequency=="Rare"])[2],
                       ";",quantile(input$TreatmentDeepR[input$Frequency=="Rare"])[4],"]")
table1$LessFrequent[6]<-paste0(median(input$TreatmentDeepR[input$Frequency=="Less Frequent"]),
                               " [",quantile(input$TreatmentDeepR[input$Frequency=="Less Frequent"])[2],
                               ";",quantile(input$TreatmentDeepR[input$Frequency=="Less Frequent"])[4],"]")
table1$Frequent[6]<-paste0(median(input$TreatmentDeepR[input$Frequency=="Frequent"]),
                           " [",quantile(input$TreatmentDeepR[input$Frequency=="Frequent"])[2],
                           ";",quantile(input$TreatmentDeepR[input$Frequency=="Frequent"])[4],"]")


table1$All[7]<-paste0(median(input$TreatmentGemini),
                      " [",quantile(input$TreatmentGemini)[2],
                      ";",quantile(input$TreatmentGemini)[4],"]")

table1$Rare[7]<-paste0(median(input$TreatmentGemini[input$Frequency=="Rare"]),
                       " [",quantile(input$TreatmentGemini[input$Frequency=="Rare"])[2],
                       ";",quantile(input$TreatmentGemini[input$Frequency=="Rare"])[4],"]")
table1$LessFrequent[7]<-paste0(median(input$TreatmentGemini[input$Frequency=="Less Frequent"]),
                               " [",quantile(input$TreatmentGemini[input$Frequency=="Less Frequent"])[2],
                               ";",quantile(input$TreatmentGemini[input$Frequency=="Less Frequent"])[4],"]")
table1$Frequent[7]<-paste0(median(input$TreatmentGemini[input$Frequency=="Frequent"]),
                           " [",quantile(input$TreatmentGemini[input$Frequency=="Frequent"])[2],
                           ";",quantile(input$TreatmentGemini[input$Frequency=="Frequent"])[4],"]")


write.table(table1,paste0(dir,"TableS5.txt"),sep="\t",quote=F,row.names = F)



