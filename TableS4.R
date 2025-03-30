library(openxlsx)
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
                   Gynecology=NA,InternalMedicine=NA,Neurolog=NA,Pediatrics=NA,Surgery=NA)

specialty<-c("Gynecology","Internal Medicine","Neurology",
             "Pediatrics","Surgery")
x<-c(10,5,7,8,11,6,9)

########
##Diagnosis
for(i in 1:5){
  for(j in 1:length(x)){
    table1[j,(2+i)]<-paste0(median(input[input$Clinical.specialty==specialty[i],x[j]]),
                             " [",quantile(input[input$Clinical.specialty==specialty[i],x[j]])[2],
                             ";",quantile(input[input$Clinical.specialty==specialty[i],x[j]])[4],"]")    
  }

}

write.table(table1,paste0(dir,"TableS4.txt"),sep="\t",quote=F,row.names = F)




