install.packages("BBmisc")

library(BBmisc)

PRS_results<-read.delim("VB_GWAS.Unknown_Removed.All.sum.best", sep=" ")

Status<-read.delim("DPCon_dbGap_FEB2023.PHENO_FILE.txt")

PRS_Results_Merged_Status<-merge(PRS_results,Status,by="IID")

All <- subset(PRS_Results_Merged_Status, PRS_Results_Merged_Status$In_Regression == "Yes")

#Females_only$PRSZ<- normalize(Females_only$PRS, method = "standardize")

All_prs_case <- subset(All, All$PHENO=="2")

All_prs_control <- subset(All, All$PHENO=="1")

All_control_mean=mean(All_prs_control$PRS)
All_control_sd=sd(All_prs_control$PRS)

All_prs_case$PRSZ<-((All_prs_case$PRS - All_control_mean)/All_control_sd)
All_prs_control$PRSZ<-((All_prs_control$PRS - All_control_mean)/All_control_sd)

#hist(prs_control$PRSZ, breaks=25, main="PRSice2 Sum Female Only Controls BCC PRS", col="white", freq=FALSE, xlim=c(-4,4),
    # ylim=c(0,0.8))
#hist(prs_case$PRSZ, breaks=10, main="PRSice2 Sum Female Only Cases BCC PRS", col="light grey", freq=FALSE, xlim=c(-4,4),
     #ylim=c(0,0.8))


case_col <- rgb(255, 0, 0, max=255, alpha=140) 
  
control_col <- rgb(0, 0, 255, max=255, alpha=140) 

overlap_col <- rgb(100, 0, 255, max=255, alpha=200)

casehist<-hist(All_prs_case$PRSZ, breaks=10, main="Male Pubertal Hallmarks PRS Cases vs Controls", xlab = "Polygenic Risk Score", col=case_col, freq=FALSE, xlim=c(-4,4),
     ylim=c(0,0.8))

controlhist<-hist(All_prs_control$PRSZ, breaks=15, freq=FALSE, col = control_col, add=TRUE)

legend("topright", legend=c("Cases","Controls","Overlap"), col=c(case_col, control_col,overlap_col), pt.cex=2, pch=15)



#Females 

Female_PRS_results<-read.delim("VB_GWAS.Unknown_Removed.FEMALES_ONLY.sum.best", sep=" ")

Status<-read.delim("DPCon_dbGap_FEB2023.PHENO_FILE.txt")

Female_PRS_Results_Merged_Status<-merge(Female_PRS_results,Status,by="IID")

Females_only <- subset(Female_PRS_Results_Merged_Status, Female_PRS_Results_Merged_Status$In_Regression == "Yes")

#Females_only$PRSZ<- normalize(Females_only$PRS, method = "standardize")

Female_prs_case <- subset(Females_only, Females_only$PHENO=="2")

Female_prs_control <- subset(Females_only, Females_only$PHENO=="1")

Female_control_mean=mean(Female_prs_control$PRS)
Female_control_sd=sd(Female_prs_control$PRS)

Female_prs_case$PRSZ<-((Female_prs_case$PRS - Female_control_mean)/Female_control_sd)
Female_prs_control$PRSZ<-((Female_prs_control$PRS - Female_control_mean)/Female_control_sd)

#hist(prs_control$PRSZ, breaks=25, main="PRSice2 Sum Female Only Controls BCC PRS", col="white", freq=FALSE, xlim=c(-4,4),
# ylim=c(0,0.8))
#hist(prs_case$PRSZ, breaks=10, main="PRSice2 Sum Female Only Cases BCC PRS", col="light grey", freq=FALSE, xlim=c(-4,4),
#ylim=c(0,0.8))

summary(Female_prs_case$PRSZ)

case_col <- rgb(255, 0, 0, max=255, alpha=140) 

control_col <- rgb(0, 0, 255, max=255, alpha=140) 

overlap_col <- rgb(100, 0, 255, max=255, alpha=200)

casehist<-hist(Female_prs_case$PRSZ, breaks=10, main="Male Pubertal Hallmarks PRS Female Cases vs Controls", xlab = "Polygenic Risk Score", col=case_col, freq=FALSE, xlim=c(-4,4),
               ylim=c(0,0.8))

controlhist<-hist(Female_prs_control$PRSZ, breaks=15, freq=FALSE, col = control_col, add=TRUE)

legend("topright", legend=c("Cases","Controls","Overlap"), col=c(case_col, control_col,overlap_col), pt.cex=2, pch=15)


#Males 

Male_PRS_results<-read.delim("VB_GWAS.Unknown_Removed.MALES_ONLY.sum.best", sep=" ")

Status<-read.delim("DPCon_dbGap_FEB2023.PHENO_FILE.txt")

Male_PRS_Results_Merged_Status<-merge(Male_PRS_results,Status,by="IID")

males_only <- subset(Male_PRS_Results_Merged_Status, Male_PRS_Results_Merged_Status$In_Regression == "Yes")

#Females_only$PRSZ<- normalize(Females_only$PRS, method = "standardize")

male_prs_case <- subset(males_only, males_only$PHENO=="2")

male_prs_control <- subset(males_only, males_only$PHENO=="1")

male_control_mean=mean(male_prs_control$PRS)
male_control_sd=sd(male_prs_control$PRS)

male_prs_case$PRSZ<-((male_prs_case$PRS - male_control_mean)/male_control_sd)
male_prs_control$PRSZ<-((male_prs_control$PRS - male_control_mean)/male_control_sd)

#hist(prs_control$PRSZ, breaks=25, main="PRSice2 Sum Female Only Controls BCC PRS", col="white", freq=FALSE, xlim=c(-4,4),
# ylim=c(0,0.8))
#hist(prs_case$PRSZ, breaks=10, main="PRSice2 Sum Female Only Cases BCC PRS", col="light grey", freq=FALSE, xlim=c(-4,4),
#ylim=c(0,0.8))

summary(male_prs_case$PRSZ)

case_col <- rgb(255, 0, 0, max=255, alpha=140) 

control_col <- rgb(0, 0, 255, max=255, alpha=140) 

overlap_col <- rgb(100, 0, 255, max=255, alpha=200)

casehist<-hist(male_prs_case$PRSZ, breaks=10, main="Male Pubertal Hallmarks PRS Male Cases vs Controls", xlab = "Polygenic Risk Score", col=case_col, freq=FALSE, xlim=c(-4,4),
               ylim=c(0,0.8))

controlhist<-hist(male_prs_control$PRSZ, breaks=15, freq=FALSE, col = control_col, add=TRUE)

legend("topright", legend=c("Cases","Controls","Overlap"), col=c(case_col, control_col,overlap_col), pt.cex=2, pch=15)


#Male V Female

MVF_PRS_results<-read.delim("VB_GWAS.Unknown_Removed.MALESvsFEMALES.sum.best", sep=" ")

Status<-read.delim("DPCon_dbGap_FEB2023.Final_Sex_as_PHENO.txt")

MVF_PRS_Results_Merged_Status<-merge(MVF_PRS_results,Status,by="IID")

MVF_only <- subset(MVF_PRS_Results_Merged_Status, MVF_PRS_Results_Merged_Status$In_Regression == "Yes")

#Females_only$PRSZ<- normalize(Females_only$PRS, method = "standardize")

MVF_female_prs_case <- subset(MVF_only, MVF_only$PHENO=="2")

MVF_male_prs_case <- subset(MVF_only, MVF_only$PHENO=="1")

MVF_male_control_mean=mean(MVF_male_prs_case$PRS)
MVF_male_control_sd=sd(MVF_male_prs_case$PRS)

MVF_male_prs_case$PRSZ<-((MVF_male_prs_case$PRS - MVF_male_control_mean)/MVF_male_control_sd)
MVF_female_prs_case$PRSZ<-((MVF_female_prs_case$PRS - MVF_male_control_mean)/MVF_male_control_sd)

#hist(prs_control$PRSZ, breaks=25, main="PRSice2 Sum Female Only Controls BCC PRS", col="white", freq=FALSE, xlim=c(-4,4),
# ylim=c(0,0.8))
#hist(prs_case$PRSZ, breaks=10, main="PRSice2 Sum Female Only Cases BCC PRS", col="light grey", freq=FALSE, xlim=c(-4,4),
#ylim=c(0,0.8))

summary(MVF_female_prs_case$PRSZ)

case_col <- rgb(255, 0, 0, max=255, alpha=140) 

control_col <- rgb(0, 0, 255, max=255, alpha=140) 

overlap_col <- rgb(100, 0, 255, max=255, alpha=200)

casehist<-hist(MVF_female_prs_case$PRSZ, breaks=12, main="Male Pubertal Hallmarks PRS Delayed Puberty Females vs Males", xlab = "Polygenic Risk Score", col=case_col, freq=FALSE, xlim=c(-4,4),
               ylim=c(0,0.8))

controlhist<-hist(MVF_male_prs_case$PRSZ, breaks=12, freq=FALSE, col = control_col, add=TRUE)

legend("topright", legend=c("Females","Males","Overlap"), col=c(case_col, control_col,overlap_col), pt.cex=2, pch=15)

