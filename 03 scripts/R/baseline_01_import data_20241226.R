### environment ----------------------- ----------------------- ----------------------- -----------------------
setwd("C:/Users/domin/GitHub/2020_baseline")
MAR_ORIGINAL <- par("mar")
par(mar=c(5,4,1,1))
rm(list=ls())



### Packages ----------------------- ----------------------- ----------------------- -----------------------
library(dplyr)
library(readxl)




### First entry [baseline main data] ----------------------- ----------------------- ----------------------- -----------------------
### 1.0) IMPORT FIRST ENTRY
DEOs <- c("AMANDINE", "AMMINAH", "AUBIN", "BAPTISTE", "DJAZIRA", "ISLAENE", "JEAN CLAUDE")
TYPEs <- c("CKME", "ECKM", "KMEC", "MECK")
DATA_FILES <- c("01 raw data/Data Entry 01/Baseline_Data Entry Template_20200121_AMANDINE_DE1_20200124.xlsx",
                "01 raw data/Data Entry 01/Baseline_Data Entry Template_20200121_AMMINAH_DE1_20200124.xlsx",
                "01 raw data/Data Entry 01/Baseline_Data Entry Template_20200121_AUBIN_DE1_20200124.xlsx",
                "01 raw data/Data Entry 01/Baseline_Data Entry Template_20200121_BAPTISTE_DE1_20200124.xlsx",
                "01 raw data/Data Entry 01/Baseline_Data Entry Template_20200121_DJAZIRA_DE1_20200124.xlsx",
                "01 raw data/Data Entry 01/Baseline_Data Entry Template_20200121_ISLAENE_DE1_20200124.xlsx",
                "01 raw data/Data Entry 01/Baseline_Data Entry Template_20201006_JEAN CLAUDE_DE1.xlsx")
DATAFRAMES <- c()

for (FILE in DATA_FILES) {
   DATA<-lapply(excel_sheets(FILE), read_excel, path = FILE)
   NAME <- excel_sheets(FILE)
   for (NO in 1: length(NAME)) {
     for (TYPE in TYPEs) {
       if(NAME[[NO]] == TYPE){
         for (D_NAM in DEOs) {
           if (grepl(pattern =D_NAM,FILE) == "TRUE") {
             assign(paste0(D_NAM,"_", TYPE), DATA[[NO]][,1:105])
             DATAFRAMES <- c(DATAFRAMES, paste0(D_NAM,"_", TYPE))
           }
         }
       }
     }
   }
}
rm(DATA)

for (FRAMES in DATAFRAMES) {
  DATASET <- get(FRAMES)
  DATASET <- DATASET[rowSums(is.na(DATASET)) != ncol(DATASET),]
  assign(FRAMES, DATASET)
}

### MERGE FIRST ENTRY
CKME <- do.call(rbind, lapply( ls(patt="_CKME"), get))
CKME$VERSION <- "CKME"

ECKM <- do.call(rbind, lapply( ls(patt="_ECKM"), get))
ECKM$VERSION <- "ECKM"

KMEC <- do.call(rbind, lapply( ls(patt="_KMEC"), get))
KMEC$VERSION <- "KMEC"

MECK <- do.call(rbind, lapply( ls(patt="_MECK"), get))
MECK$VERSION <- "MECK"
DELETE <- ls()[grep("_",ls())]
rm(list=DELETE)


for (TYPE in TYPEs) {                                        #### ENSURE ALL SUBSETS HAVE THE SAME COLUMN STRUCTURE
  DATASET <- get(TYPE)
  DATASET <- DATASET[rowSums(is.na(DATASET)) != ncol(DATASET),]
  DATASET <- DATASET[,colnames(ECKM)]
  assign(TYPE, DATASET)
}
rm(DATASET)

FIRSTENTRY <- rbind(CKME, ECKM, KMEC, MECK)
FIRSTENTRY <- data.frame(FIRSTENTRY)
rm(DATAFRAMES, DELETE, CKME, ECKM, KMEC, MECK, DEOs, FILE, FRAMES, NAME, NO, TYPE, TYPEs)



### BASIC DATA CLEANING [FIRST ENTRY] ----------------------- ----------------------- ----------------------- -----------------------
FIRSTENTRY <- FIRSTENTRY[order(FIRSTENTRY$SNO),]

FIRSTENTRY$School[FIRSTENTRY$School == "CATHOLIQUE" | FIRSTENTRY$School == "CATHORIQWE" | 
                    FIRSTENTRY$School == "CATHORIQWE NYAMATA" | FIRSTENTRY$School == "G,S NYAMATA CATHOLIQWE" | 
                    FIRSTENTRY$School == "G.S BOSCO" | FIRSTENTRY$School == "G.S CATHOLIQUE" | 
                    FIRSTENTRY$School == "G.S CATHOLIQWE" | FIRSTENTRY$School == "G.S CATTOLIC" | 
                    FIRSTENTRY$School == "G.S NYAMATA" | FIRSTENTRY$School == "G.S NYAMATA CACOLIQWE" |
                    FIRSTENTRY$School == "G.S NYAMATA CATH" | FIRSTENTRY$School == "G.S NYAMATA CATHOEQEA" | 
                    FIRSTENTRY$School == "G.S NYAMATA CATHOLIC" | FIRSTENTRY$School == "G.S NYAMATA CATHOLIQUE" | 
                    FIRSTENTRY$School == "G.S NYAMATA CATHOLIQUO" | FIRSTENTRY$School == "G.S NYAMATA CATHOLIQWE" | 
                    FIRSTENTRY$School == "G.S NYAMATA CATHORIC" | FIRSTENTRY$School == "G.S NYAMATA CATHORIQUE" |
                    FIRSTENTRY$School == "G.S NYAMATA CATHORIQWE" | FIRSTENTRY$School == "G.S NYAMATA GATHOLIQWE" |
                    FIRSTENTRY$School == "G.S NYAMATA GATORIKE" | FIRSTENTRY$School == "G.S NYAMATA QWATOLIQWE" |
                    FIRSTENTRY$School == "G.S NYMATA CATHOLIQWE" | FIRSTENTRY$School == "G.SNYAMATA GATHOLIQWE" |
                    FIRSTENTRY$School == "NYAMATA  CATHOLIQUE" | FIRSTENTRY$School == "NYAMATA CATHOLIC" | 
                    FIRSTENTRY$School == "NYAMATA CATHOLIQUE" | FIRSTENTRY$School == "NYAMATA CATHOLIQWE" | 
                    FIRSTENTRY$School == "NYAMATA CATHOQWE" | FIRSTENTRY$School == "NYAMATA CATHORIQWE" | 
                    FIRSTENTRY$School == "NYAMATA CATOLIQWE" | FIRSTENTRY$School == "SAINT JEAN BOSCO" |
                    FIRSTENTRY$School == "ST JEAN BOSCO"] <- "NYAMATA" 

FIRSTENTRY$School[FIRSTENTRY$School == "GS NYANZA" | FIRSTENTRY$School == "G NYANZA" | 
                    FIRSTENTRY$School == "G.S NYANZA" | FIRSTENTRY$School == "G.S.NYANZA"] <- "NYANZA" 

FIRSTENTRY$School[FIRSTENTRY$School == "G.S RANGO" | FIRSTENTRY$School == "G.S.RANGO" | 
                    FIRSTENTRY$School == "GS RANGO"] <- "RANGO" 

FIRSTENTRY$School[FIRSTENTRY$School == "K" | FIRSTENTRY$School == "K.S.S" | 
                    FIRSTENTRY$School == "K/S/S" | FIRSTENTRY$School == "KAGARAMA HIGH SCHOOL" | 
                    FIRSTENTRY$School == "KAGARAMA S.S" | FIRSTENTRY$School == "KAGARAMA SEC SCHOOL" | 
                    FIRSTENTRY$School == "KAGARAMA SECONDARY SCHOOL" | FIRSTENTRY$School == "KAGARAMA SECONDRY SCHOOL" | 
                    FIRSTENTRY$School == "KAGARRAMA S.S" | FIRSTENTRY$School == "KAGRAMA"] <- "KAGARAMA" 

FIRSTENTRY$Class[FIRSTENTRY$Class == "SECONDERY 1A" | 
                 FIRSTENTRY$Class == "SENION ONE A"  ] <- "S1A"

FIRSTENTRY$Class[FIRSTENTRY$Class == "SNION 2B"] <- "S2B"

FIRSTENTRY$Class[FIRSTENTRY$Class == "SENION TWO D" | 
                 FIRSTENTRY$Class == "SENIOR2D"] <- "S2D"

FIRSTENTRY$Class[FIRSTENTRY$Class == "SENION THREE A" | 
                 FIRSTENTRY$Class == "SENIOR THREE A"] <- "S3A"

FIRSTENTRY$Class[FIRSTENTRY$Class == "S3 B"] <- "S3B"
FIRSTENTRY$Class[FIRSTENTRY$Class == "S3 B"] <- "S3B"

FIRSTENTRY$Sex[FIRSTENTRY$Sex=="G  ABO" | 
               FIRSTENTRY$Sex=="GABB"] <- "GABO"

FIRSTENTRY$Sex[FIRSTENTRY$Sex=="ORE"] <- "GORE"

BSL_VARIABLES <- colnames(FIRSTENTRY)
BSL_VARIABLES <- BSL_VARIABLES[-which(BSL_VARIABLES=="PD_6")]

FIRSTENTRY[,BSL_VARIABLES] <- as.data.frame(lapply(FIRSTENTRY[,BSL_VARIABLES], function(x) ifelse( is.na(x),
                                                                                                   if(is.numeric(x)){ -999} else {"-999"},          ### REPLACE ALL NA's IN THE DATA SET INTO -999
                                                                                                   x)))                                             ### -999 INDICATES THAT FORM OR COLUMN IS ENTIRELY MISSING
#write.csv(FIRSTENTRY, "01 raw data/Data Reconciliation/FIRSTENTRY_20210913.csv")
                       


### Second entry [baseline main data]  ----------------------- ----------------------- ----------------------- -----------------------
### 2.0) IMPORT SECOND ENTRY
DEOs <- c("DAVID", "DOMINIK")
TYPEs <- c("CKME", "ECKM", "KMEC", "MECK")
DATA_FILES <- c("01 raw data/Data Entry 02/Baseline_Data Entry Template_DATA ENTRY02_DAVID_20210203.xlsx",
                "01 raw data/Data Entry 02/Baseline_Data Entry Template_DATA ENTRY02_DOMINIK_20210202.xlsx")
DATAFRAMES <- c()

for (FILE in DATA_FILES) {
  DATA<-lapply(excel_sheets(FILE), read_excel, path = FILE)
  NAME <- excel_sheets(FILE)
  for (NO in 1: length(NAME)) {
    for (TYPE in TYPEs) {
      if(NAME[[NO]] == TYPE){
        for (D_NAM in DEOs) {
          if (grepl(pattern =D_NAM,FILE) == "TRUE") {
            assign(paste0(D_NAM,"_", TYPE), DATA[[NO]][,1:105])
            DATAFRAMES <- c(DATAFRAMES, paste0(D_NAM,"_", TYPE))
          }
        }
      }
    }
  }
}
rm(DATA, D_NAM, DATA_FILES)

for (FRAMES in DATAFRAMES) {
  DATASET <- get(FRAMES)
  DATASET <- DATASET[rowSums(is.na(DATASET)) != ncol(DATASET),]
  assign(FRAMES, DATASET)
}



### MERGE SECOND ENTRY
CKME <- do.call(rbind, lapply( ls(patt="_CKME"), get))
CKME$VERSION <- "CKME"

ECKM <- do.call(rbind, lapply( ls(patt="_ECKM"), get))
ECKM$VERSION <- "ECKM"

KMEC <- do.call(rbind, lapply( ls(patt="_KMEC"), get))
KMEC$VERSION <- "KMEC"

MECK <- do.call(rbind, lapply( ls(patt="_MECK"), get))
MECK$VERSION <- "MECK"
DELETE <- ls()[grep("_",ls())]

for (TYPE in TYPEs) {                                        #### ENSURE ALL SUBSETS HAVE THE SAME COLUMN STRUCTURE
  DATASET <- get(TYPE)
  DATASET <- DATASET[rowSums(is.na(DATASET)) != ncol(DATASET),]
  DATASET <- DATASET[,colnames(ECKM)]
  assign(TYPE, DATASET)
}
rm(DATASET)

SECONDENTRY <- rbind(CKME, ECKM, KMEC, MECK)
SECONDENTRY <- data.frame(SECONDENTRY)
rm(DATAFRAMES, DELETE, CKME, ECKM, KMEC, MECK, DEOs, FILE, FRAMES, NAME, NO, TYPE, TYPEs)
rm(DAVID_CKME, DAVID_ECKM, DAVID_KMEC, DAVID_MECK, DOMINIK_CKME, DOMINIK_ECKM, DOMINIK_KMEC, DOMINIK_MECK)



### BASIC DATA CLEANING [SECOND ENTRY] ----------------------- ----------------------- ----------------------- -----------------------
SECONDENTRY <- SECONDENTRY[order(SECONDENTRY$SNO),]

SECONDENTRY[,BSL_VARIABLES] <- as.data.frame(lapply(SECONDENTRY[,BSL_VARIABLES], function(x) ifelse( is.na(x),
                                                                                                     if(is.numeric(x)){ -999} else {"-999"},          ### REPLACE ALL NA's IN THE DATA SET INTO -999
                                                                                                     x)))                                             ### -999 INDICATES THAT FORM OR COLUMN IS ENTIRELY MISSING
rm(BSL_VARIABLES)
#write.csv(SECONDENTRY, "01 raw data/Data Reconciliation/SECONDENTRY_20210913.csv")



### Data reconciliation [baseline main data]  ----------------------- ----------------------- ----------------------- -----------------------
### 3.0) DATA MISMATCHES
MISMATCHES <- data.frame(SNO = numeric(), NAME = character(), VAR = character(), FIRST = character(), SECOND = character())

VARIABLES <- colnames(FIRSTENTRY)
VARIABLES <- VARIABLES[-which(VARIABLES %in% c("SNO", "Name", "VERSION"))]
  
for(VAR in VARIABLES){
  NEW <- data.frame(SNO = FIRSTENTRY$SNO[which(FIRSTENTRY[,VAR] != SECONDENTRY[,VAR])], 
                    NAME = SECONDENTRY$Name[which(FIRSTENTRY[,VAR] != SECONDENTRY[,VAR])], 
                    VAR = VAR, 
                    FIRST = FIRSTENTRY[which(FIRSTENTRY[,VAR] != SECONDENTRY[,VAR]), VAR], 
                    SECOND = SECONDENTRY[which(FIRSTENTRY[,VAR] != SECONDENTRY[,VAR]), VAR]) 
  MISMATCHES <- rbind(MISMATCHES, NEW)
  
}

MISMATCHES <- MISMATCHES[order(MISMATCHES$SNO),]
#write.csv(MISMATCHES, "01 raw data/Data Reconciliation/MISMATCHES_20210913.csv")
rm(NEW, VAR, VARIABLES)



### INCORPORATE RECONCILED ANSWERS [GAMI'S SECOND ENTRY - MORE TRUSTWORTYHY] ----------------------- ----------------------- ----------------------- -----------------------
baseline <- SECONDENTRY
CHECKED <- as.data.frame(read_excel("01 raw data/Data Reconciliation/MISMATCHES_BASELINE_DAVID_20210807.xlsx", sheet = "MISMATCHES_20210605"))
for(ROW in 1: nrow(CHECKED)){
  baseline[baseline$SNO==CHECKED$SNO[ROW],CHECKED$VAR[ROW]] <- CHECKED$FINAL[ROW]
}



### 3.2) INCORPORATE ANSWERS FROM FIRSTENTRY
FIRSTENTRY <- FIRSTENTRY[,c("SNO", "Name")]
colnames(FIRSTENTRY)[colnames(FIRSTENTRY)=="Name"] <- "Name1"
baseline <- merge(baseline, FIRSTENTRY, by ="SNO", all.x=T) 
rm(FIRSTENTRY, SECONDENTRY)



### Clean-up and incorporate follow data First entry [baseline main data]  ----------------------- ----------------------- ----------------------- -----------------------
FOLLOWUP_DE1 <- read.csv("01 raw data/Follow Up/Final Baseline_FOLLOW_UP_DE1_DOMINIK_20210806.csv")
FOLLOWUP_DE1 <- FOLLOWUP_DE1[order(FOLLOWUP_DE1$FSNO),]
FOLLOWUP_DE2 <- as.data.frame(read_excel("01 raw data/Follow Up/Final Baseline_FOLLOW_UP_DE2_GAMI_20210807.xlsx", sheet = "Final Baseline_FOLLOW_UP_DE1_DO"))
FOLLOWUP_DE2 <- FOLLOWUP_DE2[order(FOLLOWUP_DE2$FSNO),]

MISMATCHES <- data.frame(SNO = numeric(), NAME = character(), VAR = character(), FIRST = character(), SECOND = character())
VARIABLES <- colnames(FOLLOWUP_DE1)
VARIABLES <- VARIABLES[-which(VARIABLES %in% c("FSNO", "Name", "School"))]
for(VAR in VARIABLES){
  if (length(FOLLOWUP_DE1$FSNO[which(FOLLOWUP_DE1[,VAR] != FOLLOWUP_DE2[,VAR])])>0) {
    NEW <- data.frame(SNO = FOLLOWUP_DE1$FSNO[which(FOLLOWUP_DE1[,VAR] != FOLLOWUP_DE2[,VAR])], 
                      NAME = FOLLOWUP_DE1$Name[which(FOLLOWUP_DE1[,VAR] != FOLLOWUP_DE2[,VAR])], 
                      VAR = VAR, 
                      FIRST = FOLLOWUP_DE1[which(FOLLOWUP_DE1[,VAR] != FOLLOWUP_DE2[,VAR]), VAR], 
                      SECOND = FOLLOWUP_DE2[which(FOLLOWUP_DE1[,VAR] != FOLLOWUP_DE2[,VAR]), VAR]) 
    MISMATCHES <- rbind(MISMATCHES, NEW)
  }
}
MISMATCHES <- MISMATCHES[order(MISMATCHES$SNO),]
#write.csv(MISMATCHES, "01 raw data/Follow Up/FOLLOWUP_MISMATCHES_20210807.csv")

FOLLOWUP <- FOLLOWUP_DE1
CHECKED <- as.data.frame(read_excel("01 raw data/Follow Up/FOLLOWUP_MISMATCHES_GAMI_RESULTS_20210807.XLSX", sheet = "FOLLOWUP_MISMATCHES_20210807"))

for(ROW in 1: nrow(CHECKED)){
  FOLLOWUP[FOLLOWUP$FSNO==CHECKED$SNO[ROW],CHECKED$VAR[ROW]] <- CHECKED$RESULT[ROW]
}

FOLLOWUP[,grep("_", colnames(FOLLOWUP))] <- lapply(FOLLOWUP[,grep("_", colnames(FOLLOWUP))], function(x) replace(x,x %in% -9, NA) )
colnames(FOLLOWUP) <- paste0(colnames(FOLLOWUP), "_R")
colnames(FOLLOWUP)[colnames(FOLLOWUP)=="FSNO_R"] <- "SNO"
FOLLOWUP$School_R <- NULL
FOLLOWUP$Class_R <- NULL
FOLLOWUP$Name_R <- NULL
FOLLOWUP$Age_R <- NULL
FOLLOWUP$Sex_R <- NULL
baseline <- merge(baseline, FOLLOWUP, by= "SNO", all.x = T)
rm(CHECKED, FOLLOWUP, FOLLOWUP_DE1, FOLLOWUP_DE2, MISMATCHES, NEW, ROW, VAR, VARIABLES)



# 4 VARIABLES (I.E., "M_NE2, M_GE4, M_EF5, C_AB5) WILL NOW SHOW UP IN THE BASELINE TWICE. ALL VARIABLES WITHOUT EXTENSION ARE FROM THE BASELINE. 
# VARIABLES WITH EXTENSION '_R' INCLDUE FOLLOW-UP VALUES FOR THOSE FOR WHOM WE COLLECTED AND NA FOR ALL OTHERS
# VARIABLES WITH EXTENSION '_F' INCLDUE FOLLOW-UP VALUES FOR THOSE FOR WHOM WE COLLECTED AND INITIAL BASELINE VALUES FOR ALL OTHERS
baseline$MISTAKE <- 0
baseline$MISTAKE[baseline$School=="KAGARAMA" & (baseline$Class=="S2A" | baseline$Class=="S2B" | baseline$Class=="S2C" | baseline$Class=="S2D")] <- 1
baseline$MISTAKE[baseline$School=="NYANZA" & (baseline$Class=="S2A" | baseline$Class=="S2B")] <- 1
baseline$MISTAKE[baseline$School=="NYANZA" & (baseline$Class=="S3A" | baseline$Class=="S3B" | baseline$Class=="S3C")] <- 1

VARIABLES <- substr(colnames(baseline)[grep("_R", colnames(baseline))], 1,5)
for (VAR in VARIABLES) {
  baseline$NEW <- baseline[,VAR] 
  colnames(baseline)[colnames(baseline)=="NEW"] <- paste0(VAR,"_F")
  baseline[,paste0(VAR,"_F")][!is.na(baseline[,paste0(VAR, "_R")])] <- baseline[,paste0(VAR, "_R")][!is.na(baseline[,paste0(VAR, "_R")])]
}
rm(VAR, VARIABLES)



### Incorporate consent forms  ----------------------- ----------------------- ----------------------- -----------------------
CONSENTFORMS_SNO <- as.data.frame(read_excel("01 raw data/Consent Forms_Cleaning/CONSENTFORMS_COMPARISON_20210701_VERSION03.xlsx"))
colnames(CONSENTFORMS_SNO)[colnames(CONSENTFORMS_SNO)=="BASELINE_SNO"] <- "SNO"
CONSENTFORMS_SNO <- CONSENTFORMS_SNO[,c("SNO", "CONSENTFORM_SNO")]
baseline <- merge(baseline, CONSENTFORMS_SNO, by = "SNO")

CONSENTFORMS <- as.data.frame(read.csv("01 raw data/Consent Forms_Cleaning/CONSENTFORMS_PHASE IV_20220223.csv"))
colnames(CONSENTFORMS)[colnames(CONSENTFORMS)=="SNO"] <- "CONSENTFORM_SNO"
colnames(CONSENTFORMS)[colnames(CONSENTFORMS)=="Name"] <- "Name_Consent"
colnames(CONSENTFORMS)[colnames(CONSENTFORMS)=="STUDENT"] <- "Consent"
colnames(CONSENTFORMS)[colnames(CONSENTFORMS)=="Class"] <- "Class_Consent"
CONSENTFORMS$X <- NULL 
CONSENTFORMS$School <- NULL 
baseline <- merge(baseline, CONSENTFORMS, by = "CONSENTFORM_SNO", all.x = T)
baseline$CONSENTFORM_SNO <- NULL
rm(CONSENTFORMS, CONSENTFORMS_SNO)



### Remove entries without names ----------------------- ----------------------- ----------------------- -----------------------
baseline <- baseline %>%
  filter(Name != "-9")
    


### Incorporate school marks ----------------------- ----------------------- ----------------------- -----------------------

school_marks <- read.csv("C:/Users/domin/GitHub/2019_school_marks/02 processed data/2019_school_marks_20241227.csv")
school_marks <- school_marks %>%
  rename("name_smarks" = name) %>%
  select(-c(baseline_name, gender, school, class))
baseline <- merge(baseline, school_marks, by.x = "SNO", by.y = "baseline_id", all.x = T) 
rm(school_marks)



### Order columns ----------------------- ----------------------- ----------------------- -----------------------

baseline <- baseline %>%
  rename("sno_baseline" = SNO,
         "sno_smarks" = sno2,
         "school" = School,
         "class" = Class,
         "consent" = Consent,
         "name_baseline" = Name,
         "name1" = Name1,
         "name_consent" = Name_Consent,
         "age" = Age,
         "gender" = Sex,
         "mistake" = MISTAKE,
         "pd_6" = PD_6, 
         "pd_7" = PD_7, 
         "pd_8" = PD_8,
         "version" = VERSION,
         "c_ab2" = C_AB2, "c_ab3" = C_AB3, "c_ab4" = C_AB4, "c_ab5" = C_AB5, "c_ab6" = C_AB6, "c_ab7" = C_AB7, "c_ab8" = C_AB8,
         "c_da1" = C_DA1, "c_da2" = C_DA2, "c_da3" = C_DA3,
         "c_ef2" = C_EF2, "c_ef3" = C_EF3, "c_ef5" = C_EF5,
         "c_ge1" = C_GE1, "c_ge2" = C_GE2, "c_ge3" = C_GE3, "c_ge4" = C_GE4, "c_ge5" = C_GE5,
         "c_ne2" = C_NE2, "c_ne4" = C_NE4, 
         "c_pi1" = C_PI1, "c_pi2" = C_PI2, "c_pi3" = C_PI3, "c_pi4" = C_PI4,
         "c_po2" = C_PO2, 
         "e_ab2" = E_AB2, "e_ab3" = E_AB3, "e_ab4" = E_AB4, "e_ab5" = E_AB5, "e_ab6" = E_AB6, "e_ab8" = E_AB8, 
         "e_da1" = E_DA1, "e_da3" = E_DA3, 
         "e_ef2" = E_EF2, "e_ef3" = E_EF3, "e_ef5" = E_EF5, 
         "e_ge1" = E_GE1, "e_ge3" = E_GE3, "e_ge4" = E_GE4, "e_ge5" = E_GE5, 
         "e_ne1" = E_NE1, "e_ne2" = E_NE2, "e_ne3" = E_NE3, "e_ne4" = E_NE4, 
         "e_oe2" = E_OE2, 
         "e_pi2" = E_PI2, "e_pi3" = E_PI3, "e_pi4" = E_PI4, 
         "k_ab2" = K_AB2, "k_ab3" = K_AB3, "k_ab4" = K_AB4, "k_ab5" = K_AB5, "k_ab6" = K_AB6, "k_ab8" = K_AB8,
         "k_da1" = K_DA1, "k_da3" = K_DA3,
         "k_ef2" = K_EF2, "k_ef3" = K_EF3, "k_ef5" = K_EF5,
         "k_ge1" = K_GE1, "k_ge3" = K_GE3, "k_ge4" = K_GE4, "k_ge5" = K_GE5,
         "k_ne1" = K_NE1, "k_ne2" = K_NE2, "k_ne3" = K_NE3, "k_ne4" = K_NE4,
         "k_oe2" = K_OE2,
         "k_pi2" = K_PI2, "k_pi3" = K_PI3, "k_pi4" = K_PI4, 
         "m_ab2" = M_AB2, "m_ab3" = M_AB3, "m_ab4" = M_AB4, "m_ab5" = M_AB5, "m_ab6" = M_AB6, "m_ab7" = M_AB7, "m_ab8" = M_AB8,
         "m_da1" = M_DA1, "m_da2" = M_DA2, "m_da3" = M_DA3,
         "m_ef2" = M_EF2, "m_ef3" = M_EF3, "m_ef5" = M_EF5,
         "m_ge1" = M_GE1, "m_ge2" = M_GE2, "m_ge3" = M_GE3, "m_ge4" = M_GE4, "m_ge5" = M_GE5,
         "m_ne2" = M_NE2, "m_ne4" = M_NE4,
         "m_pi1" = M_PI1, "m_pi2" = M_PI2, "m_pi3" = M_PI3, 
         "m_pi4" = M_PI4,
         "m_po2" = M_PO2,
         "c_ab5_r" = C_AB5_R, "m_ef5_r" = M_EF5_R, "m_ge4_r" = M_GE4_R, "m_ne2_r" = M_NE2_R, 
         "c_ab5_f" = C_AB5_F, "m_ef5_f" = M_EF5_F, "m_ge4_f" = M_GE4_F, "m_ne2_f" = M_NE2_F) %>%
  select(version, 
         sno_baseline, sno_smarks, 
         school, class, 
         name_baseline, name1, name_consent, name_smarks,
         consent,
         age, gender, 
         c_ab2, c_ab3, c_ab4, c_ab5, c_ab6, c_ab7, c_ab8,
         c_da1, c_da2, c_da3,
         c_ef2, c_ef3, c_ef5,
         c_ge1, c_ge2, c_ge3, c_ge4, c_ge5,
         c_ne2, c_ne4, 
         c_pi1, c_pi2, c_pi3, c_pi4,
         c_po2, 
         e_ab2, e_ab3, e_ab4, e_ab5, e_ab6, e_ab8, 
         e_da1, e_da3, 
         e_ef2, e_ef3, e_ef5, 
         e_ge1, e_ge3, e_ge4, e_ge5, 
         e_ne1, e_ne2, e_ne3, e_ne4, 
         e_oe2, 
         e_pi2, e_pi3, e_pi4, 
         k_ab2, k_ab3, k_ab4, k_ab5, k_ab6, k_ab8,
         k_da1, k_da3,
         k_ef2, k_ef3, k_ef5,
         k_ge1, k_ge3, k_ge4, k_ge5,
         k_ne1, k_ne2, k_ne3, k_ne4,
         k_oe2,
         k_pi2, k_pi3, k_pi4, 
         m_ab2, m_ab3, m_ab4, m_ab5, m_ab6, m_ab7, m_ab8,
         m_da1, m_da2, m_da3,
         m_ef2, m_ef3, m_ef5,
         m_ge1, m_ge2, m_ge3, m_ge4, m_ge5,
         m_ne2, m_ne4,
         m_pi1, m_pi2, m_pi3, m_pi4,
         m_po2,
         pd_6, pd_7, pd_8,
         mistake, 
         c_ab5_r, m_ef5_r, m_ge4_r, m_ne2_r, 
         c_ab5_f, m_ef5_f, m_ge4_f, m_ne2_f, 
         Biology_CAT, Biology_EX, Biology_TOT, Biology_Annual, 
         Chemistry_CAT, Chemistry_EX, Chemistry_TOT, Chemistry_Annual, 
         Entrepreneurship_CAT, Entrepreneurship_EX, Entrepreneurship_TOT, Entrepreneurship_Annual, 
         English_CAT, English_EX, English_TOT, English_Annual, 
         French_CAT, French_EX, French_TOT, French_Annual, 
         Geography_CAT, Geography_EX, Geography_TOT, Geography_Annual, 
         History_CAT, History_EX, History_TOT, History_Annual, 
         ICT_CAT, ICT_EX, ICT_TOT, ICT_Annual, 
         Kinyarwanda_CAT, Kinyarwanda_EX, Kinyarwanda_TOT, Kinyarwanda_Annual, 
         Kiswahili_CAT, Kiswahili_EX, Kiswahili_TOT, Kiswahili_Annual, 
         Literature_CAT, Literature_EX, Literature_TOT, Literature_Annual, 
         Mathematics_CAT, Mathematics_EX, Mathematics_TOT, Mathematics_Annual,
         Physics_CAT, Physics_EX, Physics_TOT, Physics_Annual, 
         Religion_CAT, Religion_EX, Religion_TOT, Religion_Annual, 
         Sports_CAT, Sports_EX, Sports_TOT, Sports_Annual)



### SAVE DATA  ----------------------- ----------------------- ----------------------- -----------------------

write.csv(baseline, "02 processed data/baseline_raw_20241229.csv", row.names = F)



