### ENVIRONMENT
setwd("C:/DPhil/06 Data Output/0601 Projects/202001 AER Baseline")
MAR_ORIGINAL <- par("mar")
par(mar=c(5,4,1,1))
rm(list=ls())



### PACKAGES
library(readxl)



### SCHOOL MARS
DIRECTORY <- "C:/DPhil/06 Data Output/0601 Projects/202001 AER Baseline/School Marks/"
SCHOOLS <- c("Kagarama/", "Nyamata/", "Nyanza/", "Rango/")
KAGARAMA_S1A_AVER <-read.csv(paste0(paste0(DIRECTORY, SCHOOLS[1]),"S1A2019.csv"))
KAGARAMA_S1B_AVER <-read.csv(paste0(paste0(DIRECTORY, SCHOOLS[1]),"S1B2019.csv"))
NYAMATA <- read.csv(paste0(paste0(DIRECTORY, SCHOOLS[2]),"School_Marks_NYAMATA_Data Entry_DE1_20210123.csv"))
NYANZA <-read.csv(paste0(paste0(DIRECTORY, SCHOOLS[3]),"School_Marks_NYANZA_Data Entry_DE1_20210123.csv")) 
RANGO_S1A_AVER <- read.csv(paste0(paste0(DIRECTORY, SCHOOLS[4]),"S1A_AVER.csv"))
RANGO_S1A_T1 <- read.csv(paste0(paste0(DIRECTORY, SCHOOLS[4]),"S1A_T1.csv"))
RANGO_S1A_T2 <- read.csv(paste0(paste0(DIRECTORY, SCHOOLS[4]),"S1A_T2.csv"))
RANGO_S1A_T3 <- read.csv(paste0(paste0(DIRECTORY, SCHOOLS[4]),"S1A_T3.csv"))
RANGO_S2A_AVER <- read.csv(paste0(paste0(DIRECTORY, SCHOOLS[4]),"S2A_AVER.csv"))
RANGO_S2A_T1 <- read.csv(paste0(paste0(DIRECTORY, SCHOOLS[4]),"S2A_T1.csv"))
RANGO_S2A_T2 <- read.csv(paste0(paste0(DIRECTORY, SCHOOLS[4]),"S2A_T2.csv"))
RANGO_S2A_T3 <- read.csv(paste0(paste0(DIRECTORY, SCHOOLS[4]),"S2A_T3.csv"))
rm(SCHOOLS, DIRECTORY)

SCHOOL_DATA <- rbind(NYAMATA, NYANZA)
colnames(SCHOOL_DATA) <- toupper(colnames(SCHOOL_DATA))

SUBJECTS <- colnames(SCHOOL_DATA)[which(grepl("_",colnames(SCHOOL_DATA)))]
SUBJECTS <- SUBJECTS[which(grepl("_A",SUBJECTS))]
SUBJECTS <- gsub("_A", "", SUBJECTS)


### ### ### ### ### ### ### ### ### ### ### KAGARAMA

### S1A
KAGARAMA_S1A_AVER$Names <- gsub("[\r\n]", " ", KAGARAMA_S1A_AVER$Names)
NAMES <- strsplit(as.character(KAGARAMA_S1A_AVER$Names), split = " ")
NAMES <- do.call(rbind, NAMES)
colnames(NAMES)[1:dim(NAMES)[2]] <- paste0("NAME", 1:dim(NAMES)[2])
KAGARAMA_S1A_AVER <- cbind(KAGARAMA_S1A_AVER, NAMES)

VARIABLE <- grep("NAME",colnames(KAGARAMA_S1A_AVER))
VARIABLE2 <- rev(VARIABLE)

for (COL2 in VARIABLE2) {
   KAGARAMA_S1A_AVER[,COL2] <- as.character(KAGARAMA_S1A_AVER[,COL2])
   for (COL in VARIABLE) {
     KAGARAMA_S1A_AVER[,COL2] <- as.character(KAGARAMA_S1A_AVER[,COL2])
     if (COL2 > COL) {
         ROWS <- which(KAGARAMA_S1A_AVER[,COL] == KAGARAMA_S1A_AVER[,COL2])
         KAGARAMA_S1A_AVER[ROWS ,COL2] <- NA
       
     }
   }
}
KAGARAMA_S1A_AVER$First.Name <- KAGARAMA_S1A_AVER[,VARIABLE[1]]
VARIABLE <- VARIABLE[2:length(VARIABLE)]
KAGARAMA_S1A_AVER$Last.Name <- gsub(" NA","", with(KAGARAMA_S1A_AVER, paste (NAME2, NAME3, NAME4)))
KAGARAMA_S1A_AVER$Names <- NULL
KAGARAMA_S1A_AVER$NAME1 <- NULL
KAGARAMA_S1A_AVER$NAME2 <- NULL
KAGARAMA_S1A_AVER$NAME3 <- NULL
KAGARAMA_S1A_AVER$NAME4 <- NULL
KAGARAMA_S1A_AVER$Grade <- "S1A"


### S1B
KAGARAMA_S1B_AVER$Names <- gsub("[\r\n]", " ", KAGARAMA_S1B_AVER$Names)
NAMES <- strsplit(as.character(KAGARAMA_S1B_AVER$Names), split = " ")
NAMES <- do.call(rbind, NAMES)
colnames(NAMES)[1:dim(NAMES)[2]] <- paste0("NAME", 1:dim(NAMES)[2])
KAGARAMA_S1B_AVER <- cbind(KAGARAMA_S1B_AVER, NAMES)

VARIABLE <- grep("NAME",colnames(KAGARAMA_S1B_AVER))
VARIABLE2 <- rev(VARIABLE)

for (COL2 in VARIABLE2) {
  KAGARAMA_S1B_AVER[,COL2] <- as.character(KAGARAMA_S1B_AVER[,COL2])
  for (COL in VARIABLE) {
    KAGARAMA_S1B_AVER[,COL2] <- as.character(KAGARAMA_S1B_AVER[,COL2])
    if (COL2 > COL) {
      ROWS <- which(KAGARAMA_S1B_AVER[,COL] == KAGARAMA_S1B_AVER[,COL2])
      KAGARAMA_S1B_AVER[ROWS ,COL2] <- NA
      
    }
  }
}
KAGARAMA_S1B_AVER$First.Name <- KAGARAMA_S1B_AVER[,VARIABLE[1]]
VARIABLE <- VARIABLE[2:length(VARIABLE)]
KAGARAMA_S1B_AVER$Last.Name <- gsub(" NA","", with(KAGARAMA_S1B_AVER, paste (NAME2, NAME3)))
KAGARAMA_S1B_AVER$Names <- NULL
KAGARAMA_S1B_AVER$NAME1 <- NULL
KAGARAMA_S1B_AVER$NAME2 <- NULL
KAGARAMA_S1B_AVER$NAME3 <- NULL
KAGARAMA_S1B_AVER$Grade <- "S1B"



### MERGING WTH SCHOOL MARKS
KAGARAMA_S1 <- rbind(KAGARAMA_S1A_AVER, KAGARAMA_S1B_AVER)
KAGARAMA_S1$SCHOOL <- "KAGARAMA"
rm(KAGARAMA_S1A_AVER, KAGARAMA_S1B_AVER, NAMES, COL, COL2, ROWS, VARIABLE, VARIABLE2)

colnames(KAGARAMA_S1) <- toupper(colnames(KAGARAMA_S1))
colnames(KAGARAMA_S1)[colnames(KAGARAMA_S1)=="MATHEMATICS"] <- "MATH_A"
colnames(KAGARAMA_S1)[colnames(KAGARAMA_S1)=="ENGLISH"] <- "ENG_A"
colnames(KAGARAMA_S1)[colnames(KAGARAMA_S1)=="CHEMISTRY"] <- "KINYAR_A"
colnames(KAGARAMA_S1)[colnames(KAGARAMA_S1)=="KINYARWANDA"] <- "CHEM_A"
KAGARAMA_S1$POS <- NULL

KAGARAMA_S1 <- KAGARAMA_S1[,which(colnames(KAGARAMA_S1) %in% colnames(SCHOOL_DATA))]

MISSING <- setdiff(colnames(SCHOOL_DATA), colnames(KAGARAMA_S1))
for (ELEM in MISSING) {
  KAGARAMA_S1$NEW <- NA
  colnames(KAGARAMA_S1)[length(colnames(KAGARAMA_S1))] <- ELEM
}

SCHOOL_DATA <- rbind(SCHOOL_DATA, KAGARAMA_S1)



### ### ### ### ### ### ### ### ### ### ### RANGO
RANGO_S1A_AVER <- RANGO_S1A_AVER[,-which(grepl("X.", colnames(RANGO_S1A_AVER)))]
RANGO_S2A_AVER <- RANGO_S2A_AVER[,-which(grepl("X.", colnames(RANGO_S2A_AVER)))]

RANGO <- ls()[grep("RANGO", ls())]
for (ELEM in RANGO) {
  DATA <- get(ELEM)
  colnames(DATA) <- toupper(colnames(DATA))
  if (length(which(DATA$LAST.NAME==""))>0) DATA <- DATA[-which(DATA$LAST.NAME==""),]
  DATA$X <- NULL
  DATA$BIOLOGY <- NULL
  DATA$ICT <- NULL
  DATA$HISTORY <- NULL
  DATA$GEOGRAPHY <- NULL
  DATA$ENTREPRENEURSHIP <- NULL
  DATA$FRENCH <- NULL
  DATA$KISWAHILI <- NULL
  DATA$LITERATURE <- NULL
  DATA$RELIGION <- NULL
  DATA$SPORTS <- NULL
  DATA$PHYSICS <- NULL
  DATA$NAME <- paste(DATA$FIRST.NAME, DATA$LAST.NAME)
  DATA$SCHOOL <- "RANGO"
  DATA$GRADE <- substr(ELEM, 7, 9)
  colnames(DATA)[colnames(DATA)=="SEX"] <- "GENDER"
  if (grepl("AV", ELEM)) {
    colnames(DATA)[colnames(DATA)=="CHEMISTRY"] <- "CHEM_A"
    colnames(DATA)[colnames(DATA)=="ENGLISH"] <- "ENG_A"
    colnames(DATA)[colnames(DATA)=="KINYARWANDA"] <- "KINYAR_A"
    colnames(DATA)[colnames(DATA)=="MATHS"] <- "MATH_A"  
  }
  if (grepl("T1", ELEM)) {
    colnames(DATA)[colnames(DATA)=="CHEMISTRY"] <- "CHEM_1"
    colnames(DATA)[colnames(DATA)=="ENGLISH"] <- "ENG_1"
    colnames(DATA)[colnames(DATA)=="KINYARWANDA"] <- "KINYAR_1"
    colnames(DATA)[colnames(DATA)=="MATHS"] <- "MATH_1"  
  }
  if (grepl("T2", ELEM)) {
    colnames(DATA)[colnames(DATA)=="CHEMISTRY"] <- "CHEM_2"
    colnames(DATA)[colnames(DATA)=="ENGLISH"] <- "ENG_2"
    colnames(DATA)[colnames(DATA)=="KINYARWANDA"] <- "KINYAR_2"
    colnames(DATA)[colnames(DATA)=="MATHS"] <- "MATH_2"  
  }
  if (grepl("T3", ELEM)) {
    colnames(DATA)[colnames(DATA)=="CHEMISTRY"] <- "CHEM_3"
    colnames(DATA)[colnames(DATA)=="ENGLISH"] <- "ENG_3"
    colnames(DATA)[colnames(DATA)=="KINYARWANDA"] <- "KINYAR_3"
    colnames(DATA)[colnames(DATA)=="MATHS"] <- "MATH_3"  
  }
  assign(ELEM, DATA)
}

RANGO_AVER <- rbind(RANGO_S1A_AVER, RANGO_S2A_AVER)
RANGO_T1 <- rbind(RANGO_S1A_T1, RANGO_S2A_T1)
RANGO_T2 <- rbind(RANGO_S1A_T2, RANGO_S2A_T2)
RANGO_T3 <- rbind(RANGO_S1A_T3, RANGO_S2A_T3)

RANGO <- merge(RANGO_T1, RANGO_T2, by = c("NAME", "FIRST.NAME", "LAST.NAME", "GENDER", "SCHOOL", "GRADE" ))
RANGO <- merge(RANGO, RANGO_T3, by = c("NAME", "FIRST.NAME", "LAST.NAME", "GENDER", "SCHOOL", "GRADE" ))
RANGO <- merge(RANGO, RANGO_AVER, by = c("NAME", "FIRST.NAME", "LAST.NAME", "GENDER", "SCHOOL", "GRADE" ))
RANGO$NAME <- NULL

SCHOOL_DATA <- rbind(SCHOOL_DATA, RANGO)


