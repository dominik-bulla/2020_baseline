### environment ----------------------- ----------------------- ----------------------- -----------------------
setwd("C:/Users/domin/GitHub/2020_baseline")
MAR_ORIGINAL <- par("mar")
par(mar=c(5,4,1,1))
rm(list=ls())



### Packages ----------------------- ----------------------- ----------------------- -----------------------

library(readxl)
library(dplyr)
library(psych)



### Load data ----------------------- ----------------------- ----------------------- -----------------------

baseline <- read.csv("02 processed data/baseline_raw_20241229.csv")



### Clean school- and class-level variables ----------------------- ----------------------- ----------------------- ----------------------- 

baseline <- baseline %>%
  mutate(school = ifelse(school == "MU MWAKA WA KABIRI", "KAGARAMA", school),
         school = ifelse(sno_baseline == 439 | sno_baseline == 560 | sno_baseline == 293 | sno_baseline == 616 | sno_baseline == 620 |
                         sno_baseline == 955 | sno_baseline == 969 | sno_baseline == 971 | sno_baseline == 988, "KAGARAMA", school),
         school = ifelse(school == "GS. NYAMATA CATHOLIQUE", "NYAMATA", school),
         school = ifelse(sno_baseline == 77 | sno_baseline == 96 | sno_baseline == 118 | sno_baseline == 119 | sno_baseline == 181 | 
                         sno_baseline == 1008 | sno_baseline == 1009 | sno_baseline == 1019 | sno_baseline == 1024 | sno_baseline == 1028 | 
                         sno_baseline == 1054 | sno_baseline == 1056 | sno_baseline == 1070 | sno_baseline == 1075 | sno_baseline == 1099 | 
                         sno_baseline == 1106 | sno_baseline == 1124 | sno_baseline == 1126 | sno_baseline == 1137 | sno_baseline == 1149 | 
                         sno_baseline == 1155, "NYAMATA", school),
         school = ifelse(school == "GS. NYANZA", "NYANZA", school),
         school = ifelse(sno_baseline == 763 | sno_baseline == 837 | sno_baseline == 868 | sno_baseline == 874 | sno_baseline == 903 | 
                         sno_baseline == 906 | sno_baseline == 936 | sno_baseline == 940 | sno_baseline == 941 | sno_baseline == 946 | 
                         sno_baseline == 1310 | sno_baseline == 1363 | sno_baseline == 1373, "NYANZA", school),
         school = ifelse(sno_baseline == 334 | sno_baseline == 360 | sno_baseline == 662 | sno_baseline == 669 | sno_baseline == 674 | 
                         sno_baseline == 684 | sno_baseline == 686 | sno_baseline == 694 | sno_baseline == 697 | sno_baseline == 698 | 
                         sno_baseline == 334 | sno_baseline == 360 | sno_baseline == 662 | sno_baseline == 669 | sno_baseline == 674 | 
                         sno_baseline == 684 | sno_baseline == 686 | sno_baseline == 694 | sno_baseline == 697 | sno_baseline == 698, "NYANZA", school)) %>%
  mutate(school = factor(school, ordered = F)) %>%
  mutate(school1 = ifelse(school == "KAGARAMA", 1, 0),
         school2 = ifelse(school == "NYAMATA", 1, 0),
         school3 = ifelse(school == "NYANZA", 1, 0),
         school4 = ifelse(school == "RANGO", 1, 0),)

baseline <- baseline %>%
  mutate(class = ifelse(sno_baseline == 955 | sno_baseline == 1382 | sno_baseline == 1071 | sno_baseline == 1107, "S1B", class),
         class = ifelse(sno_baseline == 1024 | sno_baseline == 1026, "S1D", class),
         class = ifelse(sno_baseline == 360 | sno_baseline == 947, "S2A", class),
         class = ifelse(sno_baseline == 613 | sno_baseline == 614 | sno_baseline == 624 | sno_baseline == 674 | sno_baseline == 722 | 
                        sno_baseline == 747, "S2B", class),
         class = ifelse(sno_baseline == 118 | sno_baseline == 120 | sno_baseline == 128 | sno_baseline == 136 | sno_baseline == 420 | 
                        sno_baseline == 423, "S2C", class),
         class = ifelse(sno_baseline == 554 | sno_baseline == 555 | sno_baseline == 560 | sno_baseline == 565 | sno_baseline == 566 | 
                        sno_baseline == 569 | sno_baseline == 603, "S2D", class),
         class = ifelse(sno_baseline == 245, "S3A", class),
         class = ifelse(sno_baseline == 320 | sno_baseline == 716 | sno_baseline == 744, "S3A", class)) %>%
  mutate(class = factor(class, ordered = F)) %>%
  mutate(level = substr(baseline$class,2,2)) %>%
  mutate(level1 = ifelse(level == 1, 1, 0),
         level2 = ifelse(level == 2, 1, 0),
         level3 = ifelse(level == 3, 1, 0))



### Clean student-level variables ----------------------- ----------------------- ----------------------- ----------------------- 

baseline <- baseline %>%
  mutate(age = ifelse(age == "2020" | age == "-9", "", age), 
         age = ifelse(age == "2008", "11", age),
         age = ifelse(age == "2007" | age == "21/10/2007       12", "12", age),
         age = ifelse(age == "2006", "13", age), 
         age = ifelse(age == "2005" | age == "13/9/205" | age == "14--> 15" | age == "14-15" | age == "13.5", "14", age),
         age = ifelse(age == "2004" | age == "204" | age == "15/2005" | age == "205/15", "15", age),
         age = ifelse(age == "2003", "16", age), 
         age = ifelse(age == "2002", "17", age), 
         age = ifelse(age == "2001", "18", age), 
         age = ifelse(age == "2000", "19", age), 
         age = ifelse(age == "1999", "20", age)) %>%
  mutate(age = as.numeric(age))

baseline <- baseline %>%
  mutate(gender = ifelse(gender == "M" | gender == "GABO" | name_baseline == "RAFIKI DAVID" | name_baseline == "IRADUKUNDA JAMES" | 
                         name_baseline == "IMENA LOUANGE" | name_baseline == "NIYONKURU SAMSON", "0", gender), 
         gender = ifelse(gender == "F" | gender == "GORE" | name_baseline == "ISHIMWE ANGE" | name_baseline == "TUYIRINGIRE NOELLA" | 
                         name_baseline == "UMWIZA ALICE" | name_baseline == "BISENGIMANA ANGE", "1", gender)) %>%
  mutate(gender = as.numeric(gender))

baseline <- baseline %>%
  mutate(pd_6 = ifelse(pd_6 == "1.2.2018", "01/02/2018", pd_6),
         pd_6 = ifelse(pd_6 == "43781", "12/11/2019", pd_6),
         pd_6 = ifelse(pd_6 == "43800", "01/12/2019", pd_6),
         pd_6 = ifelse(pd_6 == "43831" | pd_6 == "1", "01/01/2020", pd_6),
         pd_6 = ifelse(pd_6 == "43834" | pd_6 == "4/1/2", "04/01/2020", pd_6),
         pd_6 = ifelse(pd_6 == "6 /1/2020" | pd_6 == "6", "06/01/2020", pd_6),         
         pd_6 = ifelse(pd_6 == "7", "07/01/2020", pd_6),
         pd_6 = ifelse(pd_6 == "10", "10/01/2020", pd_6),
         pd_6 = ifelse(pd_6 == "13 MUTARAMA 2020" | pd_6 == "13/01/2019" | pd_6 == "13/1/2020" | 
                       pd_6 == "13", "13/01/2020", pd_6),
         pd_6 = ifelse(pd_6 == "14 IGIHEMBWE 1 2014" | pd_6 == "14 MUTARAMA 2020" | pd_6 == "14/1/2020" | 
                       pd_6 == "14/12/2020" | pd_6 == "14", "14/01/2020", pd_6),
         pd_6 = ifelse(pd_6 == "15 MUTARAMA 2020" | pd_6 == "15/1/2020" | pd_6 == "15/10/2020" | 
                       pd_6 == "15", "15/01/2020", pd_6),
         pd_6 = ifelse(pd_6 == "16/1/2020" | pd_6 == "16", "16/01/2020", pd_6),
         pd_6 = ifelse(pd_6 == "17/MUTARAMA 2020" | pd_6 == "17/1/2020" | pd_6 == "17", "17/01/2020", pd_6),
         pd_6 = ifelse(pd_6 == "18/1/2020", "18/01/2020", pd_6),
         pd_6 = ifelse(pd_6 == "19/1/2020", "19/01/2020", pd_6),
         pd_6 = ifelse(pd_6 == "20/1/2020" | pd_6 == "20", "20/01/2020", pd_6),
         pd_6 = ifelse(pd_6 == "21/01/2019" | pd_6 == "21/1/2020" | pd_6 == "21", "21/01/2020", pd_6),
         pd_6 = ifelse(pd_6 == "22/1/2020" | pd_6 == "22 MUTARAMA 2020" | pd_6 == "22", "22/01/2020", pd_6),
         pd_6 = ifelse(pd_6 == "24/01/2019" | pd_6 == "24/1/2020" | pd_6 == "24", "24/01/2020", pd_6),
         pd_6 = ifelse(pd_6 == "43862", "01/02/2020", pd_6),
         pd_6 = ifelse(pd_6 == "43891", "01/03/2020", pd_6),
         pd_6 = ifelse(pd_6 == "43922", "01/04/2020", pd_6),
         pd_6 = ifelse(pd_6 == "43952", "01/05/2020", pd_6),
         pd_6 = ifelse(pd_6 == "43983", "01/06/2020", pd_6),
         pd_6 = ifelse(pd_6 == "43992", "11/02/2020", pd_6),

         pd_6 = ifelse(pd_6 == "43993", "11/06/2020", pd_6),
         pd_6 = ifelse(pd_6 == "44013", "01/07/2020", pd_6),
         pd_6 = ifelse(pd_6 == "44044", "01/08/2020", pd_6),
         pd_6 = ifelse(pd_6 == "44075", "01/09/2020", pd_6),
         pd_6 = ifelse(pd_6 == "44105", "01/10/2020", pd_6),
         pd_6 = ifelse(pd_6 == "44136", "01/11/2020", pd_6),
         pd_6 = ifelse(pd_6 == "44166", "01/12/2020", pd_6),
         pd_6 = ifelse(pd_6 == "TUGITANGIRA", "-9", pd_6))



### Clean up motivation data ----------------------- ----------------------- ----------------------- -----------------------

baseline <- baseline %>%
  mutate(across(where(is.character), ~ gsub("%", "", .))) %>%
  mutate(across(where(is.character), ~ recode(., 
                                       "-99" = "",
                                       " -9" = "",
                                       "-9" = "",
                                       "1-AKENSHI KUKO NARIZI KANDI NIFUZA KURIMENYA" = "1",
                                       "1, CYANE" = "1", 
                                       "1, KAKAZIHA" = "1",
                                       "1, KIRAHARI" = "1",
                                       "1, KUVA TWATANGIRA NTANARIMWE TWARI BWARYIGE" = "1",
                                       "1,NZAKIGERAHO" = "1",
                                       "1, OYA" = "1",
                                       "1-OYA" = "1",
                                       "1-RIRAGAFITE" = "1",
                                       "1, RIRAGAFITE" = "1",
                                       "1, RIFITE AKAMARO" = "1",
                                       "1-YEGO" = "1",
                                       "YEGO" = "1",  
                                       "KUVA TWATANGIRA NTANARIMWE TWARI BWARYIGE" = "1",
                                       "NAHUNGABANA GUSHIZE" = "1",
                                       "2, AKENSHI KUGIRA NGO NDIMENYE" = "2",
                                       "2-IYO NARWAYE MBASHA KURIKORESHA NKORA IMITI" = "2",   
                                       "2-KUKO NTABUZE UWARINYIGISHA" = "2",
                                       "2, NDIHAGACIRO" = "2",
                                       "2-NTAMUTIMA NDWAYE" = "2",
                                       "2, RIRAKAMAZE" = "2",
                                       "2-SITURAYIKORA" = "2",                                       
                                       "3-NDARIGAHA" = "3",
                                       "3-IYO WARYIZE NEZA RIRAGUFASHA" = "3",
                                       "3, KURINJYE" = "3",
                                       "3-OYA" = "3",
                                       "3, YEGO" = "3",
                                       "KARAHARI" = "3",
                                       "KIRAHARI" = "3",
                                       "RIRAGAFITE" = "3",
                                       "RIRAKIMARIYE" = "3",
                                       "RIZAKIMARIRA" = "3",
                                       "RUZAKINGEZAHO" = "3",
                                       "(-9- BIZAMFASHA MU BUZIMA BWA BURI MUNSI)" = "3"))) %>%
  mutate(across(starts_with("c_"), ~ as.integer(.))) %>%
  mutate(across(starts_with("e_"), ~ as.integer(.))) %>%
  mutate(across(starts_with("k_"), ~ as.integer(.))) %>%
  mutate(across(starts_with("m_"), ~ as.integer(.))) %>%
  mutate(across(where(is.integer), ~ ifelse(. == -9 | . == -99 | . == -999, NA, .))) 



### Reverse coding of negative items ----------------------- ----------------------- ----------------------- -----------------------

positive <- c("c_ab2", "c_ab3", "c_ab4", "c_ab5", "c_ab5_f", "c_ab5_r", "c_ab6", "c_ab8", 
              "c_da1", "c_da2", "c_da3", 
              "c_ge1", "c_ge2","c_ge3", "c_ge4", "c_ge5", 
              "c_po2",
              "e_ab2", "e_ab3", "e_ab4", "e_ab6", "e_ab5", "e_ab8", 
              "e_da1", "e_da3", 
              "e_ge1", "e_ge3", "e_ge4", "e_ge5", 
              "k_ab2", "k_ab3", "k_ab4", "k_ab5", "k_ab6", "k_ab8", 
              "k_da1", "k_da3", 
              "k_ge1", "k_ge3", "k_ge4", "k_ge5", 
              "m_ab2", "m_ab3", "m_ab4", "m_ab5", "m_ab6", "m_ab8", 
              "m_da1", "m_da2", "m_da3", 
              "m_ge1", "m_ge2", "m_ge3", "m_ge4", "m_ge4_f", "m_ge4_r", "m_ge5", 
              "m_po2")

baseline <- baseline %>%
  mutate(across(positive, ~ (. - 4) * -1 )) %>%
  mutate(across(positive, ~ as.integer(.)))
rm(positive)



### Dummies for school data available [the 4 core subjects only] ----------------------- ----------------------- ----------------------- -----------------------

baseline <- baseline %>%
  mutate(school_marks = ifelse(!is.na(sno_smarks), 1, 0),
         school_marks_CAT = ifelse(is.na(Chemistry_CAT) & is.na(English_CAT) & is.na(Kinyarwanda_CAT) & is.na(Mathematics_CAT), 0, 1), 
         school_marks_EX = ifelse(is.na(Chemistry_EX) & is.na(English_EX) & is.na(Kinyarwanda_EX) & is.na(Mathematics_EX), 0, 1),
         school_marks_TOT  = ifelse(is.na(Chemistry_TOT) & is.na(English_TOT) & is.na(Kinyarwanda_TOT) & is.na(Mathematics_TOT), 0, 1),
         school_marks_Annual  = ifelse(is.na(Chemistry_Annual) & is.na(English_Annual) & is.na(Kinyarwanda_Annual) & is.na(Mathematics_Annual), 0, 1))



### Order variables ----------------------- ----------------------- ----------------------- -----------------------

baseline <- baseline %>%
  select(version, 
         sno_baseline, sno_smarks, 
         school, school1, school2, school3, school4, 
         level, level1, level2, level3, class, 
         name_baseline, name1, name_consent, name_smarks,
         consent,
         school_marks, school_marks_CAT, school_marks_EX, school_marks_TOT, school_marks_Annual,
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



### Create Mplus datasets ----------------------- ----------------------- ----------------------- -----------------------

baseline_mplus <- baseline %>%
  select(-c(sno_smarks, school, level, name_baseline, name1, name_consent, name_smarks, class, pd_6, pd_7, pd_8, version))
for (var in 1 : ncol(baseline_mplus)) {  
  baseline_mplus[, var][is.na(baseline_mplus[, var])] <- -999
}
rm(var)



### Save data ----------------------- ----------------------- ----------------------- -----------------------

write.csv(baseline, "02 processed data/baseline_20241229.csv", row.names = FALSE)
write.table(baseline_mplus, "02 processed data/baseline_mplus_20241229.csv", sep=",", row.names = FALSE, col.names = FALSE)

colnames(baseline_mplus)



### Clean up data imputations ----------------------- ----------------------- ----------------------- -----------------------

filenames <- list.files("03 scripts/Mplus", pattern="*.csv")
filenames <- filenames[grep("raw", filenames)]
listname <- filenames[grep("list", filenames)]
filenames <- filenames[-grep("list", filenames)]
cnames <- c("AGE",	"GENDER",					
            "SCHOOL1",	"SCHOOL2",	"SCHOOL3",				
            "LEVEL2",	"LEVEL3",					
            "C_AB2",	"C_AB3",	"C_AB4",	"C_AB5",	"C_AB6",	"C_AB7",	"C_AB8",
            "C_DA1",	"C_DA2",	"C_DA3",				
            "C_EF2",	"C_EF3",	"C_EF5",				
            "C_GE1",	"C_GE2",	"C_GE3",	"C_GE4",	"C_GE5",		
            "C_NE2",	"C_NE4",					
            "C_PI1",	"C_PI2",	"C_PI3",	"C_PI4",			
            "C_PO2",						
            "E_AB2",	"E_AB3",	"E_AB4",	"E_AB5",	"E_AB6",	"E_AB8",	
            "E_DA1",	"E_DA3",					
            "E_EF2",	"E_EF3",	"E_EF5",				
            "E_GE1",	"E_GE3",	"E_GE4",	"E_GE5",			
            "E_NE1",	"E_NE2",	"E_NE3",	"E_NE4",			
            "E_OE2",						
            "E_PI2",	"E_PI3",	"E_PI4",				
            "K_AB2",	"K_AB3",	"K_AB4",	"K_AB5",	"K_AB6",	"K_AB8",	
            "K_DA1",	"K_DA3",	"K_EF2",				
            "K_EF3",	"K_EF5",	"K_GE1",				
            "K_GE3",	"K_GE4",	"K_GE5",				
            "K_NE1",	"K_NE2",	"K_NE3",	"K_NE4",			
            "K_OE2",						
            "K_PI2",	"K_PI3",	"K_PI4",				
            "M_AB2",	"M_AB3",	"M_AB4",	"M_AB5",	"M_AB6",	"M_AB7",	"M_AB8",
            "M_DA1",	"M_DA2",	"M_DA3",				
            "M_EF2",	"M_EF3",	"M_EF5",				
            "M_GE1",	"M_GE2",	"M_GE3",	"M_GE4",	"M_GE5",		
            "M_NE2",	"M_NE4",					
            "M_PI1",	"M_PI2",	"M_PI3",	"M_PI4",			
            "M_PO2",						
            "SNO_BASE",						
            "SCHOOL4",						
            "LEVEL1",						
            "school_marks",	"school_marks_CAT",	"school_marks_EX",	"school_marks_TOT",	"school_marks_Annual",		
            "MISTAKE",						
            "C_AB5_R",	"M_EF5_R",	"M_GE4_R",	"M_NE2_R",			
            "C_AB5_F",	"M_EF5_F",	"M_GE4_F",	"M_NE2_F",			
            "Biology_CAT",	"Biology_EX",	"Biology_TOT",	"Biology_Annual",			
            "Chemistry_CAT",	"Chemistry_EX",	"Chemistry_TOT",	"Chemistry_Annual",			
            "Entrepreneurship_CAT",	"Entrepreneurship_EX",	"Entrepreneurship_TOT",	"Entrepreneurship_Annual",			
            "English_CAT",	"English_EX",	"English_TOT",	"English_Annual",			
            "French_CAT",	"French_EX",	"French_TOT",	"French_Annual",			
            "Geography_CAT",	"Geography_EX",	"Geography_TOT",	"Geography_Annual",			
            "History_CAT",	"History_EX",	"History_TOT",	"History_Annual",			
            "ICT_CAT",	"ICT_EX",	"ICT_TOT",	"ICT_Annual",			
            "Kinyarwanda_CAT",	"Kinyarwanda_EX",	"Kinyarwanda_TOT",	"Kinyarwanda_Annual",			
            "Kiswahili_CAT",	"Kiswahili_EX",	"Kiswahili_TOT",	"Kiswahili_Annual",			
            "Literature_CAT",	"Literature_EX",	"Literature_TOT",	"Literature_Annual",			
            "Mathematics_CAT",	"Mathematics_EX",	"Mathematics_TOT",	"Mathematics_Annual",			
            "Physics_CAT",	"Physics_EX",	"Physics_TOT",	"Physics_Annual",			
            "Religion_CAT",	"Religion_EX",	"Religion_TOT",	"Religion_Annual",			
            "Sports_CAT",	"Sports_EX",	"Sports_TOT",	"Sports_Annual")
motivation <- c("C_AB2",	"C_AB3",	"C_AB4",	"C_AB5",	"C_AB6",	"C_AB7",	"C_AB8",
                "C_DA1",	"C_DA2",	"C_DA3",				
                "C_EF2",	"C_EF3",	"C_EF5",				
                "C_GE1",	"C_GE2",	"C_GE3",	"C_GE4",	"C_GE5",		
                "C_NE2",	"C_NE4",					
                "C_PI1",	"C_PI2",	"C_PI3",	"C_PI4",			
                "C_PO2",						
                "E_AB2",	"E_AB3",	"E_AB4",	"E_AB5",	"E_AB6",	"E_AB8",	
                "E_DA1",	"E_DA3",					
                "E_EF2",	"E_EF3",	"E_EF5",				
                "E_GE1",	"E_GE3",	"E_GE4",	"E_GE5",			
                "E_NE1",	"E_NE2",	"E_NE3",	"E_NE4",			
                "E_OE2",						
                "E_PI2",	"E_PI3",	"E_PI4",				
                "K_AB2",	"K_AB3",	"K_AB4",	"K_AB5",	"K_AB6",	"K_AB8",	
                "K_DA1",	"K_DA3",	
                "K_EF2",  "K_EF3",	"K_EF5",	
                "K_GE1",  "K_GE3",	"K_GE4",	"K_GE5",				
                "K_NE1",	"K_NE2",	"K_NE3",	"K_NE4",			
                "K_OE2",						
                "K_PI2",	"K_PI3",	"K_PI4",				
                "M_AB2",	"M_AB3",	"M_AB4",	"M_AB5",	"M_AB6",	"M_AB7", "M_AB8",
                "M_DA1",	"M_DA2",	"M_DA3",				
                "M_EF2",	"M_EF3",	"M_EF5",				
                "M_GE1",	"M_GE2",	"M_GE3",	"M_GE4",	"M_GE5",		
                "M_NE2",	"M_NE4",					
                "M_PI1",	"M_PI2",	"M_PI3",	"M_PI4",			
                "M_PO2")
cnames <- tolower(cnames)
motivation <- tolower(motivation)

for(file in filenames) {
  file <- paste0("03 scripts/Mplus/", file)
  data <- as.data.frame(read.csv(file, header = FALSE))
  data$V1 <- gsub(" ", "x", data$V1)
  data$V1 <- gsub("xxxxx", "x", data$V1)
  data$V1 <- gsub("xxxx", "x", data$V1)
  data$V1 <- gsub("xxx", "x", data$V1)
  data$V1 <- gsub("xx", "x", data$V1)
  data <- as.data.frame(do.call('rbind', strsplit(as.character(data$V1),"x",fixed=TRUE)))
  data$V1 <- NULL
  colnames(data) <- cnames
  data[] <- sapply(data,function(x) as.numeric(gsub("\\*","-999",as.character(x))))
  data <- data %>% 
    mutate(age = as.integer(age)) %>%
    mutate(location = ifelse(school1 == 1 | school3 == 1, 1, 0)) %>%
    select(school1, school2, school3, school4, location, everything())
  
  data <- data %>%
    select(sno_base,						
           school1,	school2,	school3,	school4,			
           level1,	level2,	level3,				
           age,	gender,					
           c_ab2,	c_ab3,	c_ab4,	c_ab5,	c_ab6,	c_ab7,	c_ab8,
           c_da1,	c_da2,	c_da3,				
           c_ef2,	c_ef3,	c_ef5,				
           c_ge1,	c_ge2,	c_ge3,	c_ge4,	c_ge5,		
           c_ne2,	c_ne4,					
           c_pi1,	c_pi2,	c_pi3,	c_pi4,			
           c_po2,						
           e_ab2,	e_ab3,	e_ab4,	e_ab5,	e_ab6,	e_ab8,	
           e_da1,	e_da3,					
           e_ef2,	e_ef3,	e_ef5,				
           e_ge1,	e_ge3,	e_ge4,	e_ge5,			
           e_ne1,	e_ne2,	e_ne3,	e_ne4,			
           e_oe2,						
           e_pi2,	e_pi3,	e_pi4,				
           k_ab2,	k_ab3,	k_ab4,	k_ab5,	k_ab6,	k_ab8,	
           k_da1,	k_da3,	k_ef2,				
           k_ef3,	k_ef5,	k_ge1,				
           k_ge3,	k_ge4,	k_ge5,				
           k_ne1,	k_ne2,	k_ne3,	k_ne4,			
           k_oe2,						
           k_pi2,	k_pi3,	k_pi4,				
           m_ab2,	m_ab3,	m_ab4,	m_ab5,	m_ab6,	m_ab7,	m_ab8,
           m_da1,	m_da2,	m_da3,				
           m_ef2,	m_ef3,	m_ef5,				
           m_ge1,	m_ge2,	m_ge3,	m_ge4,	m_ge5,		
           m_ne2,	m_ne4,					
           m_pi1,	m_pi2,	m_pi3,	m_pi4,			
           m_po2,						
           school_marks,	school_marks_cat,	school_marks_ex,	school_marks_tot,	school_marks_annual,		
           mistake,						
           c_ab5_r,	m_ef5_r,	m_ge4_r,	m_ne2_r,			
           c_ab5_f,	m_ef5_f,	m_ge4_f,	m_ne2_f,			
           biology_cat,	biology_ex,	biology_tot,	biology_annual,			
           chemistry_cat,	chemistry_ex,	chemistry_tot,	chemistry_annual,			
           entrepreneurship_cat,	entrepreneurship_ex,	entrepreneurship_tot,	entrepreneurship_annual,			
           english_cat,	english_ex,	english_tot,	english_annual,			
           french_cat,	french_ex,	french_tot,	french_annual,			
           geography_cat,	geography_ex,	geography_tot,	geography_annual,			
           history_cat,	history_ex,	history_tot,	history_annual,			
           ict_cat,	ict_ex,	ict_tot,	ict_annual,			
           kinyarwanda_cat,	kinyarwanda_ex,	kinyarwanda_tot,	kinyarwanda_annual,			
           kiswahili_cat,	kiswahili_ex,	kiswahili_tot,	kiswahili_annual,			
           literature_cat,	literature_ex,	literature_tot,	literature_annual,			
           mathematics_cat,	mathematics_ex,	mathematics_tot,	mathematics_annual,			
           physics_cat,	physics_ex,	physics_tot,	physics_annual,			
           religion_cat,	religion_ex,	religion_tot,	religion_annual,			
           sports_cat,	sports_ex,	sports_tot,	sports_annual)
  filename <- gsub("03 scripts/Mplus/", "02 processed data/", file)
  filename <- gsub("raw_", "", filename)
  write.table(data, file = filename, sep=",", row.names = F, col.names=FALSE)
  
  ### in general, the first answer option is hardly selected. Thus, we create a two-options dataset
  
  data_bi <- data %>%
    mutate(across(motivation, ~ ifelse(. < 3 & . != -999, 0, .))) %>%
    mutate(across(motivation, ~ ifelse(. == 3, 1, .)))
  filename <- gsub("baseline_", "baseline_BI_", filename)
  write.table(data_bi, file = filename, sep=",", row.names = FALSE, col.names = FALSE)
  
}

for(file in listname[1]) {
  file <- paste0("03 scripts/Mplus/", file)
  data <- as.data.frame(read.csv(file, header = FALSE))
  data <- data %>%
    mutate(V1 = gsub("raw_", "", V1))
  filename <- gsub("03 scripts/Mplus/", "02 processed data/", file)
  filename <- gsub("raw_", "", filename)
  write.table(data, file = filename, row.names = F,  col.names=FALSE, quote = FALSE)
  
  ### in general, the first answer option is hardly selected. Thus, we create a two-options dataset
  
  data_bi <- data %>%
    mutate(V1 = gsub("baseline_", "baseline_BI_", V1))
  filename <- gsub("baseline_", "baseline_BI_", filename)
  write.table(data_bi, file = gsub("BL_", "BL_bi_",filename), sep=",", row.names = F, col.names=FALSE)
}
rm(data, data_bi, file, filename, filenames, cnames, listname, motivation)


