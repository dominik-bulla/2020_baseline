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

baseline <- read.csv("02 processed data/baseline_20241229.csv")



### Data analysis (proportion of missingness within motivation data) ----------------------- ----------------------- ----------------------- -----------------------

motivation1 <- colnames(baseline)[grep("c_", colnames(baseline))]
motivation2 <- colnames(baseline)[grep("e_", colnames(baseline))]
motivation2 <- motivation2[-grep("Lit", motivation2)]
motivation2 <- motivation2[-grep("name", motivation2)]
motivation3 <- colnames(baseline)[grep("k_", colnames(baseline))]
motivation4 <- colnames(baseline)[grep("m_", colnames(baseline))]
motivation <- c(motivation1, motivation2, motivation3, motivation4)
motivation <- motivation[-grep("_r", motivation)]
motivation <- motivation[-grep("_f", motivation)]
rm(motivation1, motivation2, motivation3, motivation4)

motivation_included <- c("ab2", "ab3", "ab4", "ab5", "ab6", "ab8", 
                         "ef2", "ef3", "ef5", 
                         "ge1", "ge3", "ge4", "ge5")
motivation_included1 <- paste0("c_", motivation_included)
motivation_included2 <- paste0("e_", motivation_included)
motivation_included3 <- paste0("k_", motivation_included)
motivation_included4 <- paste0("m_", motivation_included)
motivation_included <- c(motivation_included1, motivation_included2, motivation_included3, motivation_included4)
rm(motivation_included1, motivation_included2, motivation_included3, motivation_included4)

missingness_included <- data.frame(included = rep(1, length(motivation_included)))
row.names(missingness_included) <- motivation_included
motivation_included_not <- motivation[-which(motivation %in% motivation_included)]
missingness_included2 <- data.frame(included = rep(0, length(motivation_included_not)))
row.names(missingness_included2) <- motivation_included_not

missingness_included <- rbind(missingness_included, missingness_included2)
missingness_included <- missingness_included %>% 
  arrange(row.names(missingness_included))
missingness_included2 <- data.frame(included = NA)
row.names(missingness_included2) <- ""
missingness_included <- rbind(missingness_included, missingness_included2)
rm(motivation_included, motivation_included_not, missingness_included2)
missingness_motivation <- as.data.frame(summary(baseline[, motivation])[7, ])
row.names(missingness_motivation) <- motivation
missingness_motivation <- missingness_motivation %>%
  rename("all" = `summary(baseline[, motivation])[7, ]`) %>%
  mutate(all = as.numeric(sub(".*:", "", all))) %>%
  mutate(`all (in %)` = round(all / nrow(baseline), 4) * 100)
Total <- data.frame(a = sum(missingness_motivation$all), 
                    b = round(sum(missingness_motivation$all) / (nrow(baseline) * length(motivation)), 4) * 100)
colnames(Total) <- colnames(missingness_motivation)
row.names(Total) <- "Total/ average"
missingness_motivation <- rbind(missingness_motivation, Total)

missingness_motivation_kagarama <- as.data.frame(summary(baseline[baseline$school1 == 1, motivation])[7, ])
row.names(missingness_motivation_kagarama) <- motivation
missingness_motivation_kagarama <- missingness_motivation_kagarama %>%
  rename("Kagarama" = `summary(baseline[baseline$school1 == 1, motivation])[7, ]`) %>%
  mutate(Kagarama  = as.numeric(sub(".*:", "", Kagarama))) %>%
  mutate(Kagarama = ifelse(is.na(Kagarama), 0, Kagarama)) %>%
  mutate(`Kagarama (in %)` = round(Kagarama / nrow(baseline[baseline$school1 == 1, ]), 4) * 100)
Total <- data.frame(a = sum(missingness_motivation_kagarama$Kagarama), 
                    b = round(sum(missingness_motivation_kagarama$Kagarama) / (nrow(baseline[baseline$school1 == 1, ]) * length(motivation)), 4) * 100)
colnames(Total) <- colnames(missingness_motivation_kagarama)
row.names(Total) <- "Total/ average"
missingness_motivation_kagarama <- rbind(missingness_motivation_kagarama, Total)

missingness_motivation_nyamata <- as.data.frame(summary(baseline[baseline$school2 == 1, motivation])[7, ])
row.names(missingness_motivation_nyamata) <- motivation
missingness_motivation_nyamata <- missingness_motivation_nyamata %>%
  rename("Nyamata" = `summary(baseline[baseline$school2 == 1, motivation])[7, ]`) %>%
  mutate(Nyamata = as.numeric(sub(".*:", "", Nyamata))) %>%
  mutate(Nyamata = ifelse(is.na(Nyamata), 0, Nyamata)) %>%
  mutate(`Nyamata (in %)` = round(Nyamata / nrow(baseline[baseline$school2 == 1, ]), 4) * 100)
Total <- data.frame(a = sum(missingness_motivation_nyamata$Nyamata, na.rm = TRUE), 
                    b = round(sum(missingness_motivation_nyamata$Nyamata, na.rm = TRUE) / (nrow(baseline[baseline$school2 == 1, ]) * length(motivation)), 4) * 100)
colnames(Total) <- colnames(missingness_motivation_nyamata)
row.names(Total) <- "Total/ average"
missingness_motivation_nyamata <- rbind(missingness_motivation_nyamata, Total)

missingness_motivation_nyanza <- as.data.frame(summary(baseline[baseline$school3 == 1, motivation])[7, ])
row.names(missingness_motivation_nyanza) <- motivation
missingness_motivation_nyanza <- missingness_motivation_nyanza %>%
  rename("Nyanza" = `summary(baseline[baseline$school3 == 1, motivation])[7, ]`) %>%
  mutate(Nyanza = as.numeric(sub(".*:", "", Nyanza))) %>%
  mutate(Nyanza = ifelse(is.na(Nyanza), 0, Nyanza)) %>%
  mutate(`Nyanza (in %)` = round(Nyanza / nrow(baseline[baseline$school3 == 1, ]), 4) * 100)
Total <- data.frame(a = sum(missingness_motivation_nyanza$Nyanza, na.rm = TRUE), 
                    b = round(sum(missingness_motivation_nyanza$Nyanza, na.rm = TRUE) / (nrow(baseline[baseline$school3 == 1, ]) * length(motivation)), 4) * 100)
colnames(Total) <- colnames(missingness_motivation_nyanza)
row.names(Total) <- "Total/ average"
missingness_motivation_nyanza <- rbind(missingness_motivation_nyanza, Total)

missingness_motivation_rango <- as.data.frame(summary(baseline[baseline$school4 == 1, motivation])[7, ])
row.names(missingness_motivation_rango) <- motivation
missingness_motivation_rango <- missingness_motivation_rango %>%
  rename("Rango" = `summary(baseline[baseline$school4 == 1, motivation])[7, ]`) %>%
  mutate(Rango = as.numeric(sub(".*:", "", Rango))) %>%
  mutate(Rango = ifelse(is.na(Rango), 0, Rango)) %>%
  mutate(`Rango (in %)` = round(Rango / nrow(baseline[baseline$school4 == 1, ]), 4) * 100)
Total <- data.frame(a = sum(missingness_motivation_rango$Rango, na.rm = TRUE), 
                    b = round(sum(missingness_motivation_rango$Rango, na.rm = TRUE) / (nrow(baseline[baseline$school4 == 1, ]) * length(motivation)), 4) * 100)
colnames(Total) <- colnames(missingness_motivation_rango)
row.names(Total) <- "Total/ average"
missingness_motivation_rango <- rbind(missingness_motivation_rango, Total)

missingness_motivation <- cbind(missingness_included, missingness_motivation, missingness_motivation_kagarama, missingness_motivation_nyamata, missingness_motivation_nyanza, missingness_motivation_rango)
missingness_motivation2 <- missingness_motivation[nrow(missingness_motivation), ]
missingness_motivation <- missingness_motivation[1 : (nrow(missingness_motivation) - 1), ] 
missingness_motivation <- missingness_motivation %>%
  arrange(desc(`all (in %)`)) 
missingness_motivation <- rbind(missingness_motivation, missingness_motivation2)
rm(missingness_motivation2, missingness_motivation_kagarama, missingness_motivation_nyamata, missingness_motivation_nyanza, missingness_motivation_rango, motivation, Total)



### Data analysis (proportion of missingness within school-mark data) ----------------------- ----------------------- ----------------------- -----------------------

mark1 <- colnames(baseline)[grep("Chemistry_", colnames(baseline))]
mark2 <- colnames(baseline)[grep("English_", colnames(baseline))]
mark3 <- colnames(baseline)[grep("Kinyarwanda_", colnames(baseline))]
mark4 <- colnames(baseline)[grep("Mathematics_", colnames(baseline))]
mark <- c(mark1, mark2, mark3, mark4)
rm(mark1, mark2, mark3, mark4)

missingness_n <- as.data.frame(summary(baseline[, mark])[7, ])
row.names(missingness_n) <- mark
missingness_n <- missingness_n %>%
  rename("n" = `summary(baseline[, mark])[7, ]`) %>%
  mutate(n = as.numeric(sub(".*:", "", n))) %>%
  mutate(n = nrow(baseline) - n)

Total <- data.frame(a = max(missingness_n$n))
colnames(Total) <- colnames(missingness_n)
row.names(Total) <- "Max/ average"
missingness_n <- rbind(missingness_n, Total)

missingness_marks <- as.data.frame(summary(baseline[baseline$school_marks == 1, mark])[7, ])
row.names(missingness_marks) <- mark
missingness_marks <- missingness_marks %>%
  rename("all" = `summary(baseline[baseline$school_marks == 1, mark])[7, ]`) %>%
  mutate(all = as.numeric(sub(".*:", "", all))) %>%
  mutate(all = ifelse(is.na(all), 0, all)) %>%
  mutate(`all (in %)` = round(all / nrow(baseline[baseline$school_marks == 1, ]), 4) * 100)
Total <- data.frame(a = max(missingness_marks$all), 
                    b = round(sum(missingness_marks$all) / (nrow(baseline[baseline$school_marks == 1, ]) * length(mark)), 4) * 100)
colnames(Total) <- colnames(missingness_marks)
row.names(Total) <- "Max/ average"
missingness_marks <- rbind(missingness_marks, Total)

missingness_marks_kagarama <- as.data.frame(summary(baseline[baseline$school_marks == 1 & baseline$school1 == 1, mark])[7, ])
row.names(missingness_marks_kagarama) <- mark
missingness_marks_kagarama <- missingness_marks_kagarama %>%
  rename("Kagarama" = `summary(baseline[baseline$school_marks == 1 & baseline$school1 == 1, mark])[7, ]`) %>%
  mutate(Kagarama  = as.numeric(sub(".*:", "", Kagarama))) %>%
  mutate(Kagarama = ifelse(is.na(Kagarama), 0, Kagarama)) %>%
  mutate(`Kagarama (in %)` = round(Kagarama / nrow(baseline[baseline$school_marks == 1 & baseline$school1 == 1, mark]), 4) * 100)
Total <- data.frame(a = max(missingness_marks_kagarama$Kagarama), 
                    b = round(sum(missingness_marks_kagarama$Kagarama) / (nrow(baseline[baseline$school_marks == 1 & baseline$school1 == 1, mark]) * length(mark)), 4) * 100)
colnames(Total) <- colnames(missingness_marks_kagarama)
row.names(Total) <- "Max/ average"
missingness_marks_kagarama <- rbind(missingness_marks_kagarama, Total)

missingness_marks_nyamata <- as.data.frame(summary(baseline[baseline$school_marks == 1 & baseline$school2 == 1, mark])[7, ])
row.names(missingness_marks_nyamata) <- mark
missingness_marks_nyamata <- missingness_marks_nyamata %>%
  rename("Nyamata" = `summary(baseline[baseline$school_marks == 1 & baseline$school2 == 1, mark])[7, ]`) %>%
  mutate(Nyamata  = as.numeric(sub(".*:", "", Nyamata))) %>%
  mutate(Nyamata = ifelse(is.na(Nyamata), 0, Nyamata)) %>%
  mutate(`Nyamata (in %)` = round(Nyamata / nrow(baseline[baseline$school_marks == 1 & baseline$school2 == 1, mark]), 4) * 100)
Total <- data.frame(a = max(missingness_marks_nyamata$Nyamata), 
                    b = round(sum(missingness_marks_nyamata$Nyamata) / (nrow(baseline[baseline$school_marks == 1 & baseline$school2 == 1, mark]) * length(mark)), 4) * 100)
colnames(Total) <- colnames(missingness_marks_nyamata)
row.names(Total) <- "Max/ average"
missingness_marks_nyamata <- rbind(missingness_marks_nyamata, Total)

missingness_marks_nyanza <- as.data.frame(summary(baseline[baseline$school_marks == 1 & baseline$school3 == 1, mark])[7, ])
row.names(missingness_marks_nyanza) <- mark
missingness_marks_nyanza <- missingness_marks_nyanza %>%
  rename("Nyanza" = `summary(baseline[baseline$school_marks == 1 & baseline$school3 == 1, mark])[7, ]`) %>%
  mutate(Nyanza  = as.numeric(sub(".*:", "", Nyanza))) %>%
  mutate(Nyanza = ifelse(is.na(Nyanza), 0, Nyanza)) %>%
  mutate(`Nyanza (in %)` = round(Nyanza / nrow(baseline[baseline$school_marks == 1 & baseline$school3 == 1, mark]), 4) * 100)
Total <- data.frame(a = max(missingness_marks_nyanza$Nyanza), 
                    b = round(sum(missingness_marks_nyanza$Nyanza) / (nrow(baseline[baseline$school_marks == 1 & baseline$school3 == 1, mark]) * length(mark)), 4) * 100)
colnames(Total) <- colnames(missingness_marks_nyanza)
row.names(Total) <- "Max/ average"
missingness_marks_nyanza <- rbind(missingness_marks_nyanza, Total)

missingness_marks_rango <- as.data.frame(summary(baseline[baseline$school_marks == 1 & baseline$school4 == 1, mark])[7, ])
row.names(missingness_marks_rango) <- mark
missingness_marks_rango <- missingness_marks_rango %>%
  rename("Rango" = `summary(baseline[baseline$school_marks == 1 & baseline$school4 == 1, mark])[7, ]`) %>%
  mutate(Rango  = as.numeric(sub(".*:", "", Rango))) %>%
  mutate(Rango = ifelse(is.na(Rango), 0, Rango)) %>%
  mutate(`Rango (in %)` = round(Rango / nrow(baseline[baseline$school_marks == 1 & baseline$school4 == 1, mark]), 4) * 100)
Total <- data.frame(a = max(missingness_marks_rango$Rango), 
                    b = round(sum(missingness_marks_rango$Rango) / (nrow(baseline[baseline$school_marks == 1 & baseline$school4 == 1, mark]) * length(mark)), 4) * 100)
colnames(Total) <- colnames(missingness_marks_rango)
row.names(Total) <- "Max/ average"
missingness_marks_rango <- rbind(missingness_marks_rango, Total)
missingness_marks <- cbind(missingness_n, missingness_marks, missingness_marks_kagarama, missingness_marks_nyamata, missingness_marks_nyanza, missingness_marks_rango)
rm(missingness_marks_kagarama, missingness_marks_nyamata, missingness_marks_nyanza, missingness_marks_rango, missingness_n, mark, Total)



### save data ----------------------- ----------------------- ----------------------- -----------------------

write.csv(missingness_motivation, "04 results/04.1 Tables/missingness_motivation_20241229_v01.csv", row.names = TRUE)
write.csv(missingness_marks, "04 results/04.1 Tables/missingness_marks_20241229_v01.csv", row.names = TRUE)



