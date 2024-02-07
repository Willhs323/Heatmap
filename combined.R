rm(list=ls(all=T))
options(stringsASFactors = F) # no automatic data transformation
options("scipen" = 100, "digits" = 4) # supress math annotation

library(ggplot2)
library(reshape2)
library(tidyverse)
library(epitools)
library(readxl)
library(writexl)

#####
# use this one
# Remove unknown and other from numerator only
setwd('~/ANAMS paper/AAIP presentation/')
residents <- read.csv('residents.csv', na.strings = c("", "NA"))
residents <- as.data.frame(residents)
residents <- residents[,-c(1)]
residents <- as.data.frame(residents)
View(residents)

setwd('~/ANAMS paper/Heatmap/')
y2020 <- read.csv('2020 combined final.csv', na.strings = c("", "NA"))
y2020 <- as.data.frame(y2020)
y2020 <- y2020[,-c(1)]
y2020 <- as.data.frame(y2020)
View(y2020)

setwd('~/ANAMS paper/Heatmap/2021 files/')
res2021 <- read.csv('2021 combined.csv', na.strings = c("", "NA"))
res2021 <- as.data.frame(res2021)
res2021 <- res2021[,-c(1)]
res2021 <- as.data.frame(res2021)
View(res2021)

setwd('~/ANAMS paper/Heatmap/2023 files/')
res2023 <- read.csv('2023.csv', na.strings = c("", "NA"))
res2023 <- as.data.frame(res2023)
res2023 <- res2023[,-c(1)]
res2023 <- as.data.frame(res2023)
View(res2023)

total <- matrix(0,35,10)
for (i in 1:35) {
  for (j in 1:10) {
    total[i,j] <- residents[i,j] + y2020[i,j] + res2021[i,j] + res2023[i,j]
  }
}

rawtotal <- total
average <- rawtotal / 4
total <- total[,-c(8)]

# Print total spreadsheet
    rownames(rawtotal) <- c("Aerospace Medicine", "Allergy & Immunology", "Anesthesiology", "Child Neurology",
                       "Dermatology", "Emergency Medicine", "Family Medicine", "Internal Medicine", "Med-Peds", 
                       "Medical Genetics", "Neurosurgery", "Neurology", "Nuclear Medicine", "OB/GYN",
                       "Occupational & Environmental Medicine", "Ophthalmology", "Orthopedic Surgery", 
                       "Osteopathic Neuromuscular Medicine", "Otolaryngology", "Pain Medicine", "Pathology",
                       "Pediatrics", "PM&R", "Plastic Surgery", "Psychiatry", "Public Health & Preventative Medicine",
                       "Radiation Oncology", "Radiology", "Interventional Radiology", "General Surgery",
                       "Thoracic Surgery", "Transitional year", "Urology", "Vascular Surgery", "Total"
    )
    colnames(rawtotal) <- c("AI/AN", "Asian", "Black", "Hispanic", "Native Hawaiian", "White", "Other", "Unknown", "Non-US Citizen", "Unduplicated Total")
    rownames(average) <- c("Aerospace Medicine", "Allergy & Immunology", "Anesthesiology", "Child Neurology",
                            "Dermatology", "Emergency Medicine", "Family Medicine", "Internal Medicine", "Med-Peds", 
                            "Medical Genetics", "Neurosurgery", "Neurology", "Nuclear Medicine", "OB/GYN",
                            "Occupational & Environmental Medicine", "Ophthalmology", "Orthopedic Surgery", 
                            "Osteopathic Neuromuscular Medicine", "Otolaryngology", "Pain Medicine", "Pathology",
                            "Pediatrics", "PM&R", "Plastic Surgery", "Psychiatry", "Public Health & Preventative Medicine",
                            "Radiation Oncology", "Radiology", "Interventional Radiology", "General Surgery",
                            "Thoracic Surgery", "Transitional year", "Urology", "Vascular Surgery", "Total"
    )
    colnames(average) <- c("AI/AN", "Asian", "Black", "Hispanic", "Native Hawaiian", "White", "Other", "Unknown", "Non-US Citizen", "Unduplicated Total")
    View(rawtotal)
    View(average)
    average[,10] / average[35,10] * 100
    sort(average[,10], decreasing = TRUE)
    sort(average[,10] / average[35,10] * 100, decreasing = TRUE)
    setwd('~/ANAMS paper/Heatmap/Journal 3/')
    write.csv(average, file = "Average counts, 2019-2023")
    rawtotal[35,] / rawtotal[35,10] * 100
    rawtotal[,10] / 4
    sort(x = rawtotal[,10]/4,decreasing = TRUE)
    sort(x = rawtotal[,10]/rawtotal[35,10]*100,decreasing = TRUE)
    
    ## Start Appendix ##
    y1920 <- y2020
    y2021 <- res2021
    y2122 <- residents
    y2223 <- res2023
      View(y1920)
      View(y2021)
      View(y2122)
      View(y2223)
    # Appendix 1 - change in ethnicity by year
      apx1 <- matrix(0,10,7)
      pctchange <- (y2223[35,] - y1920[35,]) / y1920[35,] * 100
      pctchange
      
      View(apx1)
      # for loop
      for (i in 1:10) {
        apx1[i,1] <- y1920[35,i]
        apx1[i,2] <- y2021[35,i]
        apx1[i,3] <- y2122[35,i]
        apx1[i,4] <- y2223[35,i]
        apx1[i,5] <- (y1920[35,i] + y2021[35,i] + y2122[35,i] + y2223[35,i]) /4
        avgpct <- ((y1920[35,i] + y2021[35,i] + y2122[35,i] + y2223[35,i]) / 4) / ((y1920[35,10] + y2021[35,10] + y2122[35,10] + y2223[35,10])/4)*100
        apx1[i,6] <- paste0(round(as.numeric(avgpct),1),"%")
        apx1[i,7] <- paste0(round(as.numeric(pctchange[i]),0),"%")
      }
      rownames(apx1) <- c("AI/AN", "Asian", "Black", "Hispanic", "Native Hawaiian", "White", "Other", "Unknown", "Non-US Citizen", "Unduplicated Total")
      colnames(apx1) <- c("2019-20", "2020-21", "2021-22", "2022-23", "Average", "Avg pct", "Percent Change")
      apx1 <- as.data.frame(apx1)
      setwd('~/ANAMS paper/Heatmap/Journal 3/')
      write_csv(apx1, "Appendix 1")
      View(apx1)
    # Appendix 2 - change in specialty by year
      apx2 <- matrix(0,35,7)
      View(apx2)
      pctchangespecialty <- (y2223[,10] - y1920[,10]) / y1920[,10] * 100
      pctchangespecialty
      rownames(apx2) <- c("Aerospace Medicine", "Allergy & Immunology", "Anesthesiology", "Child Neurology",
                          "Dermatology", "Emergency Medicine", "Family Medicine", "Internal Medicine", "Med-Peds", 
                          "Medical Genetics", "Neurosurgery", "Neurology", "Nuclear Medicine", "OB/GYN",
                          "Occupational & Environmental Medicine", "Ophthalmology", "Orthopedic Surgery", 
                          "Osteopathic Neuromuscular Medicine", "Otolaryngology", "Pain Medicine", "Pathology",
                          "Pediatrics", "PM&R", "Plastic Surgery", "Psychiatry", "Public Health & Preventative Medicine",
                          "Radiation Oncology", "Radiology", "Interventional Radiology", "General Surgery",
                          "Thoracic Surgery", "Transitional year", "Urology", "Vascular Surgery", "Total"
      )
      colnames(apx2) <- c("2019-20", "2020-21", "2021-22", "2022-23", "Average", "Avg Pct", "Percent Change")
      for (i in 1:35) {
          apx2[i,1] <- y1920[i,10]
          apx2[i,2] <- y2021[i,10]
          apx2[i,3] <- y2122[i,10]
          apx2[i,4] <- y2223[i,10]
          apx2[i,5] <- ((y1920[i,10] + y2021[i,10] + y2122[i,10] + y2223[i,10])/4)
          avgpct <- ((y1920[i,10] + y2021[i,10] + y2122[i,10] + y2223[i,10])/4) / ((y1920[35,10] +y2021[35,10] + y2122[35,10] + y2223[35,10])/4) * 100
          apx2[i,6] <- paste0(round(as.numeric(avgpct),1),"%")
          apx2[i,7] <- paste0(round(as.numeric(pctchangespecialty[i]),0),"%")
        }      
      View(apx2)      
## start Making Heatmap ##
    heatmap <- matrix(0, 34, 8)
    rownames(heatmap) <- c("Aerospace Medicine", "Allergy & Immunology", "Anesthesiology", "Child Neurology",
                           "Dermatology", "Emergency Medicine", "Family Medicine", "Internal Medicine", "Med-Peds", 
                           "Medical Genetics", "Neurosurgery", "Neurology", "Nuclear Medicine", "OB/GYN",
                           "Occupational & Environmental Medicine", "Ophthalmology", "Orthopedic Surgery", 
                           "Osteopathic Neuromuscular Medicine", "Otolaryngology", "Pain Medicine", "Pathology",
                           "Pediatrics", "PM&R", "Plastic Surgery", "Psychiatry", "Public Health & Preventative Medicine",
                           "Radiation Oncology", "Radiology", "Interventional Radiology", "General Surgery",
                           "Thoracic Surgery", "Transitional year", "Urology", "Vascular Surgery"
    )
    colnames(heatmap) <- c("AI/AN", "Asian", "Black", "Hispanic", "Native Hawaiian", "White", "Other", "Non-US Citizen")
    View(heatmap)
    
    heatmaporhigh <- matrix(0, 34, 8)
    heatmaporlow <- matrix(0, 34, 8)
    
## Loop to make OR ## - 95% Confidence interval
for (x in 1:34) {
  for (y in 1:8) {
    a <- (total[x,y])
    b <- (total[35,y] - total[x,y])
    c <- (total[x,9] - total[x,y])
    d <- (total[35,9] - total[35,y] - total[x,9] + total[x,y])
    data <- matrix(c(a,b,c,d),nrow = 2, ncol = 2, byrow = TRUE)
    heatmap[x,y] <- ((a/c) / (b/d))
    
    if (heatmap[x,y] == 0) {
      heatmaporhigh[x,y] <- 0
      heatmaporlow[x,y] <- 0
    } else {
      heatmaporhigh[x,y] <- (exp(log(heatmap[x,y] + 1.96*(sqrt(1/a + 1/b + 1/c + 1/d))))) 
      if ((heatmap[x,y] - 1.96*(sqrt(1/a + 1/b + 1/c + 1/d))) < 0) {
        heatmaporlow[x,y] <- 0
      } else {
        heatmaporlow[x,y] <- (exp(log(heatmap[x,y] - 1.96*(sqrt(1/a + 1/b + 1/c + 1/d)))))
      }
    }
  }
}

View(heatmap)

rownames(heatmaporhigh) <- c("Aerospace Medicine", "Allergy & Immunology", "Anesthesiology", "Child Neurology",
                       "Dermatology", "Emergency Medicine", "Family Medicine", "Internal Medicine", "Med-Peds", 
                       "Medical Genetics", "Neurosurgery", "Neurology", "Nuclear Medicine", "OB/GYN",
                       "Occupational & Environmental Medicine", "Ophthalmology", "Orthopedic Surgery", 
                       "Osteopathic Neuromuscular Medicine", "Otolaryngology", "Pain Medicine", "Pathology",
                       "Pediatrics", "PM&R", "Plastic Surgery", "Psychiatry", "Public Health & Preventative Medicine",
                       "Radiation Oncology", "Radiology", "Interventional Radiology", "General Surgery",
                       "Thoracic Surgery", "Transitional year", "Urology", "Vascular Surgery"
)
colnames(heatmaporhigh) <- c("AI/AN", "Asian", "Black", "Hispanic", "Native Hawaiian", "White", "Other", "Non-US Citizen")
rownames(heatmaporlow) <- c("Aerospace Medicine", "Allergy & Immunology", "Anesthesiology", "Child Neurology",
                       "Dermatology", "Emergency Medicine", "Family Medicine", "Internal Medicine", "Med-Peds", 
                       "Medical Genetics", "Neurosurgery", "Neurology", "Nuclear Medicine", "OB/GYN",
                       "Occupational & Environmental Medicine", "Ophthalmology", "Orthopedic Surgery", 
                       "Osteopathic Neuromuscular Medicine", "Otolaryngology", "Pain Medicine", "Pathology",
                       "Pediatrics", "PM&R", "Plastic Surgery", "Psychiatry", "Public Health & Preventative Medicine",
                       "Radiation Oncology", "Radiology", "Interventional Radiology", "General Surgery",
                       "Thoracic Surgery", "Transitional year", "Urology", "Vascular Surgery"
)
colnames(heatmaporlow) <- c("AI/AN", "Asian", "Black", "Hispanic", "Native Hawaiian", "White", "Other", "Non-US Citizen")
View(heatmaporhigh)
View(heatmaporlow)


  ## Confidence intervals, p values ##
    # Is p significant at 0.01 - no asterisks - 99%
      ## Loop to make OR ##
      for (x in 1:34) {
        for (y in 1:8) {
          a <- (total[x,y])
          b <- (total[35,y] - total[x,y])
          c <- (total[x,9] - total[x,y])
          d <- (total[35,9] - total[35,y] - total[x,9] + total[x,y])
          data <- matrix(c(a,b,c,d),nrow = 2, ncol = 2, byrow = TRUE)
          heatmap[x,y] <- ((a/c) / (b/d))
    
          if (heatmap[x,y] == 0) {
            heatmaporhigh[x,y] <- 0
            heatmaporlow[x,y] <- 0
          } else {
            heatmaporhigh[x,y] <- (exp(log(heatmap[x,y] + 2.56*(sqrt(1/a + 1/b + 1/c + 1/d))))) 
            if ((heatmap[x,y] - 2.56*(sqrt(1/a + 1/b + 1/c + 1/d))) < 0) {
              heatmaporlow[x,y] <- 0
            } else {
              heatmaporlow[x,y] <- (exp(log(heatmap[x,y] - 2.56*(sqrt(1/a + 1/b + 1/c + 1/d)))))
            }
          }
        }
      }
          # Now run it through star

    # Is p significant at 0.001 - 99.9% Use this one as an asterisks
    ## Loop to make OR ##
    for (x in 1:34) {
      for (y in 1:8) {
        a <- (total[x,y])
        b <- (total[35,y] - total[x,y])
        c <- (total[x,9] - total[x,y])
        d <- (total[35,9] - total[35,y] - total[x,9] + total[x,y])
        data <- matrix(c(a,b,c,d),nrow = 2, ncol = 2, byrow = TRUE)
        heatmap[x,y] <- ((a/c) / (b/d))
    
        if (heatmap[x,y] == 0) {
          heatmaporhigh[x,y] <- 0
          heatmaporlow[x,y] <- 0
        } else {
          heatmaporhigh[x,y] <- (exp(log(heatmap[x,y] + 3.89*(sqrt(1/a + 1/b + 1/c + 1/d))))) 
          if ((heatmap[x,y] - 3.89*(sqrt(1/a + 1/b + 1/c + 1/d))) < 0) {
            heatmaporlow[x,y] <- 0
          } else {
            heatmaporlow[x,y] <- (exp(log(heatmap[x,y] - 3.89*(sqrt(1/a + 1/b + 1/c + 1/d)))))
          }
        }
      }
    }
    # Now run it through star

# Is p significant at 0.0001 - 99.99% this is an adjusted cutoff
## Loop to make OR ##
for (x in 1:34) {
  for (y in 1:8) {
    a <- (total[x,y])
    b <- (total[35,y] - total[x,y])
    c <- (total[x,9] - total[x,y])
    d <- (total[35,9] - total[35,y] - total[x,9] + total[x,y])
    data <- matrix(c(a,b,c,d),nrow = 2, ncol = 2, byrow = TRUE)
    heatmap[x,y] <- ((a/c) / (b/d))
    
    if (heatmap[x,y] == 0) {
      heatmaporhigh[x,y] <- 0
      heatmaporlow[x,y] <- 0
    } else {
      heatmaporhigh[x,y] <- (exp(log(heatmap[x,y] + 4.417*(sqrt(1/a + 1/b + 1/c + 1/d))))) 
      if ((heatmap[x,y] - 4.417*(sqrt(1/a + 1/b + 1/c + 1/d))) < 0) {
        heatmaporlow[x,y] <- 0
      } else {
        heatmaporlow[x,y] <- (exp(log(heatmap[x,y] - 4.417*(sqrt(1/a + 1/b + 1/c + 1/d)))))
      }
    }
  }
}
    # Now run it through star


## Start making Figure 1 ##
# Now get asterisks in statistically significant OR at alpha = 0.05
star <- heatmap
for (x in 1:34){
  for (y in 1:8) {
    if (heatmaporhigh[x,y] < 1 | heatmaporlow[x,y] > 1) {
      if (heatmap[x,y] == 0) {
        star[x,y] <- c("NA")
        #star[x,y] <- paste(star[x,y], "")
      } else {
        # add * to data in heatmap
        star[x,y] <- paste(star[x,y], "/*")
      }
    }
  }
}

View(star)

star <- as.data.frame(star)
star <- cbind(rownames(star), star)
colnames(star)[1] <- "Specialty"
star <- melt(data = star, id = "Specialty")
View(star)

test <- star
test <- separate(data = test, col = value, into = c("newvalue", "star"), sep = c("/"))
test$newvalue <- as.numeric(test$newvalue)
View(test)
# want star NA to be removed
for (a in 1:272) {
  if (is.na(test[a,4]) == TRUE) {
    test[a,4] <- c("")
  } else {
    test[a,4] <- c("*")
  }
}
View(test)

# Make 1 column to map to colors to include Na, make 1 column map to labels so it is presented as zero
newtest <- cbind(test, test$newvalue)
colnames(newtest)[5] <- "textlabels"
View(newtest)
for (a in 1:272) {
  if(is.na(newtest[a,5])) {
    newtest[a,5] <- 0
  }
}
colnames(newtest)[3] <- "Odds Ratio"
View(newtest)

x11()
ggplot(data = newtest, aes(x = variable, y = Specialty, fill = `Odds Ratio`)) +
  geom_tile() +
  theme_bw() +
 # labs(title = "2019 - 2023 Resident Population: ", subtitle = "Odds Ratios of Ethnicity and Specialty") +
  theme(plot.title = element_text(size = 24, face = "bold",hjust = 0.5)) +
  theme(plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_fill_gradient2(low = "red", mid = "white", high = "steelblue", midpoint = 1, na.value = "gray80", breaks = c(0.01, 1:4), limits = c(0.01,4)) + 
  geom_text(aes(label = paste(round(textlabels, digits = 1), star)), color = "black", size = 4) +
  theme(legend.text = element_text(size = 16)) +
  theme(legend.position = "right", legend.box.just = "center") +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.key.size = unit(1.5, 'cm')) +
  geom_point(data = newtest, aes(size="0"), shape =NA, colour = "grey80") +
  guides(size=guide_legend("No Residents",  override.aes=list(shape = 15, size = 20),label = TRUE, direction = "vertical"))


#####
# Raw data - including other and unknown
setwd('~/ANAMS paper/AAIP presentation/')
residents <- read.csv('residents.csv', na.strings = c("", "NA"))
residents <- as.data.frame(residents)
residents <- residents[,-c(1)]
residents <- as.data.frame(residents)
View(residents)

setwd('~/ANAMS paper/Heatmap/')
y2020 <- read.csv('2020 combined final.csv', na.strings = c("", "NA"))
y2020 <- as.data.frame(y2020)
y2020 <- y2020[,-c(1)]
y2020 <- as.data.frame(y2020)
View(y2020)

setwd('~/ANAMS paper/Heatmap/2021 files/')
res2021 <- read.csv('2021 combined.csv', na.strings = c("", "NA"))
res2021 <- as.data.frame(res2021)
res2021 <- res2021[,-c(1)]
res2021 <- as.data.frame(res2021)
View(res2021)

setwd('~/ANAMS paper/Heatmap/2023 files/')
res2023 <- read.csv('2023.csv', na.strings = c("", "NA"))
res2023 <- as.data.frame(res2023)
res2023 <- res2023[,-c(1)]
res2023 <- as.data.frame(res2023)
View(res2023)

total <- matrix(0,35,10)
for (i in 1:35) {
  for (j in 1:10) {
    total[i,j] <- residents[i,j] + y2020[i,j] + res2021[i,j] + res2023[i,j]
  }
}

View(total)


heatmap <- matrix(0, 34, 9)
rownames(heatmap) <- c("Aerospace Medicine", "Allergy & Immunology", "Anesthesiology", "Child Neurology",
                       "Dermatology", "Emergency Medicine", "Family Medicine", "Internal Medicine", "Med-Peds", 
                       "Medical Genetics", "Neurosurgery", "Neurology", "Nuclear Medicine", "OB/GYN",
                       "Occupational & Environmental Medicine", "Ophthalmology", "Orthopedic Surgery", 
                       "Osteopathic Neuromuscular Medicine", "Otolaryngology", "Pain Medicine", "Pathology",
                       "Pediatrics", "PM&R", "Plastic Surgery", "Psychiatry", "Public Health & Preventative Medicine",
                       "Radiation Oncology", "Radiology", "Interventional Radiology", "General Surgery",
                       "Thoracic Surgery", "Transitional year", "Urology", "Vascular Surgery"
)
colnames(heatmap) <- c("AI/AN", "Asian", "Black", "Hispanic", "Native Hawaiian", "White", "Other",
                       "Unknown", "Non-US Citizen")
View(heatmap)

heatmaporhigh <- matrix(0, 34, 9)
heatmaporlow <- matrix(0, 34, 9)

for (x in 1:34) {
  for (y in 1:9) {
    a <- (total[x,y])
    b <- (total[35,y] - total[x,y])
    c <- (total[x,10] - total[x,y])
    d <- (total[35,10] - total[35,y] - total[x,10] + total[x,y])
    data <- matrix(c(a,b,c,d),nrow = 2, ncol = 2, byrow = TRUE)
    heatmap[x,y] <- ((a/c) / (b/d))
    
    if (heatmap[x,y] == 0) {
      heatmaporhigh[x,y] <- 0
      heatmaporlow[x,y] <- 0
    } else {
      heatmaporhigh[x,y] <- (exp(log(heatmap[x,y] + 1.96*(sqrt(1/a + 1/b + 1/c + 1/d))))) 
      if ((heatmap[x,y] - 1.96*(sqrt(1/a + 1/b + 1/c + 1/d))) < 0) {
        heatmaporlow[x,y] <- 0
      } else {
        heatmaporlow[x,y] <- (exp(log(heatmap[x,y] - 1.96*(sqrt(1/a + 1/b + 1/c + 1/d)))))
      }
    }
  }
}

View(heatmap)
View(heatmaporhigh)
View(heatmaporlow)

# Now get asterisks in statistically significant OR at alpha = 0.05
star <- heatmap
for (x in 1:34){
  for (y in 1:9) {
    if (heatmaporhigh[x,y] < 1 | heatmaporlow[x,y] > 1) {
      if (heatmap[x,y] == 0) {
        star[x,y] <- c("NA")
        #star[x,y] <- paste(star[x,y], "")
      } else {
        # add * to data in heatmap
        star[x,y] <- paste(star[x,y], "/*")
      } 
      
    }
  }
}

View(star)

star <- as.data.frame(star)
star <- cbind(rownames(star), star)
colnames(star)[1] <- "Specialty"
star <- melt(data = star, id = "Specialty")
View(star)

test <- star
test <- separate(data = test, col = value, into = c("newvalue", "star"), sep = c("/"))
test$newvalue <- as.numeric(test$newvalue)
View(test)
# want star NA to be removed
for (a in 1:306) {
  if (is.na(test[a,4]) == TRUE) {
    test[a,4] <- c("")
  } else {
    test[a,4] <- c("*")
  }
}
View(test)

# Make 1 column to map to colors to include Na, make 1 column map to labels so it is presented as zero
newtest <- cbind(test, test$newvalue)
colnames(newtest)[5] <- "textlabels"
View(newtest)
for (a in 1:306) {
  if(is.na(newtest[a,5])) {
    newtest[a,5] <- 0
  }
}
colnames(newtest)[3] <- "Odds Ratio"
View(newtest)

x11()
ggplot(data = newtest, aes(x = variable, y = Specialty, fill = `Odds Ratio`)) +
  geom_tile() +
  theme_bw() +
  labs(title = "2019 - 2023 Resident Population: ", subtitle = "Odds Ratios of Ethnicity and Specialty") +
  theme(plot.title = element_text(size = 24, face = "bold",hjust = 0.5)) +
  theme(plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_fill_gradient2(low = "red", mid = "white", high = "steelblue", midpoint = 1, na.value = "gray80", breaks = c(0.01, 1:4), limits = c(0.01,4)) + 
  geom_text(aes(label = paste(round(textlabels, digits = 2), star)), color = "black", size = 4) +
  theme(legend.text = element_text(size = 16)) +
  theme(legend.position = "right", legend.box.just = "center") +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.key.size = unit(1.5, 'cm')) +
  geom_point(data = newtest, aes(size="0"), shape =NA, colour = "grey80") +
  guides(size=guide_legend("No Residents",  override.aes=list(shape = 15, size = 20),label = TRUE, direction = "vertical"))


#####
# Remove unknown and other from denominator
setwd('~/ANAMS paper/AAIP presentation/')
residents <- read.csv('residents.csv', na.strings = c("", "NA"))
residents <- as.data.frame(residents)
residents <- residents[,-c(1)]
residents <- as.data.frame(residents)
View(residents)

setwd('~/ANAMS paper/Heatmap/')
y2020 <- read.csv('2020 combined final.csv', na.strings = c("", "NA"))
y2020 <- as.data.frame(y2020)
y2020 <- y2020[,-c(1)]
y2020 <- as.data.frame(y2020)
View(y2020)

setwd('~/ANAMS paper/Heatmap/2021 files/')
res2021 <- read.csv('2021 combined.csv', na.strings = c("", "NA"))
res2021 <- as.data.frame(res2021)
res2021 <- res2021[,-c(1)]
res2021 <- as.data.frame(res2021)
View(res2021)

setwd('~/ANAMS paper/Heatmap/2023 files/')
res2023 <- read.csv('2023.csv', na.strings = c("", "NA"))
res2023 <- as.data.frame(res2023)
res2023 <- res2023[,-c(1)]
res2023 <- as.data.frame(res2023)
View(res2023)

total <- matrix(0,35,10)
for (i in 1:35) {
  for (j in 1:10) {
    total[i,j] <- residents[i,j] + y2020[i,j] + res2021[i,j] + res2023[i,j]
  }
}

View(total)

newtotal <- total
newtotal[,10] <- newtotal[,10] - total[,8] - total[,9]
View(newtotal)
newtotal <- newtotal[,-c(8:9)]


heatmap <- matrix(0, 34, 7)
rownames(heatmap) <- c("Aerospace Medicine", "Allergy & Immunology", "Anesthesiology", "Child Neurology",
                       "Dermatology", "Emergency Medicine", "Family Medicine", "Internal Medicine", "Med-Peds", 
                       "Medical Genetics", "Neurosurgery", "Neurology", "Nuclear Medicine", "OB/GYN",
                       "Occupational & Environmental Medicine", "Ophthalmology", "Orthopedic Surgery", 
                       "Osteopathic Neuromuscular Medicine", "Otolaryngology", "Pain Medicine", "Pathology",
                       "Pediatrics", "PM&R", "Plastic Surgery", "Psychiatry", "Public Health & Preventative Medicine",
                       "Radiation Oncology", "Radiology", "Interventional Radiology", "General Surgery",
                       "Thoracic Surgery", "Transitional year", "Urology", "Vascular Surgery"
)
colnames(heatmap) <- c("AI/AN", "Asian", "Black", "Hispanic", "Native Hawaiian", "White", "Non-US Citizen")
View(heatmap)

heatmaporhigh <- matrix(0, 34, 7)
heatmaporlow <- matrix(0, 34, 7)

for (x in 1:34) {
  for (y in 1:7) {
    a <- (newtotal[x,y])
    b <- (newtotal[35,y] - newtotal[x,y])
    c <- (newtotal[x,8] - newtotal[x,y])
    d <- (newtotal[35,8] - newtotal[35,y] - newtotal[x,8] + newtotal[x,y])
    data <- matrix(c(a,b,c,d),nrow = 2, ncol = 2, byrow = TRUE)
    heatmap[x,y] <- ((a/c) / (b/d))
    
    if (heatmap[x,y] == 0) {
      heatmaporhigh[x,y] <- 0
      heatmaporlow[x,y] <- 0
    } else {
      heatmaporhigh[x,y] <- (exp(log(heatmap[x,y] + 1.96*(sqrt(1/a + 1/b + 1/c + 1/d))))) 
      if ((heatmap[x,y] - 1.96*(sqrt(1/a + 1/b + 1/c + 1/d))) < 0) {
        heatmaporlow[x,y] <- 0
      } else {
        heatmaporlow[x,y] <- (exp(log(heatmap[x,y] - 1.96*(sqrt(1/a + 1/b + 1/c + 1/d)))))
      }
    }
  }
}

View(heatmap)
View(heatmaporhigh)
View(heatmaporlow)

# Now get asterisks in statistically significant OR at alpha = 0.05
star <- heatmap
for (x in 1:34){
  for (y in 1:7) {
    if (heatmaporhigh[x,y] < 1 | heatmaporlow[x,y] > 1) {
      if (heatmap[x,y] == 0) {
        star[x,y] <- c("NA")
        #star[x,y] <- paste(star[x,y], "")
      } else {
        # add * to data in heatmap
        star[x,y] <- paste(star[x,y], "/*")
      } 
      
    }
  }
}

View(star)

star <- as.data.frame(star)
star <- cbind(rownames(star), star)
colnames(star)[1] <- "Specialty"
star <- melt(data = star, id = "Specialty")
View(star)

test <- star
test <- separate(data = test, col = value, into = c("newvalue", "star"), sep = c("/"))
test$newvalue <- as.numeric(test$newvalue)
View(test)
# want star NA to be removed
for (a in 1:238) {
  if (is.na(test[a,4]) == TRUE) {
    test[a,4] <- c("")
  } else {
    test[a,4] <- c("*")
  }
}
View(test)

# Make 1 column to map to colors to include Na, make 1 column map to labels so it is presented as zero
newtest <- cbind(test, test$newvalue)
colnames(newtest)[5] <- "textlabels"
View(newtest)
for (a in 1:238) {
  if(is.na(newtest[a,5])) {
    newtest[a,5] <- 0
  }
}
colnames(newtest)[3] <- "Odds Ratio"
View(newtest)

x11()
ggplot(data = newtest, aes(x = variable, y = Specialty, fill = `Odds Ratio`)) +
  geom_tile() +
  theme_bw() +
  labs(title = "2019 - 2023 Resident Population: ", subtitle = "Odds Ratios of Ethnicity and Specialty") +
  theme(plot.title = element_text(size = 24, face = "bold",hjust = 0.5)) +
  theme(plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_fill_gradient2(low = "red", mid = "white", high = "steelblue", midpoint = 1, na.value = "gray80", breaks = c(0.01, 1:4), limits = c(0.01,4)) + 
  geom_text(aes(label = paste(round(textlabels, digits = 1), star)), color = "black", size = 4) +
  theme(legend.text = element_text(size = 16)) +
  theme(legend.position = "right", legend.box.just = "center") +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.key.size = unit(1.5, 'cm')) +
  geom_point(data = newtest, aes(size="0"), shape =NA, colour = "grey80") +
  guides(size=guide_legend("No Residents",  override.aes=list(shape = 15, size = 20),label = TRUE, direction = "vertical"))
