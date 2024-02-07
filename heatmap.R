rm(list=ls(all=T))
options(stringsASFactors = F) # no automatic data transformation
options("scipen" = 100, "digits" = 4) # supress math annotation

setwd('~/ANAMS paper/AAIP presentation/')
residents <- read.csv('residents.csv', na.strings = c("", "NA"))
str(residents)

residents <- as.data.frame(residents)

residents <- residents[,-c(1)]
View(residents)

residents <- as.data.frame(residents)

library(ggplot2)
library(reshape2)
library(tidyverse)
library(epitools)

a <- oddsratio(data)
a$measure[2,1]
View(residents)

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

#####
# Final loop
for (x in 1:34) {
  for (y in 1:9) {
    data <- matrix(c((residents[x,y]), 
                     (residents[35,y] - residents[x,y]),
                     (residents[x,10] - residents[x,y]), 
                     (residents[35,10] - residents[35,y] - residents[x,10] + residents[x,y])),
                   nrow = 2, ncol = 2, byrow = TRUE)
    heatmap[x,y] <- (data[1,1]/data[2,1]) / (data[1,2]/data[2,2])
  }
}

heatmap <- as.data.frame(heatmap)
View(heatmap)
write.csv(heatmap, file = '/users/M276066/Documents/ANAMS paper/Heatmap/heatmap.csv', row.names=TRUE)

test <- cbind(rownames(heatmap), heatmap)
colnames(test)[1] <- "Specialty"
View(test)

test <- melt(data = test, id = "Specialty")
View(test)

x11()
ggplot(data = test, aes(x = variable, y = Specialty, fill = value)) +
  geom_tile() +
  theme_bw() +
  labs(title = "Odds Ratios: Resident Race and Specialty") +
  theme(plot.title = element_text(size = 24, face = "bold")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_fill_gradient2(low = "red", mid = "white", high = "steelblue", midpoint = 1) +
  geom_text(aes(label = round(value, digits = 1)), color = "black", size = 4) +
  theme(legend.text = element_text(size = 16))

#####
# loop to make heatmap with ORs
for (x in 1:34) {
  for (y in 1:9) {
    a <- (residents[x,y])
    b <- (residents[35,y] - residents[x,y])
    c <- (residents[x,10] - residents[x,y])
    d <- (residents[35,10] - residents[35,y] - residents[x,10] + residents[x,y])
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
  #labs(title = "2021 - 2022 Resident Population: ", subtitle = "Odds Ratios of Ethnicity and Specialty") +
  theme(plot.title = element_text(size = 24, face = "bold",hjust = 0.5)) +
  theme(plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_fill_gradient2(low = "red", mid = "white", high = "steelblue", midpoint = 1, na.value = "gray80", breaks = c(0.1, 1:9), limits = c(0.1,9)) + 
  geom_text(aes(label = paste(round(textlabels, digits = 1), star)), color = "black", size = 4) +
  theme(legend.text = element_text(size = 16)) +
  theme(legend.position = "right", legend.box.just = "center") +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.key.size = unit(1.5, 'cm')) +
  geom_point(data = newtest, aes(size="0"), shape =NA, colour = "grey80") +
  guides(size=guide_legend("No Residents",  override.aes=list(shape = 15, size = 20),label = TRUE, direction = "vertical"))

write.csv(heatmap, file = '/users/M276066/Documents/ANAMS paper/Heatmap/heatmap.csv', row.names=TRUE)

#####
# Heatmap for 2020
setwd('~/ANAMS paper/Heatmap/')
y2020 <- read.csv('2020 combined final.csv', na.strings = c("", "NA"))
str(y2020)
y2020 <- as.data.frame(y2020)
y2020 <- y2020[,-c(1)]
View(y2020)

heatmap2020 <- matrix(0, 34, 9)
rownames(heatmap2020) <- c("Aerospace Medicine", "Allergy & Immunology", "Anesthesiology", "Child Neurology",
                       "Dermatology", "Emergency Medicine", "Family Medicine", "Internal Medicine", "Med-Peds", 
                       "Medical Genetics", "Neurosurgery", "Neurology", "Nuclear Medicine", "OB/GYN",
                       "Occupational & Environmental Medicine", "Ophthalmology", "Orthopedic Surgery", 
                       "Osteopathic Neuromuscular Medicine", "Otolaryngology", "Pain Medicine", "Pathology",
                       "Pediatrics", "PM&R", "Plastic Surgery", "Psychiatry", "Public Health & Preventative Medicine",
                       "Radiation Oncology", "Radiology", "Interventional Radiology", "General Surgery",
                       "Thoracic Surgery", "Transitional year", "Urology", "Vascular Surgery"
)
colnames(heatmap2020) <- c("AI/AN", "Asian", "Black", "Hispanic", "Native Hawaiian", "White", "Other",
                       "Unknown", "Non-US Citizen")
View(heatmap2020)

heatmaporhigh2020 <- matrix(0, 34, 9)
heatmaporlow2020 <- matrix(0, 34, 9)

# loop to make heatmap with ORs
for (x in 1:34) {
  for (y in 1:9) {
    a <- (y2020[x,y])
    b <- (y2020[35,y] - y2020[x,y])
    c <- (y2020[x,10] - y2020[x,y])
    d <- (y2020[35,10] - y2020[35,y] - y2020[x,10] + y2020[x,y])
    data <- matrix(c(a,b,c,d),nrow = 2, ncol = 2, byrow = TRUE)
    heatmap2020[x,y] <- ((a/c) / (b/d))
    
    if (heatmap2020[x,y] == 0) {
      heatmaporhigh2020[x,y] <- 0
      heatmaporlow2020[x,y] <- 0
    } else {
      heatmaporhigh2020[x,y] <- (exp(log(heatmap2020[x,y] + 1.96*(sqrt(1/a + 1/b + 1/c + 1/d))))) 
      if ((heatmap2020[x,y] - 1.96*(sqrt(1/a + 1/b + 1/c + 1/d))) < 0) {
        heatmaporlow2020[x,y] <- 0
      } else {
        heatmaporlow2020[x,y] <- (exp(log(heatmap2020[x,y] - 1.96*(sqrt(1/a + 1/b + 1/c + 1/d)))))
      }
    }
  }
}

View(heatmap2020)
View(heatmaporhigh2020)
View(heatmaporlow2020)

# Now get asterisks in statistically significant OR at alpha = 0.05
star2020 <- heatmap2020
for (x in 1:34){
  for (y in 1:9) {
    if (heatmaporhigh2020[x,y] < 1 | heatmaporlow2020[x,y] > 1) {
      if (heatmap2020[x,y] == 0) {
        star2020[x,y] <- c("NA")
        #star[x,y] <- paste(star[x,y], "")
      } else {
        # add * to data in heatmap
        star2020[x,y] <- paste(star2020[x,y], "/*")
      } 
    }
  }
}

View(star2020)

star2020 <- as.data.frame(star2020)
star2020 <- cbind(rownames(star2020), star2020)
colnames(star2020)[1] <- "Specialty"
star2020 <- melt(data = star2020, id = "Specialty")
View(star2020)


test2020 <- star2020
test2020 <- separate(data = test2020, col = value, into = c("newvalue", "star"), sep = c("/"))
test2020$newvalue <- as.numeric(test2020$newvalue)
View(test2020)
# want star NA to be removed
for (a in 1:306) {
  if (is.na(test2020[a,4]) == TRUE) {
    test2020[a,4] <- c("")
  } else {
    test2020[a,4] <- c("*")
  }
}
View(test2020)

# Make 1 column to map to colors to include Na, make 1 column map to labels so it is presented as zero
newtest2020 <- cbind(test2020, test2020$newvalue)
colnames(newtest2020)[5] <- "textlabels"
View(newtest2020)
for (a in 1:306) {
  if(is.na(newtest2020[a,5])) {
    newtest2020[a,5] <- 0
  }
}
colnames(newtest2020)[3] <- "Odds Ratio"
View(newtest2020)
str(newtest$textlabels)

x11()
ggplot(data = newtest2020, aes(x = variable, y = Specialty, fill = `Odds Ratio`)) +
  geom_tile() +
  theme_bw() +
  #labs(title = "2020 - 2021 Resident Population: ", subtitle = "Odds Ratios of Ethnicity and Specialty") +
  theme(plot.title = element_text(size = 24, face = "bold",hjust = 0.5)) +
  theme(plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_fill_gradient2(low = "red", mid = "white", high = "steelblue", midpoint = 1, na.value = "gray80", breaks = c(0.1, 1:9), limits = c(0.1,9)) + 
  geom_text(aes(label = paste(round(textlabels, digits = 1), star)), color = "black", size = 4) +
  theme(legend.text = element_text(size = 16)) +
  theme(legend.position = "right", legend.box.just = "center") +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.key.size = unit(1.5, 'cm')) +
  geom_point(data = newtest, aes(size="0"), shape =NA, colour = "grey80") +
  guides(size=guide_legend("No Residents",  override.aes=list(shape = 15, size = 20),label = TRUE, direction = "vertical"))

write.csv(heatmap, file = '/users/M276066/Documents/ANAMS paper/Heatmap/heatmap.csv', row.names=TRUE)

#####
# Heatmap for 2021-2022
setwd('~/ANAMS paper/Heatmap/2021 files/')
res2021 <- read.csv('2021 combined.csv', na.strings = c("", "NA"))
res2021 <- as.data.frame(res2021)
res2021 <- res2021[,-c(1)]
View(res2021)
res2021 <- as.data.frame(res2021)

library(ggplot2)
library(reshape2)
library(tidyverse)
library(epitools)

heatmap21 <- matrix(0, 34, 9)
rownames(heatmap21) <- c("Aerospace Medicine", "Allergy & Immunology", "Anesthesiology", "Child Neurology",
                       "Dermatology", "Emergency Medicine", "Family Medicine", "Internal Medicine", "Med-Peds", 
                       "Medical Genetics", "Neurosurgery", "Neurology", "Nuclear Medicine", "OB/GYN",
                       "Occupational & Environmental Medicine", "Ophthalmology", "Orthopedic Surgery", 
                       "Osteopathic Neuromuscular Medicine", "Otolaryngology", "Pain Medicine", "Pathology",
                       "Pediatrics", "PM&R", "Plastic Surgery", "Psychiatry", "Public Health & Preventative Medicine",
                       "Radiation Oncology", "Radiology", "Interventional Radiology", "General Surgery",
                       "Thoracic Surgery", "Transitional year", "Urology", "Vascular Surgery"
)
colnames(heatmap21) <- c("AI/AN", "Asian", "Black", "Hispanic", "Native Hawaiian", "White", "Other",
                       "Unknown", "Non-US Citizen")
View(heatmap21)

heatmaporhigh21 <- matrix(0, 34, 9)
heatmaporlow21 <- matrix(0, 34, 9)

# loop to make heatmap with ORs
for (x in 1:34) {
  for (y in 1:9) {
    a <- (res2021[x,y])
    b <- (res2021[35,y] - res2021[x,y])
    c <- (res2021[x,10] - res2021[x,y])
    d <- (res2021[35,10] - res2021[35,y] - res2021[x,10] + res2021[x,y])
    data <- matrix(c(a,b,c,d),nrow = 2, ncol = 2, byrow = TRUE)
    heatmap21[x,y] <- ((a/c) / (b/d))
   
    if (heatmap21[x,y] == 0) {
      heatmaporhigh21[x,y] <- 0
      heatmaporlow21[x,y] <- 0
    } else {
      heatmaporhigh21[x,y] <- (exp(log(heatmap21[x,y] + 1.96*(sqrt(1/a + 1/b + 1/c + 1/d))))) 
      if ((heatmap21[x,y] - 1.96*(sqrt(1/a + 1/b + 1/c + 1/d))) < 0 )  {
        heatmaporlow21[x,y] <- 0
      } else {
        heatmaporlow21[x,y] <- (exp(log(heatmap21[x,y] - 1.96*(sqrt(1/a + 1/b + 1/c + 1/d)))))
      }
    }
  }
}

View(heatmap21)
View(heatmaporhigh21)
View(heatmaporlow21)

# Now get asterisks in statistically significant OR at alpha = 0.05
star <- heatmap21
for (x in 1:34){
  for (y in 1:9) {
    if (heatmaporhigh21[x,y] < 1 | heatmaporlow21[x,y] > 1) {
      if (heatmap21[x,y] == 0) {
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
  #labs(title = "2021 - 2022 Resident Population: ", subtitle = "Odds Ratios of Ethnicity and Specialty") +
  theme(plot.title = element_text(size = 24, face = "bold",hjust = 0.5)) +
  theme(plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_fill_gradient2(low = "red", mid = "white", high = "steelblue", midpoint = 1, na.value = "gray80", breaks = c(0.1, 1:9), limits = c(0.1,9)) + 
  geom_text(aes(label = paste(round(textlabels, digits = 1), star)), color = "black", size = 4) +
  theme(legend.text = element_text(size = 16)) +
  theme(legend.position = "right", legend.box.just = "center") +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.key.size = unit(1.5, 'cm')) +
  geom_point(data = newtest, aes(size="0"), shape =NA, colour = "grey80") +
  guides(size=guide_legend("No Residents",  override.aes=list(shape = 15, size = 20),label = TRUE, direction = "vertical"))

#####
# Heatmap for 2022-2023

setwd('~/ANAMS paper/Heatmap/2023 files/')
res2023 <- read.csv('2023.csv', na.strings = c("", "NA"))
res2023 <- as.data.frame(res2023)
res2023 <- res2023[,-c(1)]
View(res2023)
res2021 <- as.data.frame(res2023)

library(ggplot2)
library(reshape2)
library(tidyverse)
library(epitools)

heatmap23 <- matrix(0, 34, 9)
rownames(heatmap23) <- c("Aerospace Medicine", "Allergy & Immunology", "Anesthesiology", "Child Neurology",
                         "Dermatology", "Emergency Medicine", "Family Medicine", "Internal Medicine", "Med-Peds", 
                         "Medical Genetics", "Neurosurgery", "Neurology", "Nuclear Medicine", "OB/GYN",
                         "Occupational & Environmental Medicine", "Ophthalmology", "Orthopedic Surgery", 
                         "Osteopathic Neuromuscular Medicine", "Otolaryngology", "Pain Medicine", "Pathology",
                         "Pediatrics", "PM&R", "Plastic Surgery", "Psychiatry", "Public Health & Preventative Medicine",
                         "Radiation Oncology", "Radiology", "Interventional Radiology", "General Surgery",
                         "Thoracic Surgery", "Transitional year", "Urology", "Vascular Surgery"
)
colnames(heatmap23) <- c("AI/AN", "Asian", "Black", "Hispanic", "Native Hawaiian", "White", "Other",
                         "Unknown", "Non-US Citizen")
View(heatmap23)

heatmaporhigh23 <- matrix(0, 34, 9)
heatmaporlow23 <- matrix(0, 34, 9)

# loop to make heatmap with ORs
for (x in 1:34) {
  for (y in 1:9) {
    a <- (res2023[x,y])
    b <- (res2023[35,y] - res2023[x,y])
    c <- (res2023[x,10] - res2023[x,y])
    d <- (res2023[35,10] - res2023[35,y] - res2023[x,10] + res2023[x,y])
    data <- matrix(c(a,b,c,d),nrow = 2, ncol = 2, byrow = TRUE)
    heatmap23[x,y] <- ((a/c) / (b/d))
    
    if (heatmap23[x,y] == 0) {
      heatmaporhigh23[x,y] <- 0
      heatmaporlow23[x,y] <- 0
    } else {
      heatmaporhigh23[x,y] <- (exp(log(heatmap23[x,y] + 1.96*(sqrt(1/a + 1/b + 1/c + 1/d))))) 
      if ((heatmap23[x,y] - 1.96*(sqrt(1/a + 1/b + 1/c + 1/d))) < 0 )  {
        heatmaporlow23[x,y] <- 0
      } else {
        heatmaporlow23[x,y] <- (exp(log(heatmap23[x,y] - 1.96*(sqrt(1/a + 1/b + 1/c + 1/d)))))
      }
    }
  }
}

View(heatmap23)
View(heatmaporhigh23)
View(heatmaporlow23)

# Now get asterisks in statistically significant OR at alpha = 0.05
star <- heatmap23
for (x in 1:34){
  for (y in 1:9) {
    if (heatmaporhigh23[x,y] < 1 | heatmaporlow23[x,y] > 1) {
      if (heatmap23[x,y] == 0) {
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
.
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
  #labs(title = "2021 - 2022 Resident Population: ", subtitle = "Odds Ratios of Ethnicity and Specialty") +
  theme(plot.title = element_text(size = 24, face = "bold",hjust = 0.5)) +
  theme(plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_fill_gradient2(low = "red", mid = "white", high = "steelblue", midpoint = 1, na.value = "gray80", breaks = c(0.1, 1:6), limits = c(0.1,6)) + 
  geom_text(aes(label = paste(round(textlabels, digits = 1), star)), color = "black", size = 4) +
  theme(legend.text = element_text(size = 16)) +
  theme(legend.position = "right", legend.box.just = "center") +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.key.size = unit(1.5, 'cm')) +
  geom_point(data = newtest, aes(size="0"), shape =NA, colour = "grey80") +
  guides(size=guide_legend("No Residents",  override.aes=list(shape = 15, size = 20),label = TRUE, direction = "vertical"))


#####
# Total numbers - change over time
y2020 <- read.csv('2020 combined final.csv', na.strings = c("", "NA"))
residents <- read.csv('residents.csv', na.strings = c("", "NA"))

residents <- residents[,-c(1)]
y2020 <- y2020[,-c(1)]

View(residents)
View(y2020)

changematrix <- matrix(0,35,10)
rownames(changematrix) <- c("Aerospace Medicine", "Allergy & Immunology", "Anesthesiology", "Child Neurology",
                            "Dermatology", "Emergency Medicine", "Family Medicine", "Internal Medicine", "Med-Peds", 
                            "Medical Genetics", "Neurosurgery", "Neurology", "Nuclear Medicine", "OB/GYN",
                            "Occupational & Environmental Medicine", "Ophthalmology", "Orthopedic Surgery", 
                            "Osteopathic Neuromuscular Medicine", "Otolaryngology", "Pain Medicine", "Pathology",
                            "Pediatrics", "PM&R", "Plastic Surgery", "Psychiatry", "Public Health & Preventative Medicine",
                            "Radiation Oncology", "Radiology", "Interventional Radiology", "General Surgery",
                            "Thoracic Surgery", "Transitional year", "Urology", "Vascular Surgery", "Total"
)
colnames(changematrix) <- c("AI/AN", "Asian", "Black", "Hispanic", "Native Hawaiian", "White", "Other",
                            "Unknown", "Non-US Citizen", "Total")



View(changematrix)

for (i in 1:35) {
  for (j in 1:10) {
    changematrix[i,j] <- ((residents[i,j] - y2020[i,j]) / y2020[i,j]) * 100
    if (changematrix[i,j] == Inf | is.nan(changematrix[i,j])) {
      changematrix[i,j] <- NA
    }
  }
}
changematrix[c(1,15),10] <- NA
View(changematrix)

changematrix <- as.data.frame(changematrix)
changematrix <- cbind(rownames(changematrix), changematrix)
colnames(changematrix)[1] <- "Specialty"
changematrix <- melt(data = changematrix, id = "Specialty")
View(changematrix)

testchangematrix <- cbind(changematrix,0)
colnames(testchangematrix)[4] <- "textlabels"
View(testchangematrix)

for (a in 1:350) {
  if(is.na(testchangematrix[a,3])) {
    testchangematrix[a,4] <- c("NA")
  } else {
    testchangematrix[a,4] <- round(testchangematrix[a,3], digits = 0)
  }
}
View(testchangematrix)
colnames(testchangematrix)[3] <- "change"

x11()
ggplot(data = testchangematrix, aes(x = variable, y = Specialty, fill = `change`)) +
  geom_tile() +
  theme_bw() +
  #labs(title = "Change from 2020 - 2022", subtitle = "Percentage") +
  theme(plot.title = element_text(size = 24, face = "bold",hjust = 0.5)) +
  theme(plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_fill_gradient2(low = "red", mid = "white", high = "steelblue", midpoint = 0, na.value = "gray80",breaks = c(-100, -50, 100, 200, 300, 400, 500), limits = c(-100,500)) + 
  geom_text(aes(label = paste(textlabels)) , color = "black", size = 4) +
  theme(legend.text = element_text(size = 16)) +
  theme(legend.position = "right", legend.box.just = "center") +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.key.size = unit(1.5, 'cm')) +
  geom_point(data = testchangematrix, aes(size="0"), shape =NA, colour = "grey80") +
  guides(size=guide_legend("No Residents 2020",  override.aes=list(shape = 15, size = 20),label = TRUE, direction = "vertical"))

write.csv(heatmap, file = '/users/M276066/Documents/ANAMS paper/Heatmap/heatmap.csv', row.names=TRUE)



#####
# Change in heatmap OR over time - figure 3
setwd('~/ANAMS paper/Heatmap/')
y2020 <- read.csv('2020 combined final.csv', na.strings = c("", "NA"))
residents <- read.csv('residents.csv', na.strings = c("", "NA"))
residents <- residents[,-c(1)]
y2020 <- y2020[,-c(1)]
View(residents)
View(y2020)
orchange <- matrix(0,34,9)
rownames(orchange) <- c("Aerospace Medicine", "Allergy & Immunology", "Anesthesiology", "Child Neurology",
                            "Dermatology", "Emergency Medicine", "Family Medicine", "Internal Medicine", "Med-Peds", 
                            "Medical Genetics", "Neurosurgery", "Neurology", "Nuclear Medicine", "OB/GYN",
                            "Occupational & Environmental Medicine", "Ophthalmology", "Orthopedic Surgery", 
                            "Osteopathic Neuromuscular Medicine", "Otolaryngology", "Pain Medicine", "Pathology",
                            "Pediatrics", "PM&R", "Plastic Surgery", "Psychiatry", "Public Health & Preventative Medicine",
                            "Radiation Oncology", "Radiology", "Interventional Radiology", "General Surgery",
                            "Thoracic Surgery", "Transitional year", "Urology", "Vascular Surgery")

colnames(orchange) <- c("AI/AN", "Asian", "Black", "Hispanic", "Native Hawaiian", "White", "Other",
                            "Unknown", "Non-US Citizen")

heatmap22 <- matrix(0,34,9)
heatmap20 <- matrix(0,34,9)
pvalue <- matrix(0,34,9)
logdelta <- matrix(0,34,9)
sedelta <- matrix(0,34,9)
se22 <- matrix(0,34,9)
se20 <- matrix(0,34,9)

for (x in 1:34) {
  for (y in 1:9) {
    a22 <- (residents[x,y])
    b22 <- (residents[35,y] - residents[x,y])
    c22 <- (residents[x,10] - residents[x,y])
    d22 <- (residents[35,10] - residents[35,y] - residents[x,10] + residents[x,y])
    se22 <- sqrt(1/a22 + 1/b22 + 1/c22 + 1/d22)
    data22 <- matrix(c(a22,b22,c22,d22),nrow = 2, ncol = 2, byrow = TRUE)
    heatmap22[x,y] <- ((a22/c22) / (b22/d22))
    
    a20 <- (y2020[x,y])
    b20 <- (y2020[35,y] - y2020[x,y])
    c20 <- (y2020[x,10] - y2020[x,y])
    d20 <- (y2020[35,10] - y2020[35,y] - y2020[x,10] + y2020[x,y])
    se20 <- sqrt(1/a20 + 1/b20 + 1/c20 + 1/d20)
    data20 <- matrix(c(a20,b20,c20,d20),nrow = 2, ncol = 2, byrow = TRUE)
    heatmap20[x,y] <- ((a20/c20) / (b20/d20))
    
    orchange[x,y] <- heatmap22[x,y] - heatmap20[x,y]
    logdelta[x,y] <- log10(heatmap22[x,y]) - log10(heatmap20[x,y])
    sedelta[x,y] <- sqrt((se22 * se22) + (se20*se20))
    if (sedelta[x,y] == Inf) {
      sedelta[x,y] <- 0
      # assign p value of NA
      pvalue[x,y] <- NA
    } else {
      pvalue[x,y] <- pnorm(q = logdelta[x,y], mean = 0, sd = sedelta[x,y])
    }
  }
}

# You use the difference as the variable and the p-value for the star
orchange
pvalue

##
# None are significant


star <- orchange
star[c(1,15),] <- NA
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
    newtest[a,5] <- NA
  }
}
colnames(newtest)[3] <- "Odds Ratio"
View(newtest)

x11()
ggplot(data = newtest, aes(x = variable, y = Specialty, fill = `Odds Ratio`)) +
  geom_tile() +
  theme_bw() +
  labs(title = "Change in Odds Ratios from 2020 to 2022") +
  theme(plot.title = element_text(size = 24, face = "bold",hjust = 0.5)) +
  theme(plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_fill_gradient2(low = "red", mid = "white", high = "steelblue", midpoint = 0, na.value = "gray80", breaks = c(-5, -3, -1, 1, 3, 5, 7, 9), limits = c(-5,9)) + 
  geom_text(aes(label = paste(round(textlabels, digits = 1), star)), color = "black", size = 4) +
  theme(legend.text = element_text(size = 16)) +
  theme(legend.position = "right", legend.box.just = "center") +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.key.size = unit(1.5, 'cm')) +
  geom_point(data = newtest, aes(size="0"), shape =NA, colour = "grey80") +
  guides(size=guide_legend("No Residents",  override.aes=list(shape = 15, size = 20),label = TRUE, direction = "vertical"))



#####
# Looking at totals for resident table
View(residents)
residents <- as.data.frame(residents)
# Total by ethnicity
residents[35,]
a <- residents[35,1:10] / 149353 * 100
View(a)
# Total by specialty
b <- residents[,c(1,11)]
b <- cbind(b, b$Unduplicated.Total / 149353 * 100)
View(b)




#####
# Heatmap for AI/AN only
naonly <- newtest[1:34,]
naonly <- as.data.frame(naonly)
x11()
ggplot(data = naonly, aes(x = variable, y = Specialty, fill = `Odds Ratio`)) +
  geom_tile() +
  theme_bw() +
  labs(title = "Odds Ratios: Native Resident Specialties") +
  theme(plot.title = element_text(size = 24, face = "bold")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_fill_gradient2(low = "red", mid = "white", high = "steelblue", midpoint = 1, na.value = "gray80", breaks = c(0.1, 1:5), limits = c(0.1,5)) +
  geom_text(aes(label = paste(round(textlabels, digits = 1), star)), color = "black", size = 4) +
  theme(legend.text = element_text(size = 16)) +
  theme(legend.title = element_blank()) +
  theme(legend.key.size = unit(1.5, "cm"))

#####
# heatmap md only
setwd('~/ANAMS paper/Heatmap/')
md <- read.csv('2022 Report on Residents - Table B5 (2).csv', na.strings = c("", "NA"))
md <- as.data.frame(md)
do <- read.csv('2022 Report on Residents - Table B6 (2).csv', na.strings = c("", "NA"))
do <- as.data.frame(do)
md[81:89, 17]
do[73:77,17]

#####
setwd('~/ANAMS paper/Heatmap/')
census <- read.csv('DECENNIALPL2020.P1-2023-08-22T001541.csv', na.strings = c("", "NA"))
census <- as.data.frame(census)
View(census)
# Make columns numeric
census$White <- as.numeric(census$White)
census$Black <- as.numeric(census$Black)
census$AI.AN <- as.numeric(census$AI.AN)
census$Asian <- as.numeric(census$Asian)
census$NH <- as.numeric(census$NH)
census$Other <- as.numeric(census$Other)
census$Total <- as.numeric(census$Total)
View(census)
str(census)

str(census[1,2])
# Change NA's to zeros
for (i in 1:71) {
  for (j in 2:7) {
    if (is.na(census[i,j])) {
      census[i,j] <- 0
    }
  }
}
View(census)
str(census)

# New table - only filling out first column here
newc <- matrix(0, 6, 4)
rownames(newc) <- c("White", "Black", "AI/AN", "Asian", "NH", "Other")
View(newc)

newc[1,1]

count <- 0

for (a in 1:71) {
  for (b in 1:6) {
    if (census[a,b+1] == 1) {
      newc[b,1] <- newc[b,1] + census[a, 8]
    } else {
      newc[b,1] <- newc[b,1] + 0
    }
  }
}

View(newc)


