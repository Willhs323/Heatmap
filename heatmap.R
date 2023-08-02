rm(list=ls(all=T))
options(stringsASFactors = F) # no automatic data transformation
options("scipen" = 100, "digits" = 4) # supress math annotation

setwd('ANAMS paper/AAIP presentation/')
residents <- read.csv('residents.csv', na.strings = c("", "NA"))
str(residents)

residents <- as.data.frame(residents)

residents <- residents[,-c(1)]
View(residents)

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
                       "Pediatrics", "PM&R", "Plastic Surgery", "Psychiatry", "Public Health & General Preventative Medicine",
                       "Radiation Oncology", "Radiology", "Interventional Radiology", "General Surgery",
                       "Thoracic Surgery", "Transitional year", "Urology", "Vascular Surgery"
)
colnames(heatmap) <- c("AI/AN", "Asian", "Black", "Hispanic", "Native Hawaiian", "White", "Other",
                      "Unknown", "Non-US Citizen")
View(heatmap)

heatmaporhigh <- matrix(0, 34, 9)
heatmaporlow <- matrix(0, 34, 9)

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
    heatmap[x,y] <- ((a/b) / (c/d))
    
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

# Now get asterisks in statistically significant OR where low > 1 and high < 1

star <- heatmap
for (x in 1:34){
  for (y in 1:9) {
    if (heatmaporhigh[x,y] < 1 | heatmaporlow[x,y] > 1) {
      # add * to data in heatmap
      star[x,y] <- paste(star[x,y], "/*")
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
View(test)

test <- separate(data = test, col = value, into = c("newvalue", "star"), sep = c("/"))
test$newvalue <- as.numeric(test$newvalue)
test <- replace(test, is.na(test), "")
View(test)

x11()
ggplot(data = test, aes(x = variable, y = Specialty, fill = newvalue)) +
  geom_tile() +
  theme_bw() +
  labs(title = "Odds Ratios: Resident Race and Specialty") +
  theme(plot.title = element_text(size = 24, face = "bold")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_fill_gradient2(low = "red", mid = "white", high = "steelblue", midpoint = 1) +
  geom_text(aes(label = paste(round(newvalue, digits = 1), star)), color = "black", size = 4) +
  theme(legend.text = element_text(size = 16)) +
  theme(legend.title = element_blank()) +
  theme(legend.key.size = unit(1.5, "cm"))

write.csv(heatmap, file = '/users/M276066/Documents/ANAMS paper/Heatmap/heatmap.csv', row.names=TRUE)

#####
# Heatmap for AI/AN only
naonly <- test[1:34,]
naonly <- as.data.frame(naonly)
x11()
ggplot(data = naonly, aes(x = variable, y = Specialty, fill = newvalue)) +
  geom_tile() +
  theme_bw() +
  labs(title = "Odds Ratios: Native Resident Specialties") +
  theme(plot.title = element_text(size = 24, face = "bold")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_fill_gradient2(low = "red", mid = "white", high = "steelblue", midpoint = 1) +
  geom_text(aes(label = paste(round(newvalue, digits = 1), star)), color = "black", size = 4) +
  theme(legend.text = element_text(size = 16)) +
  theme(legend.title = element_blank()) +
  theme(legend.key.size = unit(1.5, "cm"))
