rm(list=ls(all=T))
options(stringsASFactors = F) # no automatic data transformation
options("scipen" = 100, "digits" = 4) # supress math annotation

# Set file location w/ tabbing
setwd('~/')
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
                       "Pediatrics", "PM&R", "Plastic Surgery", "Psychiatry", "Public Health & Preventative Medicine",
                       "Radiation Oncology", "Radiology", "Interventional Radiology", "General Surgery",
                       "Thoracic Surgery", "Transitional year", "Urology", "Vascular Surgery"
)
colnames(heatmap) <- c("AI/AN", "Asian", "Black", "Hispanic", "Native Hawaiian", "White", "Other",
                      "Unknown", "Non-US Citizen")
View(heatmap)

#####
# Final loop - if making without statistical testing and just displaying OR
# Graph needs some work
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

# Write figure to same file location
write.csv(heatmap, file = '/', row.names=TRUE)

heatmap <- cbind(rownames(heatmap), heatmap)
colnames(heatmap)[1] <- "Specialty"
View(heatmap)

heatmap <- melt(data = heatmap, id = "Specialty")
View(heatmap)

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
# loop to make heatmap with ORs and test at p < 0.05
heatmaporhigh <- matrix(0, 34, 9)
heatmaporlow <- matrix(0, 34, 9)

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

# Separate stars into a new column
test <- star
star <- separate(data = star, col = value, into = c("newvalue", "star"), sep = c("/"))

star$newvalue <- as.numeric(star$newvalue)
View(star)

# want star NA to be removed
for (a in 1:306) {
  if (is.na(star[a,4]) == TRUE) {
    star[a,4] <- c("")
  } else {
    star[a,4] <- c("*")
  }
  }
View(star)

# Make 1 column to map to colors to include Na, make 1 column map to labels so it is presented as zero
star <- cbind(star, star$newvalue)
colnames(star)[5] <- "textlabels"
View(star)
for (a in 1:306) {
  if(is.na(star[a,5])) {
    star[a,5] <- 0
  }
}
colnames(star)[3] <- "Odds Ratio"
View(star)

x11()
ggplot(data = star, aes(x = variable, y = Specialty, fill = `Odds Ratio`)) +
  geom_tile() +
  theme_bw() +
  labs(title = "2021-2022 Resident Race and Specialty") +
  theme(plot.title = element_text(size = 24, face = "bold")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_fill_gradient2(low = "red", mid = "white", high = "steelblue", midpoint = 1, na.value = "gray80", breaks = c(0.2, 1:9), limits = c(0.2,9)) + 
  geom_text(aes(label = paste(round(textlabels, digits = 1), star)), color = "black", size = 4) +
  theme(legend.text = element_text(size = 16)) +
  theme(legend.position = "right", legend.box.just = "center") +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.key.size = unit(1.5, 'cm')) +
  geom_point(data = star, aes(size="0"), shape =NA, colour = "grey80") +
  guides(size=guide_legend("No Residents",  override.aes=list(shape = 15, size = 20),label = TRUE, direction = "vertical"))

#####
# Heatmap for AI/AN only
  # or do for any race, replace 1:34 with the appropriate rows in star
naonly <- star[1:34,]
naonly <- as.data.frame(naonly)
View(naonly)
x11()
ggplot(data = naonly, aes(x = variable, y = Specialty, fill = `Odds Ratio`)) +
  geom_tile() +
  theme_bw() +
  labs(title = "Native Resident Specialties") +
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_fill_gradient2(low = "red", mid = "white", high = "steelblue", midpoint = 1, na.value = "gray80", breaks = c(0.4, 1:5), limits = c(0.4,5)) +
  geom_text(aes(label = paste(round(`textlabels`, digits = 1), star)), color = "black", size = 4) +
  theme(legend.text = element_text(size = 16)) +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.key.size = unit(1.5, "cm"))
