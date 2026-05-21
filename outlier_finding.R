### set your own working directory and store all used files there
setwd('C:/Users/lopez061/LER')

### loads metafor and data.table packages
library(metafor)
library(data.table)
library(readxl)
library(ggplot2)
library(dplyr)

# --- read input data-----
data_all <- as.data.table(readxl::read_xlsx('data/merged_und_cricno_tcyes_outliersin_nometa.xlsx'))

# --- prepare data for analysis-----
# when data on variance (SD) is missing for the LER of the control and treated plot, estimate this from the CV of the other studies
data_all[, LERt_cv := mean(sqrt(V_LERt)/LERt,na.rm=T) * 1.25]
data_all[is.na(V_LERt), V_LERt := LERt * LERt_cv]

# Recoding missing values in Location to "Not-specified"
data_all[is.na(Location), Location := "Not specified"]

# Recoding missing values in Year to "1"
data_all[is.na(Year), Year := 1]


#changing data classes
data_all[, Full_LER := as.factor(Full_LER)]

#----subset results of meta-analystical studies and primary studies-------
datap <- subset(data_all, meta=="No")
datas <- subset(data_all, meta=="Yes")

### Calculating the natural log of variance of LERt
datap$n_t <- as.numeric(datap$n_t)
datap$N_input <- as.numeric(datap$N_input)
datap$Fert_type <- as.factor(datap$Fert_type)
datap$Pesticide <- as.factor(datap$Pesticide)
datap$Irrigation <- as.factor(datap$Irrigation)
lV_LERt <- datap$V_LERt*((1/(datap$n_t*(datap$LERt)^2))+(1/datap$n_c))

#checking the distribution of variance across study, location and year levels
datap <- datap %>%
  mutate(siye = paste(Location, Year, sep = " "))


# grouping data by agricultural system before analysis
datap[,system := div]
datap <- datap %>% group_by(system)


# --- analysis -----
#I know that the variable I want to study does not follow a normal distribution but I will use log(LER), so with the log it should be normal
#random effects model at the experiment (study x location_year) level

# Filter the data to include only the specific group

mono <- datap %>%filter(system == "mono")
rotation <- datap %>%filter(system == "rotation")
cover <- datap %>%filter(system == "cover")
ic <- datap %>%filter(system == "ic")
sparse <- datap %>%filter(system == "sparse")
alley <- datap %>%filter(system == "alley")
agroforest <- datap %>%filter(system == "agroforest")
silvopas <- datap %>%filter(system == "silvopas")
cropliv <- datap %>%filter(system == "cropliv")
croplivmin <- datap %>%filter(system == "croplivmin")
croplivmax <- datap %>%filter(system == "croplivmax")

datap <- as.data.table(datap)

# Recoding missing values in N_type, Pesticide and irrigation to "Unknown"
datap[is.na(Fert_type),Fert_type := "Unknown"]
datap[is.na(Pesticide), Pesticide := "Unknown"]
datap[is.na(Irrigation), Irrigation := "Unknown"]


# Apply the rma.mv function to the filtered data
# monoculture
lV_LERt <- mono$V_LERt*((1/(mono$n_t*(mono$LERt)^2))+(1/mono$n_c))
res_M1p <- rma.mv(yi = log(LERt), 
                  V = lV_LERt, 
                  random = ~ 1 | study/siye, data=mono, verbose=TRUE, control=list(rel.tol=1e-8))

summary(res_M1p)

# Calculate Cook's distance
cooks_dist <- cooks.distance(res_M1p)


# Identify influential studies based on Cook's distance
influential_cutoff <- 4 / nrow(mono)
influential_studies <- cooks_dist > influential_cutoff

# Print the results
mono$cooks_dist <- cooks_dist
mono$influential <- influential_studies

# Plot Cook's distance
plot(cooks_dist, type="h", main="Cook's Distance", xlab="Study", ylab="Cook's Distance")
abline(h = influential_cutoff, col = "red")


# Calculate Cook's distance
cooks_dist <- cooks.distance(res_M1p)  # 'res' is your meta-analysis model

# Define the influential cutoff (commonly used value is 4/n)
influential_cutoff <- 4 / length(cooks_dist)

# Plot Cook's distance
plot(cooks_dist, type="h", main="Cook's Distance", ylab="Cook's Distance")
abline(h=influential_cutoff, col="red", lty=2)

# Highlight influential points
influential_points <- which(cooks_dist > influential_cutoff)
points(influential_points, cooks_dist[influential_points], col="red", pch=19)

# Add labels to influential points (optional)
text(influential_points, cooks_dist[influential_points], labels=influential_points, pos=3, cex=0.8)

# Subset the original data frame to get all columns for the influential studies
influential_studies <- mono[influential_points, ]

# Print the subset data frame
print(influential_studies)





# Crop rotation 
lV_LERt <- rotation$V_LERt*((1/(rotation$n_t*(rotation$LERt)^2))+(1/rotation$n_c))
res_S1 <- rma.mv(yi = log(LERt), 
                 V = lV_LERt, 
                 mods = ~ Full_LER-1,
                 random = ~ 1 | study/siye, data=rotation)
res_S1

# Calculate Cook's distance
cooks_dist <- cooks.distance(res_S1)

# Define the influential cutoff (commonly used value is 4/n)
influential_cutoff <- 4 / nrow(rotation)
influential_cutoff <- 4 / length(cooks_dist)
influential_studies <- cooks_dist > influential_cutoff

# Plot Cook's distance
plot(cooks_dist, type="h", main="Cook's Distance", ylab="Cook's Distance")
abline(h=influential_cutoff, col="red", lty=2)

# Highlight influential points
influential_points <- which(cooks_dist > influential_cutoff)
points(influential_points, cooks_dist[influential_points], col="red", pch=19)

# Add labels to influential points (optional)
text(influential_points, cooks_dist[influential_points], labels=influential_points, pos=3, cex=0.8)

# Subset the original data frame to get all columns for the influential studies
influential_studies <- rotation[influential_points, ]

# Print the subset data frame
print(influential_studies)


# Cover crops
lV_LERt <- cover$V_LERt*((1/(cover$n_t*(cover$LERt)^2))+(1/cover$n_c))
res_cover <- rma.mv(yi = log(LERt), 
                 V = lV_LERt, 
                 mods = ~ Full_LER-1,
                 random = ~ 1 | study/siye, data=cover)
res_cover

# Calculate Cook's distance
cooks_dist <- cooks.distance(res_cover)

# Define the influential cutoff (commonly used value is 4/n)
influential_cutoff <- 4 / nrow(cover)
influential_cutoff <- 4 / length(cooks_dist)
influential_studies <- cooks_dist > influential_cutoff

# Plot Cook's distance
plot(cooks_dist, type="h", main="Cook's Distance", ylab="Cook's Distance")
abline(h=influential_cutoff, col="red", lty=2)

# Highlight influential points
influential_points <- which(cooks_dist > influential_cutoff)
points(influential_points, cooks_dist[influential_points], col="red", pch=19)

# Add labels to influential points (optional)
text(influential_points, cooks_dist[influential_points], labels=influential_points, pos=3, cex=0.8)

# Subset the original data frame to get all columns for the influential studies
influential_studies <- cover[influential_points, ]

# Print the subset data frame
print(influential_studies)
















# Intercropping 
lV_LERt <- ic$V_LERt*((1/(ic$n_t*(ic$LERt)^2))+(1/ic$n_c))
res_I1 <- rma.mv(yi = log(LERt), 
                 V = lV_LERt, 
                 mods = ~ Full_LER-1,
                 random = ~ 1 | study/siye, data=ic, sparse = TRUE)
res_I1

# Calculate Cook's distance
cooks_dist <- cooks.distance(res_I1)

# Define the influential cutoff (commonly used value is 4/n)
influential_cutoff <- 4 / length(cooks_dist)
influential_studies <- cooks_dist > influential_cutoff

# Plot Cook's distance
plot(cooks_dist, type="h", main="Cook's Distance Intercropping", ylab="Cook's Distance")
abline(h=influential_cutoff, col="red", lty=2)

# Highlight influential points
influential_points <- which(cooks_dist > influential_cutoff)
points(influential_points, cooks_dist[influential_points], col="red", pch=19)

# Add labels to influential points (optional)
text(influential_points, cooks_dist[influential_points], labels=influential_points, pos=3, cex=0.8)

# Subset the original data frame to get all columns for the influential studies
influential_studies <- ic[influential_points, ]

# Print the subset data frame
print(influential_studies)






















# Sparse trees 
lV_LERt <- sparse$V_LERt*((1/(sparse$n_t*(sparse$LERt)^2))+(1/sparse$n_c))
res_ST1 <- rma.mv(yi = log(LERt), 
                  V = lV_LERt, 
                  #mods = ~ Full_LER-1,
                  random = ~ 1 | study/siye, data=sparse)
res_ST1

# Calculate Cook's distance
cooks_dist <- cooks.distance(res_ST1)

# Define the influential cutoff (commonly used value is 4/n)
influential_cutoff <- 4 / length(cooks_dist)
influential_studies <- cooks_dist > influential_cutoff

# Plot Cook's distance
plot(cooks_dist, type="h", main="Cook's Distance Sparse trees", ylab="Cook's Distance")
abline(h=influential_cutoff, col="red", lty=2)

# Highlight influential points
influential_points <- which(cooks_dist > influential_cutoff)
points(influential_points, cooks_dist[influential_points], col="red", pch=19)

# Add labels to influential points (optional)
text(influential_points, cooks_dist[influential_points], labels=influential_points, pos=3, cex=0.8)

# Subset the original data frame to get all columns for the influential studies
influential_studies <- sparse[influential_points, ]

# Print the subset data frame
print(influential_studies)
























# Tree+crop
lV_LERt <- alley$V_LERt*((1/(alley$n_t*(alley$LERt)^2))+(1/alley$n_c))
res_treecrop <- rma.mv(yi = log(LERt), 
                       V = lV_LERt, 
                       mods = ~ Full_LER-1,
                       random = ~ 1 | study/siye, data=alley)
res_treecrop

# Calculate Cook's distance
cooks_dist <- cooks.distance(res_treecrop)

# Define the influential cutoff (commonly used value is 4/n)
influential_cutoff <- 4 / length(cooks_dist)
influential_studies <- cooks_dist > influential_cutoff

# Plot Cook's distance
plot(cooks_dist, type="h", main="Cook's Distance Trees and crops", ylab="Cook's Distance")
abline(h=influential_cutoff, col="red", lty=2)

# Highlight influential points
influential_points <- which(cooks_dist > influential_cutoff)
points(influential_points, cooks_dist[influential_points], col="red", pch=19)

# Add labels to influential points (optional)
text(influential_points, cooks_dist[influential_points], labels=influential_points, pos=3, cex=0.8)

# Subset the original data frame to get all columns for the influential studies
influential_studies <- alley[influential_points, ]

# Print the subset data frame
print(influential_studies)




















# Agroforest 

# Make a copy of the original data
lV_LERt <- agroforest$V_LERt*((1/(agroforest$n_t*(agroforest$LERt)^2))+(1/agroforest$n_c))
res_AF <- rma.mv(yi = log(LERt), 
                 V = lV_LERt, 
                 #mods = ~ Full_LER-1,
                 random = ~ 1 | study/siye, data=agroforest)


print(is.na(agroforest$Full_LER))

res_AF

# Calculate Cook's distance
cooks_dist <- cooks.distance(res_AF)

# Define the influential cutoff (commonly used value is 4/n)
influential_cutoff <- 4 / length(cooks_dist)
influential_studies <- cooks_dist > influential_cutoff

# Plot Cook's distance
plot(cooks_dist, type="h", main="Cook's Distance Agroforest", ylab="Cook's Distance")
abline(h=influential_cutoff, col="red", lty=2)

# Highlight influential points
influential_points <- which(cooks_dist > influential_cutoff)
points(influential_points, cooks_dist[influential_points], col="red", pch=19)

# Add labels to influential points (optional)
text(influential_points, cooks_dist[influential_points], labels=influential_points, pos=3, cex=0.8)

# Subset the original data frame to get all columns for the influential studies
influential_studies <- agroforest[influential_points, ]

# Print the subset data frame
print(influential_studies)



















# Silvopastoralism
lV_LERt <- silvopas$V_LERt*((1/(silvopas$n_t*(silvopas$LERt)^2))+(1/silvopas$n_c))
res_SP <- rma.mv(yi = log(LERt), 
                 V = lV_LERt, 
                 #mods = ~ Full_LER-1,
                 random = ~ 1 | study/siye, data=silvopas)
res_SP

# Calculate Cook's distance
cooks_dist <- cooks.distance(res_SP)

# Define the influential cutoff (commonly used value is 4/n)
influential_cutoff <- 4 / length(cooks_dist)
influential_studies <- cooks_dist > influential_cutoff

# Plot Cook's distance
plot(cooks_dist, type="h", main="Cook's Distance Silvopastoralism", ylab="Cook's Distance")
abline(h=influential_cutoff, col="red", lty=2)

# Highlight influential points
influential_points <- which(cooks_dist > influential_cutoff)
points(influential_points, cooks_dist[influential_points], col="red", pch=19)

# Add labels to influential points (optional)
text(influential_points, cooks_dist[influential_points], labels=influential_points, pos=3, cex=0.8)

# Subset the original data frame to get all columns for the influential studies
influential_studies <- silvopas[influential_points, ]

# Print the subset data frame
print(influential_studies)







#Crop-livestock integrated systems
lV_LERt <- cropliv$V_LERt*((1/(cropliv$n_t*(cropliv$LERt)^2))+(1/cropliv$n_c))
res_cl <- rma.mv(yi = log(LERt), 
                 V = lV_LERt, 
                 mods = ~ Full_LER-1,
                 random = ~ 1 | study/siye, data=cropliv)
res_cl

# Calculate Cook's distance
cooks_dist <- cooks.distance(res_cl)

# Define the influential cutoff (commonly used value is 4/n)
influential_cutoff <- 4 / nrow(cropliv)
influential_cutoff <- 4 / length(cooks_dist)
influential_studies <- cooks_dist > influential_cutoff

# Plot Cook's distance
plot(cooks_dist, type="h", main="Cook's Distance Crop-livestock integrated", ylab="Cook's Distance")
abline(h=influential_cutoff, col="red", lty=2)

# Highlight influential points
influential_points <- which(cooks_dist > influential_cutoff)
points(influential_points, cooks_dist[influential_points], col="red", pch=19)

# Add labels to influential points (optional)
text(influential_points, cooks_dist[influential_points], labels=influential_points, pos=3, cex=0.8)

# Subset the original data frame to get all columns for the influential studies
influential_studies <- cropliv[influential_points, ]

# Print the subset data frame
print(influential_studies)












#Crop-livestock integrated systems_MIN
lV_LERt <- croplivmin$V_LERt*((1/(croplivmin$n_t*(croplivmin$LERt)^2))+(1/croplivmin$n_c))
res_clmin <- rma.mv(yi = log(LERt), 
                 V = lV_LERt, 
                 #mods = ~ Full_LER-1,
                 random = ~ 1 | study/siye, data=croplivmin)
res_clmin

# Calculate Cook's distance
cooks_dist <- cooks.distance(res_clmin)

# Define the influential cutoff (commonly used value is 4/n)
influential_cutoff <- 4 / nrow(croplivmin)
influential_cutoff <- 4 / length(cooks_dist)
influential_studies <- cooks_dist > influential_cutoff

# Plot Cook's distance
plot(cooks_dist, type="h", main="Cook's Distance Crop-livestock integrated MIN", ylab="Cook's Distance")
abline(h=influential_cutoff, col="red", lty=2)

# Highlight influential points
influential_points <- which(cooks_dist > influential_cutoff)
points(influential_points, cooks_dist[influential_points], col="red", pch=19)

# Add labels to influential points (optional)
text(influential_points, cooks_dist[influential_points], labels=influential_points, pos=3, cex=0.8)

# Subset the original data frame to get all columns for the influential studies
influential_studies <- croplivmin[influential_points, ]

# Print the subset data frame
print(influential_studies)








#Crop-livestock integrated systems_MAX
lV_LERt <- croplivmax$V_LERt*((1/(croplivmax$n_t*(croplivmax$LERt)^2))+(1/croplivmax$n_c))
res_clmax <- rma.mv(yi = log(LERt), 
                    V = lV_LERt, 
                    #mods = ~ Full_LER-1,
                    random = ~ 1 | study/siye, data=croplivmax)
res_clmax

# Calculate Cook's distance
cooks_dist <- cooks.distance(res_clmax)

# Define the influential cutoff (commonly used value is 4/n)
influential_cutoff <- 4 / nrow(croplivmax)
influential_cutoff <- 4 / length(cooks_dist)
influential_studies <- cooks_dist > influential_cutoff

# Plot Cook's distance
plot(cooks_dist, type="h", main="Cook's Distance Crop-livestock integrated MAX", ylab="Cook's Distance")
abline(h=influential_cutoff, col="red", lty=2)

# Highlight influential points
influential_points <- which(cooks_dist > influential_cutoff)
points(influential_points, cooks_dist[influential_points], col="red", pch=19)

# Add labels to influential points (optional)
text(influential_points, cooks_dist[influential_points], labels=influential_points, pos=3, cex=0.8)

# Subset the original data frame to get all columns for the influential studies
influential_studies <- croplivmax[influential_points, ]

# Print the subset data frame
print(influential_studies)
