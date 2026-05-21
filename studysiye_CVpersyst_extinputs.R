### set your own working directory and store all used files there
setwd('C:/Users/lopez061/LER')

### loads metafor and data.table packages
library(metafor)
library(data.table)
library(readxl)
library(ggplot2)
library(dplyr)

# --- read input data-----
data_all <- as.data.table(readxl::read_xlsx('data/merged_rotcov_noinfl_nometa.xlsx'))

# --- prepare data for analysis-----



# Recoding missing values in Location to "Not-specified"
data_all[is.na(Location), Location := "Not specified"]

# Recoding missing values in Year to "1"
data_all[is.na(Year), Year := 1]


#changing data classes
data_all[, Full_LER := as.factor(Full_LER)]

#----subset results of meta-analystical studies and primary studies-------
datap <- subset(data_all, meta=="No")
datas <- subset(data_all, meta=="Yes")

datap$Full_LER <- as.factor(datap$Full_LER)
datap$n_t <- as.numeric(datap$n_t)
datap$N_input <- as.numeric(datap$N_input)
datap$Fert_type <- as.factor(datap$Fert_type)
datap$Pesticide <- as.factor(datap$Pesticide)
datap$Irrigation <- as.factor(datap$Irrigation)


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


# Count the number of unique studies 
num_unique_studies_mono <- mono %>%
  summarise(num_unique_studies = n_distinct(study))

num_unique_studies_rotation <- rotation %>%
  summarise(num_unique_studies = n_distinct(study))

num_unique_studies_cover <- cover %>%
  summarise(num_unique_studies = n_distinct(study))

num_unique_studies_ic <- ic %>%
  summarise(num_unique_studies = n_distinct(study))

num_unique_studies_sparse <- sparse %>%
  summarise(num_unique_studies = n_distinct(study))

num_unique_studies_alley <- alley %>%
  summarise(num_unique_studies = n_distinct(study))

num_unique_studies_cropliv <- cropliv %>%
  summarise(num_unique_studies = n_distinct(study))

num_unique_studies_agroforest <- agroforest %>%
  summarise(num_unique_studies = n_distinct(study))

num_unique_studies_silvopas <- silvopas %>%
  summarise(num_unique_studies = n_distinct(study))

num_unique_studies_mono
num_unique_studies_rotation
num_unique_studies_cover
num_unique_studies_ic
num_unique_studies_sparse
num_unique_studies_alley
num_unique_studies_cropliv
num_unique_studies_agroforest
num_unique_studies_silvopas



# Dropping unused levels of FullLER
mono$Full_LER <- droplevels(mono$Full_LER)
rotation$Full_LER <- droplevels(rotation$Full_LER)
cover$Full_LER <- droplevels(cover$Full_LER)
ic$Full_LER <- droplevels(ic$Full_LER)
sparse$Full_LER <- droplevels(sparse$Full_LER)
alley$Full_LER <- droplevels(alley$Full_LER)
agroforest$Full_LER <- droplevels(agroforest$Full_LER)
silvopas$Full_LER <- droplevels(silvopas$Full_LER)
cropliv$Full_LER <- droplevels(cropliv$Full_LER)
croplivmin$Full_LER <- droplevels(croplivmin$Full_LER)
croplivmax$Full_LER <- droplevels(croplivmax$Full_LER)



# Recoding missing values in N_type, Pesticide and irrigation to "Unknown"
datap <- as.data.table(datap)
datap[is.na(Fert_type),Fert_type := "Unknown"]
datap[is.na(Pesticide), Pesticide := "Unknown"]
datap[is.na(Irrigation), Irrigation := "Unknown"]

country_count <- datap %>%
  group_by(div, Country) %>%
  summarise(count = n(), .groups = 'drop')
print(country_count, n=170)



# Apply the rma.mv function to the filtered data
# monoculture
# when data on variance (SD) is missing for the LER of the control and treated plot, estimate this from the CV of the other studies
mono <- as.data.table(mono)
mono[, LERt_cv := mean(sqrt(V_LERt)/LERt,na.rm=T) * 1.25]
mono[is.na(V_LERt), V_LERt := LERt * LERt_cv]




lV_LERt <- mono$V_LERt*((1/(mono$n_t*(mono$LERt)^2))+(1/mono$n_c))
res_M1p <- rma.mv(yi = log(LERt), 
                  V = lV_LERt, 
                  #mods = ~ Full_LER-1,
                  random = ~ 1 | study/siye, data=mono, verbose=TRUE, control=list(rel.tol=1e-8))

summary(res_M1p)
regtest(res_M1p$yi, res_M1p$vi, data=mono)
fsn(x=log(mono$LERt), vi=lV_LERt)

# Crop rotation 
rotation <- as.data.table(rotation)
rotation[, LERt_cv := mean(sqrt(V_LERt)/LERt,na.rm=T) * 1.25]
rotation[is.na(V_LERt), V_LERt := LERt * LERt_cv]
lV_LERt <- rotation$V_LERt*((1/(rotation$n_t*(rotation$LERt)^2))+(1/rotation$n_c))
res_rot <- rma.mv(yi = log(LERt), 
                  V = lV_LERt, 
                  mods = ~ Full_LER-1,
                  random = ~ 1 | study/siye, data=rotation)
res_rot
regtest(res_rot$yi, res_rot$vi)
fsn(x=log(rotation$LERt), vi=lV_LERt)

# Cover crops 
cover <- as.data.table(cover)
cover[, LERt_cv := mean(sqrt(V_LERt)/LERt,na.rm=T) * 1.25]
cover[is.na(V_LERt), V_LERt := LERt * LERt_cv]
lV_LERt <- cover$V_LERt*((1/(cover$n_t*(cover$LERt)^2))+(1/cover$n_c))
res_cover <- rma.mv(yi = log(LERt), 
                    V = lV_LERt, 
                    #mods = ~ Full_LER-1,
                    random = ~ 1 | study/siye, data=cover)
res_cover
regtest(res_cover$yi, res_cover$vi)
fsn(x=log(cover$LERt), vi=lV_LERt)

# Intercropping 
ic <- as.data.table(ic)
ic[, LERt_cv := mean(sqrt(V_LERt)/LERt,na.rm=T) * 1.25]
ic[is.na(V_LERt), V_LERt := LERt * LERt_cv]
lV_LERt <- ic$V_LERt*((1/(ic$n_t*(ic$LERt)^2))+(1/ic$n_c))
res_I1 <- rma.mv(yi = log(LERt), 
                 V = lV_LERt, 
                 mods = ~ Full_LER-1,
                 random = ~ 1 | study/siye, data=ic)
res_I1
regtest(res_I1$yi, res_I1$vi, method = "DL")
fsn(x=log(ic$LERt), vi=lV_LERt)

# Sparse trees 
sparse <- as.data.table(sparse)
sparse[, LERt_cv := mean(sqrt(V_LERt)/LERt,na.rm=T) * 1.25]
sparse[is.na(V_LERt), V_LERt := LERt * LERt_cv]
lV_LERt <- sparse$V_LERt*((1/(sparse$n_t*(sparse$LERt)^2))+(1/sparse$n_c))
res_ST1 <- rma.mv(yi = log(LERt), 
                  V = lV_LERt, 
                  mods = ~ Full_LER-1,
                  random = ~ 1 | study/siye, data=sparse)
res_ST1
regtest(res_ST1$yi, res_ST1$vi)
fsn(x=log(sparse$LERt), vi=lV_LERt)

# Tree+crop 
alley <- as.data.table(alley)
alley[, LERt_cv := mean(sqrt(V_LERt)/LERt,na.rm=T) * 1.25]
alley[is.na(V_LERt), V_LERt := LERt * LERt_cv]
lV_LERt <- alley$V_LERt*((1/(alley$n_t*(alley$LERt)^2))+(1/alley$n_c))
res_treecrop <- rma.mv(yi = log(LERt), 
                       V = lV_LERt, 
                       mods = ~ Full_LER-1,
                       random = ~ 1 | study/siye, data=alley)
res_treecrop
regtest(res_treecrop$yi, res_treecrop$vi)
fsn(x=log(alley$LERt), vi=lV_LERt)

# Agroforest 
agroforest <- as.data.table(agroforest)
#agroforest[, LERt_cv := mean(sqrt(V_LERt)/LERt,na.rm=T) * 1.25]
LERt_cv <- 1.38214452772197
#agroforest[is.na(V_LERt), V_LERt := LERt * LERt_cv]
#agroforest[, V_LERt := LERt * LERt_cv]
agroforest$V_LERt <- agroforest$LERt*LERt_cv
lV_LERt <- agroforest$V_LERt*((1/(agroforest$n_t*(agroforest$LERt)^2))+(1/agroforest$n_c))
res_AF <- rma.mv(yi = log(LERt), 
                 V = lV_LERt, 
                 mods = ~ Full_LER-1,
                 random = ~ 1 | study/siye, data=agroforest)
res_AF
regtest(res_AF$yi, res_AF$vi)
fsn(res_AF$yi, res_AF$vi)
fsn(x=log(agroforest$LERt), vi=lV_LERt)

# Silvopastoralism
silvopas <- as.data.table(silvopas)
silvopas[, LERt_cv := mean(sqrt(V_LERt)/LERt,na.rm=T) * 1.25]
silvopas[is.na(V_LERt), V_LERt := LERt * LERt_cv]
lV_LERt <- silvopas$V_LERt*((1/(silvopas$n_t*(silvopas$LERt)^2))+(1/silvopas$n_c))
res_SP <- rma.mv(yi = log(LERt), 
                 V = lV_LERt, 
                 mods = ~ Full_LER-1,
                 random = ~ 1 | study/siye, data=silvopas)
res_SP
regtest(res_SP$yi, res_SP$vi)
fsn(res_SP$yi, res_SP$vi)

# Crop-livestock integrated systems
cropliv <- as.data.table(cropliv)
cropliv[, LERt_cv := mean(sqrt(V_LERt)/LERt,na.rm=T) * 1.25]
cropliv[is.na(V_LERt), V_LERt := LERt * LERt_cv]
lV_LERt <- cropliv$V_LERt*((1/(cropliv$n_t*(cropliv$LERt)^2))+(1/cropliv$n_c))
res_cl <- rma.mv(yi = log(LERt), 
                 V = lV_LERt, 
                 mods = ~ Full_LER-1,
                 random = ~ 1 | study/siye, data=cropliv)
res_cl
regtest(res_cl$yi, res_cl$vi)
fsn(res_cl$yi, res_cl$vi)



# Crop-livestock integrated systems
croplivmin <- as.data.table(croplivmin)
croplivmin[, LERt_cv := mean(sqrt(V_LERt)/LERt,na.rm=T) * 1.25]
croplivmin[is.na(V_LERt), V_LERt := LERt * LERt_cv]
lV_LERt <- croplivmin$V_LERt*((1/(croplivmin$n_t*(croplivmin$LERt)^2))+(1/croplivmin$n_c))
res_clmin <- rma.mv(yi = log(LERt), 
                    V = lV_LERt, 
                    mods = ~ Full_LER-1,
                    random = ~ 1 | study/siye, data=croplivmin)
res_clmin


# Crop-livestock integrated systems
croplivmax <- as.data.table(croplivmax)
croplivmax[, LERt_cv := mean(sqrt(V_LERt)/LERt,na.rm=T) * 1.25]
croplivmax[is.na(V_LERt), V_LERt := LERt * LERt_cv]
lV_LERt <- croplivmax$V_LERt*((1/(croplivmax$n_t*(croplivmax$LERt)^2))+(1/croplivmax$n_c))
res_clmax <- rma.mv(yi = log(LERt), 
                    V = lV_LERt, 
                    mods = ~ Full_LER-1,
                    random = ~ 1 | study/siye, data=croplivmax)
res_clmax






#Count number of observations by type of LER for each diversified system
counts <- datap %>%
  group_by(div, Full_LER) %>%
  summarise(k = n(), .groups = 'drop')
print(counts, n=30)


#Extract values from meta-analyses
# List of models
models <- list(
  "Silvopastoralism" = res_SP,
  "Agroforest" = res_AF,
  "Trees and crops" = res_treecrop,
  "Sparse trees" = res_ST1,
  "Integrated crop-livestock" = res_cl,
  "Intercrop" = res_I1,
  "Rotation" = res_rot,
  "Cover crops" = res_cover,
  "Orchards" = res_M1p
)

# Initialize an empty data frame to store results
results_combined <- data.frame()

# Loop through each model and extract the results
for (model_name in names(models)) {
  res <- summary(models[[model_name]])
  model_results <- data.frame(
    Model = model_name,
    Moderator = rownames(res$beta),
    Estimate = res$beta[, 1],
    CI.Lower = res$ci.lb,
    CI.Upper = res$ci.ub,
    p = res$pval,
    k = res$k.eff  # Number of effective studies
  )
  results_combined <- rbind(results_combined, model_results)
}


# Copy the combined table to the clipboard for easy pasting
write.table(results_combined, "clipboard", sep="\t", row.names=FALSE, col.names=TRUE)







########################################################################################
##### RESULTS cropliv ######################
########################################################################################

##########Plotting results subgroup analysis
# List of models
models <- list(
  "Minimum" = res_clmin,
  "Average" = res_cl,
  "Maximum" = res_clmax
)

# Initialize an empty data frame to store results
results_combined <- data.frame()

# Loop through each model and extract the results
for (model_name in names(models)) {
  res <- summary(models[[model_name]])
  model_results <- data.frame(
    Model = model_name,
    Moderator = rownames(res$beta),
    Estimate = res$beta[, 1],
    CI.Lower = res$ci.lb,
    CI.Upper = res$ci.ub,
    p = res$pval,
    k = res$k.eff  # Number of effective studies
  )
  results_combined <- rbind(results_combined, model_results)
}

# Copy the combined table to the clipboard for easy pasting
write.table(results_combined, "clipboard", sep="\t", row.names=FALSE, col.names=TRUE)



########################################################################################
##### MODERATORS ANALYSIS ######################
########################################################################################




# ----Filter only full LER
#MONOCULTURE
monoF <- mono %>%
  filter(Full_LER == "Full")

monoF <- as.data.table(monoF)

# Recoding missing values in Location to "Not-specified"
monoF[is.na(Location), Location := "Not specified"]

# Recoding missing values in Year to "1"
monoF[is.na(Year), Year := 1]

monoF <- monoF %>%
  mutate(siye = paste(Location, Year, sep = " "))

#changing data classes
monoF[, Full_LER := as.factor(Full_LER)]


monoF$Full_LER <- as.factor(monoF$Full_LER)
monoF$n_t <- as.numeric(monoF$n_t)
monoF$N_input <- as.numeric(monoF$N_input)
monoF$Fert_type <- as.factor(monoF$Fert_type)
monoF$Pesticide <- as.factor(monoF$Pesticide)
monoF$Irrigation <- as.factor(monoF$Irrigation)


# when data on variance (SD) is missing for the LER of the control and treated plot, estimate this from the CV of the other studies
monoF[, LERt_cv := mean(sqrt(V_LERt)/LERt,na.rm=T) * 1.25]
monoF[is.na(V_LERt), V_LERt := LERt * LERt_cv]




#COVER CROPS
coverF <- cover %>%
  filter(Full_LER == "Full")

coverF <- as.data.table(coverF)

# Recoding missing values in Location to "Not-specified"
coverF[is.na(Location), Location := "Not specified"]

# Recoding missing values in Year to "1"
coverF[is.na(Year), Year := 1]

coverF <- coverF %>%
  mutate(siye = paste(Location, Year, sep = " "))

#changing data classes
coverF[, Full_LER := as.factor(Full_LER)]


coverF$Full_LER <- as.factor(coverF$Full_LER)
coverF$n_t <- as.numeric(coverF$n_t)
coverF$N_input <- as.numeric(coverF$N_input)
coverF$Fert_type <- as.factor(coverF$Fert_type)
coverF$Pesticide <- as.factor(coverF$Pesticide)
coverF$Irrigation <- as.factor(coverF$Irrigation)



# when data on variance (SD) is missing for the LER of the control and treated plot, estimate this from the CV of the other studies
coverF[, LERt_cv := mean(sqrt(V_LERt)/LERt,na.rm=T) * 1.25]
coverF[is.na(V_LERt), V_LERt := LERt * LERt_cv]





#CROP ROTATION
rotationF <- rotation %>%
  filter(Full_LER == "Full")

rotationF <- as.data.table(rotationF)

# Recoding missing values in Location to "Not-specified"
rotationF[is.na(Location), Location := "Not specified"]

# Recoding missing values in Year to "1"
rotationF[is.na(Year), Year := 1]

rotationF <- rotationF %>%
  mutate(siye = paste(Location, Year, sep = " "))


#changing data classes
rotationF[, Full_LER := as.factor(Full_LER)]


rotationF$Full_LER <- as.factor(rotationF$Full_LER)
rotationF$n_t <- as.numeric(rotationF$n_t)
rotationF$N_input <- as.numeric(rotationF$N_input)
rotationF$Fert_type <- as.factor(rotationF$Fert_type)
rotationF$Pesticide <- as.factor(rotationF$Pesticide)
rotationF$Irrigation <- as.factor(rotationF$Irrigation)


# when data on variance (SD) is missing for the LER of the control and treated plot, estimate this from the CV of the other studies
rotationF[, LERt_cv := mean(sqrt(V_LERt)/LERt,na.rm=T) * 1.25]
rotationF[is.na(V_LERt), V_LERt := LERt * LERt_cv]




#INTERCROPPING
icF <- ic %>%
  filter(Full_LER == "Full")

icF <- as.data.table(icF)

# Recoding missing values in Location to "Not-specified"
icF[is.na(Location), Location := "Not specified"]

# Recoding missing values in Year to "1"
icF[is.na(Year), Year := 1]

icF <- icF %>%
  mutate(siye = paste(Location, Year, sep = " "))

#changing data classes
icF[, Full_LER := as.factor(Full_LER)]


icF$Full_LER <- as.factor(icF$Full_LER)
icF$n_t <- as.numeric(icF$n_t)
icF$N_input <- as.numeric(icF$N_input)
icF$Fert_type <- as.factor(icF$Fert_type)
icF$Pesticide <- as.factor(icF$Pesticide)
icF$Irrigation <- as.factor(icF$Irrigation)


# when data on variance (SD) is missing for the LER of the control and treated plot, estimate this from the CV of the other studies
icF[, LERt_cv := mean(sqrt(V_LERt)/LERt,na.rm=T) * 1.25]
icF[is.na(V_LERt), V_LERt := LERt * LERt_cv]



#SPARSE TREES
sparseF <- sparse %>%
  filter(Full_LER == "Full")

sparseF <- as.data.table(sparseF)

# Recoding missing values in Location to "Not-specified"
sparseF[is.na(Location), Location := "Not specified"]

# Recoding missing values in Year to "1"
sparseF[is.na(Year), Year := 1]

sparseF <- sparseF %>%
  mutate(siye = paste(Location, Year, sep = " "))

#changing data classes
sparseF[, Full_LER := as.factor(Full_LER)]


sparseF$Full_LER <- as.factor(sparseF$Full_LER)
sparseF$n_t <- as.numeric(sparseF$n_t)
sparseF$N_input <- as.numeric(sparseF$N_input)
sparseF$Fert_type <- as.factor(sparseF$Fert_type)
sparseF$Pesticide <- as.factor(sparseF$Pesticide)
sparseF$Irrigation <- as.factor(sparseF$Irrigation)


# when data on variance (SD) is missing for the LER of the control and treated plot, estimate this from the CV of the other studies
LERt_cv <- 1.38214452772197
sparseF$V_LERt <- sparseF$LERt*LERt_cv
#sparseF[, LERt_cv := mean(sqrt(V_LERt)/LERt,na.rm=T) * 1.25]
#sparseF[is.na(V_LERt), V_LERt := LERt * LERt_cv]






#ALLEY CROPPING
alleyF <- alley %>%
  filter(Full_LER == "Full")

alleyF <- as.data.table(alleyF)

# Recoding missing values in Location to "Not-specified"
alleyF[is.na(Location), Location := "Not specified"]

# Recoding missing values in Year to "1"
alleyF[is.na(Year), Year := 1]

alleyF <- alleyF %>%
  mutate(siye = paste(Location, Year, sep = " "))

#changing data classes
alleyF[, Full_LER := as.factor(Full_LER)]


alleyF$Full_LER <- as.factor(alleyF$Full_LER)
alleyF$n_t <- as.numeric(alleyF$n_t)
alleyF$N_input <- as.numeric(alleyF$N_input)
alleyF$Fert_type <- as.factor(alleyF$Fert_type)
alleyF$Pesticide <- as.factor(alleyF$Pesticide)
alleyF$Irrigation <- as.factor(alleyF$Irrigation)


# when data on variance (SD) is missing for the LER of the control and treated plot, estimate this from the CV of the other studies
alleyF[, LERt_cv := mean(sqrt(V_LERt)/LERt,na.rm=T) * 1.25]
alleyF[is.na(V_LERt), V_LERt := LERt * LERt_cv]



#CROP LIVESTOCK INTEGRATED SYSTEMS
croplivF <- cropliv %>%
  filter(Full_LER == "Full")

croplivF <- as.data.table(croplivF)

# Recoding missing values in Location to "Not-specified"
croplivF[is.na(Location), Location := "Not specified"]

# Recoding missing values in Year to "1"
croplivF[is.na(Year), Year := 1]

croplivF <- croplivF %>%
  mutate(siye = paste(Location, Year, sep = " "))

#changing data classes
croplivF[, Full_LER := as.factor(Full_LER)]


croplivF$Full_LER <- as.factor(croplivF$Full_LER)
croplivF$n_t <- as.numeric(croplivF$n_t)
croplivF$N_input <- as.numeric(croplivF$N_input)
croplivF$Fert_type <- as.factor(croplivF$Fert_type)
croplivF$Pesticide <- as.factor(croplivF$Pesticide)
croplivF$Irrigation <- as.factor(croplivF$Irrigation)


# when data on variance (SD) is missing for the LER of the control and treated plot, estimate this from the CV of the other studies
croplivF[, LERt_cv := mean(sqrt(V_LERt)/LERt,na.rm=T) * 1.25]
croplivF[is.na(V_LERt), V_LERt := LERt * LERt_cv]



# Count the number of unique studies 
num_unique_studies_monoF <- monoF %>%
  summarise(num_unique_studies = n_distinct(study))

num_unique_studies_rotationF <- rotationF %>%
  summarise(num_unique_studies = n_distinct(study))

num_unique_studies_coverF <- coverF %>%
  summarise(num_unique_studies = n_distinct(study))

num_unique_studies_icF <- icF %>%
  summarise(num_unique_studies = n_distinct(study))

num_unique_studies_sparseF <- sparseF %>%
  summarise(num_unique_studies = n_distinct(study))

num_unique_studies_alleyF <- alleyF %>%
  summarise(num_unique_studies = n_distinct(study))
num_unique_studies_alleyF

num_unique_studies_croplivF <- croplivF %>%
  summarise(num_unique_studies = n_distinct(study))



# ----- Moderators analysis for only full LER
# count number of data points
mods_counts <- datap %>%
  filter(Full_LER == "Full") %>%
  group_by(div, N_input) %>%
  summarise(count = n(), .groups = 'drop')
print(mods_counts, n=150)

country_count <- datap %>%
  group_by(div, Country) %>%
  summarise(count = n(), .groups = 'drop')
print(country_count, n=160)



#MONOCULTURE PERENNIALS

############################ Fertilizer ###################################### 
lV_LERt <- monoF$V_LERt*((1/(monoF$n_t*(monoF$LERt)^2))+(1/monoF$n_c))
resM1p_N <- rma.mv(yi = log(LERt), 
                   V = lV_LERt, 
                   mods = ~N_input, #*factor(Fert_type)-1,
                   random = ~ 1 | study/siye, data=monoF)

summary(resM1p_N)


############################ BY Pesticide use ###################################### 
resM1p_P <- rma.mv(yi = log(LERt), 
                   V = lV_LERt, 
                   mods = ~factor(Pesticide)-1,
                   random = ~ 1 | study/siye, data=monoF)
resM1p_P
############################ BY Irrigation ###################################### 
resM1p_I <- rma.mv(yi = log(LERt), 
                   V = lV_LERt, 
                   mods = ~factor(Irrigation)-1,
                   random = ~ 1 | study/siye, data=monoF)



resM1p_I


# ----COVER CROPS
lV_LERt <- coverF$V_LERt*((1/(coverF$n_t*(coverF$LERt)^2))+(1/coverF$n_c))
############################ BY Fertilizer, Cover Crop full LER ###################################### 
res_cov_N <- rma.mv(yi = log(LERt), 
                    V = lV_LERt, 
                    mods = ~N_input, #*factor(Fert_type)-1,
                    random = ~ 1 | study/siye, data=coverF)
res_cov_N

############################ BY Pesticide use ###################################### 

rescov_P <- rma.mv(yi = log(LERt), 
                   V = lV_LERt, 
                   mods = ~Pesticide-1,
                   random = ~ 1 | study/siye, data=coverF)

summary(rescov_P)
#No irrigation data for cover crops


# ----CROP ROTATION
lV_LERt <- rotationF$V_LERt*((1/(rotationF$n_t*(rotationF$LERt)^2))+(1/rotationF$n_c))
############################ BY Fertilizer, Crop rotation full lER ###################################### 
res_S1_N <- rma.mv(yi = log(LERt), 
                   V = lV_LERt, 
                   mods = ~N_input, #*factor(Fert_type)-1,
                   random = ~ 1 | study/siye, data=rotationF)
res_S1_N
#No pesticides nor irrigation data for crop rotation

#INTERCROPPING
lV_LERt <- icF$V_LERt*((1/(icF$n_t*(icF$LERt)^2))+(1/icF$n_c))
############################ BY Fertilizer, intercropping full LER ###################################### 
res_ic_N <- rma.mv(yi = log(LERt), 
                   V = lV_LERt, 
                   mods = ~N_input, #*factor(Fert_type)-1,
                   random = ~ 1 | study/siye, data=icF)
res_ic_N

############################ BY Pesticide use ###################################### 

res_ic_P <- rma.mv(yi = log(LERt), 
                   V = lV_LERt, 
                   mods = ~factor(Pesticide)-1,
                   random = ~ 1 | study/siye, data=icF)

res_ic_P
############################ BY Irrigation, TREATMENT 2 ###################################### 
res_ic_I <- rma.mv(yi = log(LERt), 
                   V = lV_LERt, 
                   mods = ~factor(Irrigation)-1,
                   random = ~ 1 | study/siye, data=icF)

res_ic_I

#SPARSE TREES IN AGRICULTURAL LAND
lV_LERt <- sparseF$V_LERt*((1/(sparseF$n_t*(sparseF$LERt)^2))+(1/sparseF$n_c))
############################ BY Fertilizer, sparse full LER ###################################### 
res_sparse_N <- rma.mv(yi = log(LERt), 
                       V = lV_LERt, 
                       mods = ~N_input, #*factor(Fert_type)-1,
                       random = ~ 1 | study/siye, data=sparseF, verbose = TRUE, control=list(rel.tol=1e-8))
res_sparse_N


#ALLEY CROPPING
lV_LERt <- alleyF$V_LERt*((1/(alleyF$n_t*(alleyF$LERt)^2))+(1/alleyF$n_c))
############################ BY Fertilizer, alley full LER ###################################### 
res_alley_N <- rma.mv(yi = log(LERt), 
                      V = lV_LERt, 
                      mods = ~N_input, #*factor(Fert_type),
                      random = ~ 1 | study/siye, data=alleyF)
res_alley_N

############################ BY Irrigation, ###################################### 
res_alley_I <- rma.mv(yi = log(LERt), 
                      V = lV_LERt, 
                      mods = ~factor(Irrigation)-1,
                      random = ~ 1 | study/siye, data=alleyF)

res_alley_I

#CROP-LIVESTOCK integrated systems
lV_LERt <- croplivF$V_LERt*((1/(croplivF$n_t*(croplivF$LERt)^2))+(1/croplivF$n_c))
############################ BY Fertilizer, crop-livestock full LER ###################################### 
res_cl_N <- rma.mv(yi = log(LERt), 
                   V = lV_LERt, 
                   mods = ~N_input, #*factor(Fert_type)-1,
                   random = ~ 1 | study/siye, data=croplivF)
res_cl_N



######################## Copy results of subgroup analysis to excel #################################
# List of models
models <- list(
  resM1p_N = resM1p_N,
  resM1p_P = resM1p_P,
  resM1p_I = resM1p_I,
  res_cov_N = res_cov_N,
  rescov_P = rescov_P,
  res_S1_N = res_S1_N,
  res_ic_N = res_ic_N,
  res_ic_P = res_ic_P,
  res_ic_I = res_ic_I,
  res_sparse_N = res_sparse_N,
  res_alley_N = res_alley_N,
  res_alley_I = res_alley_I,
  res_cropliv_N = res_cl_N
  
)


# Initialize an empty data frame to store results
results_combined <- data.frame()

# Loop through each model and extract the results
for (model_name in names(models)) {
  res <- summary(models[[model_name]])
  model_results <- data.frame(
    Model = model_name,
    Moderator = rownames(res$beta),
    Estimate = res$beta[, 1],
    SE = res$se,
    Zval = res$zval,
    Pval = res$pval,
    CI.Lower = res$ci.lb,
    CI.Upper = res$ci.ub,
    n = res$k
  )
  results_combined <- rbind(results_combined, model_results)
}

# Copy the combined table to the clipboard for easy pasting
write.table(results_combined, "clipboard", sep="\t", row.names=FALSE, col.names=TRUE)


# Significant differences between categorical variables

#MONOCULTURE PERENNIALS
lV_LERt <- monoF$V_LERt*((1/(monoF$n_t*(monoF$LERt)^2))+(1/monoF$n_c))
############################ BY Pesticide use ###################################### 
resM1p_P <- rma.mv(yi = log(LERt), 
                   V = lV_LERt, 
                   mods = ~factor(Pesticide),
                   random = ~ 1 | study/siye, data=monoF)
resM1p_P
############################ BY Irrigation ###################################### 
resM1p_I <- rma.mv(yi = log(LERt), 
                   V = lV_LERt, 
                   mods = ~factor(Irrigation),
                   random = ~ 1 | study/siye, data=monoF)



resM1p_I


# ----COVER CROPS 
lV_LERt <- coverF$V_LERt*((1/(coverF$n_t*(coverF$LERt)^2))+(1/coverF$n_c))
############################ BY Pesticide use ###################################### 

rescover_P <- rma.mv(yi = log(LERt), 
                     V = lV_LERt, 
                     mods = ~Pesticide,
                     random = ~ 1 | study/siye, data=coverF)

summary(rescover_P)


#INTERCROPPING
lV_LERt <- icF$V_LERt*((1/(icF$n_t*(icF$LERt)^2))+(1/icF$n_c))
############################ BY Pesticide use ###################################### 

res_ic_P <- rma.mv(yi = log(LERt), 
                   V = lV_LERt, 
                   mods = ~factor(Pesticide),
                   random = ~ 1 | study/siye, data=icF)

res_ic_P
############################ BY Irrigation ###################################### 
res_ic_I <- rma.mv(yi = log(LERt), 
                   V = lV_LERt, 
                   mods = ~factor(Irrigation),
                   random = ~ 1 | study/siye, data=icF)

res_ic_I


#ALLEY CROPPING
lV_LERt <- alleyF$V_LERt*((1/(alleyF$n_t*(alleyF$LERt)^2))+(1/alleyF$n_c))
############################ BY Irrigation ###################################### 
res_alley_I <- rma.mv(yi = log(LERt), 
                      V = lV_LERt, 
                      mods = ~factor(Irrigation),
                      random = ~ 1 | study/siye, data=alleyF)

res_alley_I



# List of models
models <- list(
  resM1p_P = resM1p_P,
  resM1p_I = resM1p_I,
  rescover_P = rescover_P,
  res_ic_P = res_ic_P,
  res_ic_I = res_ic_I,
  res_alley_I = res_alley_I

)


# Initialize an empty data frame to store results
results_combined <- data.frame()

# Loop through each model and extract the results
for (model_name in names(models)) {
  res <- summary(models[[model_name]])
  model_results <- data.frame(
    Model = model_name,
    Moderator = rownames(res$beta),
    Estimate = res$beta[, 1],
    SE = res$se,
    Zval = res$zval,
    Pval = res$pval,
    CI.Lower = res$ci.lb,
    CI.Upper = res$ci.ub,
    n = res$k
  )
  results_combined <- rbind(results_combined, model_results)
}

# Copy the combined table to the clipboard for easy pasting
write.table(results_combined, "clipboard", sep="\t", row.names=FALSE, col.names=TRUE)


