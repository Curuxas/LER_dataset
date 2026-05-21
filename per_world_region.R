### set your own working directory and store all used files there
setwd('C:/Users/lopez061/LER')

### loads metafor and data.table packages
library(metafor)
library(data.table)
library(readxl)
library(ggplot2)
library(dplyr)
library(countrycode)
library(tidyr)
library(writexl)
library(ggtext)
# --- read input data-----
datap <- as.data.table(readxl::read_xlsx('data/merged_rotcov_noout_nometa_regions.xlsx'))


# --- prepare data for analysis-----

# Recoding missing values in Location to "Not-specified"
datap[is.na(Location), Location := "Not specified"]

# Recoding missing values in Year to "1"
datap[is.na(Year), Year := 1]


#changing data classes
datap[, Full_LER := as.factor(Full_LER)]


datap$region <- countrycode(datap$Country,
                            origin = "country.name",
                            destination = "region")
datap$region <- factor(datap$region)

table_region_system <- datap %>%
  group_by(div, region) %>%
  summarise(
    k = n(),
    .groups = "drop"
  )
print(table_region_system,n=90)
table_wide <- table_region_system %>%
  pivot_wider(
    names_from = div,
    values_from = k,
    values_fill = 0
  )
write.table(table_wide, "clipboard", sep = "\t", row.names = FALSE)

#datap$Full_LER <- as.factor(datap$Full_LER)
#datap$n_t <- as.numeric(datap$n_t)


#checking the distribution of variance across study, location and year levels
datap <- datap %>%
  mutate(siye = paste(Location, Year, sep = " "))



dat_full <- subset(datap, Full_LER == "Full")
table_region_system <- dat_full %>%
  group_by(div, region) %>%
  summarise(
    k = n(),
    .groups = "drop"
  )
print(table_region_system,n=48)
table_wide <- table_region_system %>%
  pivot_wider(
    names_from = div,
    values_from = k,
    values_fill = 0
  )
write.table(table_wide, "clipboard", sep = "\t", row.names = FALSE)



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






# ----Filter only full LER
monoF <- mono %>%
  filter(Full_LER == "Full")

rotationF <- rotation %>%
  filter(Full_LER == "Full")

coverF <- cover %>%
  filter(Full_LER == "Full")

icF <- ic %>%
  filter(Full_LER == "Full")

sparseF <- sparse %>%
  filter(Full_LER == "Full")

alleyF <- alley %>%
  filter(Full_LER == "Full")

croplivF <- cropliv %>%
  filter(Full_LER == "Full")


#ORCHARDS
monoF <- as.data.table(monoF)
monoF[, LERt_cv := mean(sqrt(V_LERt)/LERt,na.rm=T) * 1.25]
monoF[is.na(V_LERt), V_LERt := LERt * LERt_cv]

monoF$lV_LERt <- monoF$V_LERt*((1/(monoF$n_t*(monoF$LERt)^2))+(1/monoF$n_c))
resM1pF<- rma.mv(yi = log(LERt), 
                 V = monoF$lV_LERt, 
                 mods = ~ factor(region),
                 random = ~ 1 | study/siye, data=monoF)

# ROTATION
rotationF <- as.data.table(rotationF)
rotationF[, LERt_cv := mean(sqrt(V_LERt)/LERt,na.rm=T) * 1.25]
rotationF[is.na(V_LERt), V_LERt := LERt * LERt_cv]
lV_LERt <- rotationF$V_LERt*((1/(rotationF$n_t*(rotationF$LERt)^2))+(1/rotationF$n_c))
res_rot_F <- rma.mv(yi = log(LERt), 
                    V = lV_LERt, 
                    mods = ~ factor(region),
                    random = ~ 1 | study/siye, data=rotationF)

#COVER CROPS
coverF <- as.data.table(coverF)
coverF[, LERt_cv := mean(sqrt(V_LERt)/LERt,na.rm=T) * 1.25]
coverF[is.na(V_LERt), V_LERt := LERt * LERt_cv]

lV_LERt <- coverF$V_LERt*((1/(coverF$n_t*(coverF$LERt)^2))+(1/coverF$n_c))
res_cover_F <- rma.mv(yi = log(LERt), 
                      V = lV_LERt, 
                      mods = ~ factor(region),
                      random = ~ 1 | study/siye, data=coverF)

#INTERCROPPING
icF <- as.data.table(icF)
icF[, LERt_cv := mean(sqrt(V_LERt)/LERt,na.rm=T) * 1.25]
icF[is.na(V_LERt), V_LERt := LERt * LERt_cv]
lV_LERt <- icF$V_LERt*((1/(icF$n_t*(icF$LERt)^2))+(1/icF$n_c))
res_ic_F <- rma.mv(yi = log(LERt), 
                   V = lV_LERt, 
                   mods = ~ factor(region),
                   random = ~ 1 | study/siye, data=icF)

#SPARSE
sparseF <- as.data.table(sparseF)
sparseF[, LERt_cv := mean(sqrt(V_LERt)/LERt,na.rm=T) * 1.25]
sparseF[is.na(V_LERt), V_LERt := LERt * LERt_cv]
lV_LERt <- sparseF$V_LERt*((1/(sparseF$n_t*(sparseF$LERt)^2))+(1/sparseF$n_c))
res_sparse_F <- rma.mv(yi = log(LERt), 
                       V = lV_LERt, 
                       mods = ~ factor(region),
                       random = ~ 1 | study/siye, data=sparseF)

#TREES AND CROPS
alleyF <- as.data.table(alleyF)
alleyF[, LERt_cv := mean(sqrt(V_LERt)/LERt,na.rm=T) * 1.25]
alleyF[is.na(V_LERt), V_LERt := LERt * LERt_cv]
lV_LERt <- alleyF$V_LERt*((1/(alleyF$n_t*(alleyF$LERt)^2))+(1/alleyF$n_c))
res_alley_F <- rma.mv(yi = log(LERt), 
                      V = lV_LERt,
                      mods = ~ factor(region),
                      random = ~ 1 | study/siye, data=alleyF)

#CROP-LIVESTOCK
croplivF <- as.data.table(croplivF)
croplivF[, LERt_cv := mean(sqrt(V_LERt)/LERt,na.rm=T) * 1.25]
croplivF[is.na(V_LERt), V_LERt := LERt * LERt_cv]
lV_LERt <- croplivF$V_LERt*((1/(croplivF$n_t*(croplivF$LERt)^2))+(1/croplivF$n_c))
res_cl_F <- rma.mv(yi = log(LERt), 
                   V = lV_LERt, 
                   mods = ~ factor(region),
                   random = ~ 1 | study/siye, data=croplivF)

resM1pF
res_cover_F
res_rot_F
res_ic_F
res_sparse_F
res_alley_F
res_cl_F

models <- list(
  Orchards = resM1pF,
  Cover = res_cover_F,
  Rotation = res_rot_F,
  Intercropping = res_ic_F,
  Alley = res_alley_F,
  Cropliv = res_cl_F
)

extract_coefs <- function(model, system_name) {
  coefs <- as.data.frame(coef(summary(model)))
  coefs$term <- rownames(coefs)
  coefs$system <- system_name
  rownames(coefs) <- NULL
  coefs
}

coef_table <- do.call(
  rbind,
  mapply(extract_coefs, models, names(models), SIMPLIFY = FALSE)
)

extract_model_info <- function(model, system_name) {
  data.frame(
    system = system_name,
    k = model$k,
    QM = model$QM,
    QM_df = model$m,
    QM_p = model$QMp,
    QE = model$QE,
    QE_df = model$k - model$p,
    QE_p = model$QEp,
    tau2_total = sum(model$sigma2)
  )
}

model_table <- do.call(
  rbind,
  mapply(extract_model_info, models, names(models), SIMPLIFY = FALSE)
)

combined_table <- merge(coef_table, model_table, by = "system", all.x = TRUE)
coef_table$signif <- ifelse(coef_table$pval < 0.05, "*", "")
coef_table <- coef_table[order(coef_table$system, coef_table$term), ]
write.table(combined_table, "clipboard", sep = "\t", row.names = FALSE)



######## RESULTS PER WORLD REGION #######
#Test of moderators only significant for orchrds, intercropping and cropliv, so we will only test these

#Orchards
monoF$lV_LERt <- monoF$V_LERt*((1/(monoF$n_t*(monoF$LERt)^2))+(1/monoF$n_c))
resM1pF<- rma.mv(yi = log(LERt), 
                 V = monoF$lV_LERt, 
                 mods = ~ factor(region)-1,
                 random = ~ 1 | study/siye, data=monoF)

#Intercropping
icF$lV_LERt <- icF$V_LERt*((1/(icF$n_t*(icF$LERt)^2))+(1/icF$n_c))
res_ic_F <- rma.mv(yi = log(LERt), 
                   V = icF$lV_LERt, 
                   mods = ~ factor(region)-1,
                   random = ~ 1 | study/siye, data=icF)

#Crop-livestock integrated systems
croplivF <- as.data.table(croplivF)
croplivF[, LERt_cv := mean(sqrt(V_LERt)/LERt,na.rm=T) * 1.25]
croplivF[is.na(V_LERt), V_LERt := LERt * LERt_cv]
croplivF$lV_LERt <- croplivF$V_LERt*((1/(croplivF$n_t*(croplivF$LERt)^2))+(1/croplivF$n_c))
res_cl_F <- rma.mv(yi = log(LERt), 
                   V = croplivF$lV_LERt, 
                   mods = ~ factor(region)-1,
                   random = ~ 1 | study/siye, data=croplivF)

resM1pF
res_ic_F
res_cl_F


models <- list(
  Orchards = resM1pF,
  Intercropping = res_ic_F,
  Cropliv = res_cl_F
)

extract_coefs <- function(model, system_name) {
  coefs <- as.data.frame(coef(summary(model)))
  coefs$term <- rownames(coefs)
  coefs$system <- system_name
  rownames(coefs) <- NULL
  coefs
}

coef_table <- do.call(
  rbind,
  mapply(extract_coefs, models, names(models), SIMPLIFY = FALSE)
)

extract_model_info <- function(model, system_name) {
  data.frame(
    system = system_name,
    k = model$k,
    QM = model$QM,
    QM_df = model$m,
    QM_p = model$QMp,
    QE = model$QE,
    QE_df = model$k - model$p,
    QE_p = model$QEp,
    tau2_total = sum(model$sigma2)
  )
}

model_table <- do.call(
  rbind,
  mapply(extract_model_info, models, names(models), SIMPLIFY = FALSE)
)

combined_table <- merge(coef_table, model_table, by = "system", all.x = TRUE)
coef_table$signif <- ifelse(coef_table$pval < 0.05, "*", "")
coef_table <- coef_table[order(coef_table$system, coef_table$term), ]
write.table(combined_table, "clipboard", sep = "\t", row.names = FALSE)

