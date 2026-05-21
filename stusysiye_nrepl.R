### set your own working directory and store all used files there
setwd('C:/Users/lopez061/LER')

### loads metafor and data.table packages
library(metafor)
library(data.table)
library(readxl)
library(ggplot2)
library(dplyr)

# --- read input data-----
data_all <- as.data.table(readxl::read_xlsx('data/merged_rotcov_noout_nometa.xlsx'))

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


# Recoding missing values in N_type, Pesticide and irrigation to "Unknown"
datap <- as.data.table(datap)
datap[is.na(Fert_type),Fert_type := "Unknown"]
datap[is.na(Pesticide), Pesticide := "Unknown"]
datap[is.na(Irrigation), Irrigation := "Unknown"]


# Apply the rma.mv function to the filtered data
# monoculture
lV_LERt <- mono$V_LERt*((1/(mono$n_t*(mono$LERt)^2))+(1/mono$n_c))
res_M1p <- rma.mv(yi = log(LERt), 
                  V = lV_LERt, 
                  W = n_t,
                  mods = ~ Full_LER-1,
                  random = ~ 1 | study/siye, data=mono, verbose=TRUE, control=list(rel.tol=1e-8))

summary(res_M1p)
regtest(res_M1p$yi, res_M1p$vi, data=mono)

# Cover crop 
lV_LERt <- cover$V_LERt*((1/(cover$n_t*(cover$LERt)^2))+(1/cover$n_c))
res_cover <- rma.mv(yi = log(LERt), 
                 V = lV_LERt, 
                 W = cover$n_t,
                 mods = ~ Full_LER-1,
                 random = ~ 1 | study/siye, data=cover)
res_cover
regtest(res_S1$yi, res_S1$vi)






# Crop rotation 
lV_LERt <- rotation$V_LERt*((1/(rotation$n_t*(rotation$LERt)^2))+(1/rotation$n_c))
res_S1 <- rma.mv(yi = log(LERt), 
                 V = lV_LERt, 
                 W = rotation$n_t,
                 mods = ~ Full_LER-1,
                 random = ~ 1 | study/siye, data=rotation)
res_S1
regtest(res_S1$yi, res_S1$vi)
# Intercropping 
lV_LERt <- ic$V_LERt*((1/(ic$n_t*(ic$LERt)^2))+(1/ic$n_c))
res_I1 <- rma.mv(yi = log(LERt), 
                 V = lV_LERt, 
                 W = ic$n_t,
                 mods = ~ Full_LER-1,
                 random = ~ 1 | study/siye, data=ic)
res_I1
regtest(res_I1$yi, res_I1$vi)

# Sparse trees 
lV_LERt <- sparse$V_LERt*((1/(sparse$n_t*(sparse$LERt)^2))+(1/sparse$n_c))
res_ST1 <- rma.mv(yi = log(LERt), 
                  V = lV_LERt, 
                  W = sparse$n_t,
                  mods = ~ Full_LER-1,
                  random = ~ 1 | study/siye, data=sparse)
res_ST1
regtest(res_ST1$yi, res_ST1$vi)
# Tree+crop 
lV_LERt <- alley$V_LERt*((1/(alley$n_t*(alley$LERt)^2))+(1/alley$n_c))
res_treecrop <- rma.mv(yi = log(LERt), 
                       V = lV_LERt, 
                       W = alley$n_t,
                       mods = ~ Full_LER-1,
                       random = ~ 1 | study/siye, data=alley)
res_treecrop
regtest(res_treecrop$yi, res_treecrop$vi)
# Agroforest 
lV_LERt <- agroforest$V_LERt*((1/(agroforest$n_t*(agroforest$LERt)^2))+(1/agroforest$n_c))
res_AF <- rma.mv(yi = log(LERt), 
                 V = lV_LERt, 
                 W = agroforest$n_t,
                 mods = ~ Full_LER-1,
                 random = ~ 1 | study/siye, data=agroforest)
res_AF
regtest(res_AF$yi, res_AF$vi)
# Silvopastoralism
lV_LERt <- silvopas$V_LERt*((1/(silvopas$n_t*(silvopas$LERt)^2))+(1/silvopas$n_c))
res_SP <- rma.mv(yi = log(LERt), 
                 V = lV_LERt, 
                 W = silvopas$n_t,
                 mods = ~ Full_LER-1,
                 random = ~ 1 | study/siye, data=silvopas)
res_SP
regtest(res_SP$yi, res_SP$vi)
# Crop-livestock integrated systems
lV_LERt <- cropliv$V_LERt*((1/(cropliv$n_t*(cropliv$LERt)^2))+(1/cropliv$n_c))
res_cl <- rma.mv(yi = log(LERt), 
                 V = lV_LERt, 
                 W = cropliv$n_t,
                 mods = ~ Full_LER-1,
                 random = ~ 1 | study/siye, data=cropliv)
res_cl
regtest(res_cl$yi, res_cl$vi)

########################################################################################
##### PLOTTING RESULTS OF Full, Incomplete and Underestimated LER ######################
########################################################################################
counts <- datap %>%
  group_by(div, Full_LER) %>%
  summarise(k = n(), .groups = 'drop')
print(counts, n=24)

##########Plotting results subgroup analysis
# List of models
models <- list(
  "Silvopastoralism" = res_SP,
  "Agroforest" = res_AF,
  "Trees and crops" = res_treecrop,
  "Sparse trees" = res_ST1,
  "Integrated crop-livestock" = res_cl,
  "Intercrop" = res_I1,
  "Rotation" = res_S1,
  "Cover" = res_cover,
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


# Back-transform the estimates and confidence intervals
results_combined$Estimate <- exp(results_combined$Estimate)
results_combined$CI.Lower <- exp(results_combined$CI.Lower)
results_combined$CI.Upper <- exp(results_combined$CI.Upper)

# Ensure Model is a factor with the levels in the desired order
results_combined$Model <- factor(results_combined$Model, levels = names(models))

# Clean the Moderator labels
results_combined$Moderator <- gsub("factor\\(([^)]+)\\)(.*)", "\\1\\2", results_combined$Moderator)

# Combine Model and Moderator into one variable
results_combined$Model_Moderator <- factor(paste(results_combined$Model, results_combined$Moderator, sep = " - "), 
                                           levels = unique(paste(results_combined$Model, results_combined$Moderator, sep = " - ")))


# Plotting the estimates and confidence intervals using ggplot2
ggplot(results_combined, aes(x = Model_Moderator, y = Estimate, ymin = CI.Lower, ymax = CI.Upper, color = Moderator)) +
  geom_pointrange() +
  geom_errorbar(aes(ymin = CI.Lower, ymax = CI.Upper), width = 0.2) +
  geom_point(aes(size = k), shape = 21, fill = "white") +  # Points representing the number of studies
  geom_hline(yintercept = 1, linetype = "dashed", color = "#333333", size = 1) +  # Reference line at 1 (assuming 1 is the reference value)
  coord_flip() +  # Flip coordinates to make it horizontal
  labs(title = "LER: Full, Incomplete and Partial",
       x = "Diversification system",
       y = "LER",
       size = "Number of Studies (k)",
       color = "Moderator") +  # Set legend title for color
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  scale_color_brewer(palette = "Set1")  # Optional: use a color palette for better distinction

















# ----Filter only full LER
monoF <- mono %>%
  filter(Full_LER == "Full")

rotationF <- rotation %>%
  filter(Full_LER == "Full")

icF <- ic %>%
  filter(Full_LER == "Full")

sparseF <- sparse %>%
  filter(Full_LER == "Full")

alleyF <- alley %>%
  filter(Full_LER == "Full")

croplivF <- cropliv %>%
  filter(Full_LER == "Full")


resM1pF<- rma.mv(yi = log(LERt), 
                 V = lV_LERt, 
                 random = ~ 1 | study/siye, data=monoF)

res_S1_F <- rma.mv(yi = log(LERt), 
                   V = lV_LERt, 
                   random = ~ 1 | study/siye, data=rotationF)
res_ic_F <- rma.mv(yi = log(LERt), 
                   V = lV_LERt, 
                   random = ~ 1 | study/siye, data=icF)
res_sparse_F <- rma.mv(yi = log(LERt), 
                       V = lV_LERt, 
                       random = ~ 1 | study/siye, data=sparseF)
res_alley_F <- rma.mv(yi = log(LERt), 
                      V = lV_LERt, 
                      random = ~ 1 | study/siye, data=alleyF)
res_cl_F <- rma.mv(yi = log(LERt), 
                   V = lV_LERt, 
                   random = ~ 1 | study/siye, data=croplivF)
forest(res_cl_F)


# Count the number of unique studies 
num_unique_studies_monoF <- monoF %>%
  summarise(num_unique_studies = n_distinct(study))

num_unique_studies_rotationF <- rotationF %>%
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
num_unique_studies_croplivF


# List of models
models <- list(
  resM1pF = resM1pF,
  resS1_F = res_S1_F,
  res_ic_F = res_ic_F,
  res_sparse_F = res_sparse_F,
  res_alley_F = res_alley_F,
  res_cl_F = res_cl_F
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
    CI.Upper = res$ci.ub
  )
  results_combined <- rbind(results_combined, model_results)
}

# Copy the combined table to the clipboard for easy pasting
write.table(results_combined, "clipboard", sep="\t", row.names=FALSE, col.names=TRUE)

##########Plotting results subgroup analysis
# List of models
models <- list(
  "Orchards n=26 obs=88" = resM1pF,
  "Rotation n=37 obs=288" = res_S1_F,
  "Intercrop n=102, obs=1137" = res_ic_F,
  "Sparse n=2, obs=28" = res_sparse_F,
  "Trees and crops n=24 obs=225" = res_alley_F,
  "Integrated crop-livestock n=7 obs=31" =res_cl_F
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
    k = res$k.eff  # Number of effective studies
  )
  results_combined <- rbind(results_combined, model_results)
}

# Back-transform the estimates and confidence intervals
results_combined$Estimate <- exp(results_combined$Estimate)
results_combined$CI.Lower <- exp(results_combined$CI.Lower)
results_combined$CI.Upper <- exp(results_combined$CI.Upper)


# Plotting the estimates and confidence intervals using ggplot2
ggplot(results_combined, aes(x = Model, y = Estimate, ymin = CI.Lower, ymax = CI.Upper, color = Moderator)) +
  geom_errorbar(aes(ymin = CI.Lower, ymax = CI.Upper), width = 0.2, color="blue") +
  geom_point(aes(size = k), shape = 21, fill = "white", color="blue") +  # Points representing the number of studies
  geom_hline(yintercept = 1, linetype = "dashed", color = "#333333", size = 1) +  # Reference line at 1 (assuming 1 is the reference value)
  coord_flip() +  # Flip coordinates to make it horizontal
  labs(title = "Full LER",
       x = "Diversification system",
       y = "LER",
       size = "Number of observations (k)",
       color = "Moderator") +  # Set legend title for color
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  scale_color_brewer(palette = "Set1")  # Optional: use a color palette for better distinction


























# ----- Moderators analysis for only full LER
# count number of data points
mods_counts <- datap %>%
  filter(Full_LER == "Full") %>%
  group_by(div, N_input) %>%
  summarise(count = n(), .groups = 'drop')
print(mods_counts, n=116)

country_count <- datap %>%
  group_by(div, Country) %>%
  summarise(count = n(), .groups = 'drop')
print(country_count, n=132)



#MONOCULTURE PERENNIALS

############################ Fertilizer ###################################### 
resM1p_N <- rma.mv(yi = log(LERt), 
                   V = lV_LERt, 
                   mods = ~N_input, #*factor(Fert_type)-1,
                   random = ~ 1 | study/siye, data=monoF)

summary(resM1p_N)

resM1p_Nt <- rma.mv(yi = log(LERt), 
                    V = lV_LERt, 
                    mods = ~factor(Fert_type)-1,
                    random = ~ 1 | study/siye, data=monoF)

resM1p_Nt
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


# ----CROP ROTATION

############################ BY Fertilizer, Crop rotation full lER ###################################### 
res_S1_N <- rma.mv(yi = log(LERt), 
                   V = lV_LERt, 
                   mods = ~N_input, #*factor(Fert_type)-1,
                   random = ~ 1 | study/siye, data=rotationF)
res_S1_N

res_S1_Nt <- rma.mv(yi = log(LERt), 
                    V = lV_LERt, 
                    mods = ~factor(Fert_type)-1,
                    random = ~ 1 | study/siye, data=rotationF)
res_S1_Nt
############################ BY Pesticide use ###################################### 

resS1_P <- rma.mv(yi = log(LERt), 
                  V = lV_LERt, 
                  mods = ~Pesticide-1,
                  random = ~ 1 | study/siye, data=rotationF)

summary(resS1_P)
#No irrigation data for crop rotation


#INTERCROPPING

############################ BY Fertilizer, intercropping full LER ###################################### 
res_ic_N <- rma.mv(yi = log(LERt), 
                   V = lV_LERt, 
                   mods = ~N_input, #*factor(Fert_type)-1,
                   random = ~ 1 | study/siye, data=icF)
res_ic_N

res_ic_Nt <- rma.mv(yi = log(LERt), 
                    V = lV_LERt, 
                    mods = ~factor(Fert_type)-1,
                    random = ~ 1 | study/siye, data=icF)
res_ic_Nt
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

############################ BY Fertilizer, sparse full LER ###################################### 
res_sparse_N <- rma.mv(yi = log(LERt), 
                       V = lV_LERt, 
                       mods = ~N_input, #*factor(Fert_type)-1,
                       random = ~ 1 | study/siye, data=sparseF)
res_sparse_N

res_sparse_Nt <- rma.mv(yi = log(LERt), 
                        V = lV_LERt, 
                        mods = ~factor(Fert_type)-1,
                        random = ~ 1 | study/siye, data=sparseF)
res_sparse_Nt


#ALLEY CROPPING

############################ BY Fertilizer, alley full LER ###################################### 
res_alley_N <- rma.mv(yi = log(LERt), 
                      V = lV_LERt, 
                      mods = ~N_input, #*factor(Fert_type),
                      random = ~ 1 | study/siye, data=alleyF)
res_alley_N

res_alley_Nt <- rma.mv(yi = log(LERt), 
                       V = lV_LERt, 
                       mods = ~factor(Fert_type)-1,
                       random = ~ 1 | study/siye, data=alleyF)
res_alley_Nt

############################ BY Irrigation, TREATMENT 2 ###################################### 
res_alley_I <- rma.mv(yi = log(LERt), 
                      V = lV_LERt, 
                      mods = ~factor(Irrigation)-1,
                      random = ~ 1 | study/siye, data=alleyF)

res_alley_I

#CROP-LIVESTOCK integrated systems

############################ BY Fertilizer, crop-livestock full LER ###################################### 
res_cl_N <- rma.mv(yi = log(LERt), 
                   V = lV_LERt, 
                   mods = ~N_input, #*factor(Fert_type)-1,
                   random = ~ 1 | study/siye, data=croplivF)
res_cl_N

res_cl_Nt <- rma.mv(yi = log(LERt), 
                    V = lV_LERt, 
                    mods = ~factor(Fert_type)-1,
                    random = ~ 1 | study/siye, data=croplivF)
res_cl_Nt
############################ BY Pesticide use ###################################### 

res_cl_P <- rma.mv(yi = log(LERt), 
                   V = lV_LERt, 
                   mods = ~factor(Pesticide)-1,
                   random = ~ 1 | study/siye, data=croplivF)

res_cl_P
############################ BY Irrigation, TREATMENT 2 ###################################### 
res_cl_I <- rma.mv(yi = log(LERt), 
                   V = lV_LERt, 
                   mods = ~factor(Irrigation)-1,
                   random = ~ 1 | study/siye, data=croplivF)

res_cl_I


# List of models
models <- list(
  resM1p_N = resM1p_N,
  resM1p_Nt = resM1p_Nt,
  resM1p_P = resM1p_P,
  resM1p_I = resM1p_I,
  res_S1_N = res_S1_N,
  res_S1_Nt = res_S1_Nt,
  resS1_P = resS1_P,
  res_ic_N = res_ic_N,
  res_ic_Nt = res_ic_Nt,
  res_ic_P = res_ic_P,
  res_ic_I = res_ic_I,
  res_sparse_N = res_sparse_N,
  res_sparse_Nt = res_sparse_Nt,
  res_alley_N = res_alley_N,
  res_alley_Nt = res_alley_Nt,
  res_alley_I = res_alley_I,
  res_cropliv_N = res_cl_N,
  res_cropliv_Nt = res_cl_Nt,
  res_cropliv_P = res_cl_P,
  res_cropliv_I = res_cl_I
  
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
    CI.Upper = res$ci.ub
  )
  results_combined <- rbind(results_combined, model_results)
}

# Copy the combined table to the clipboard for easy pasting
write.table(results_combined, "clipboard", sep="\t", row.names=FALSE, col.names=TRUE)

##########Plotting results subgroup analysis
# List of models
models <- list(
  "Alley I" = res_alley_I,
  "Alley F" = res_alley_Nt,
  "Sparse F" = res_sparse_Nt,
  "Crop-livestock I" = res_cl_I,
  "Crop-livestock P" = res_cl_P,
  "Crop-livestock F" = res_cl_Nt,
  "Intercrop I" = res_ic_I,
  "Intercrop P" = res_ic_P,
  "Intercrop F" = res_ic_Nt,
  "Rotation P" = resS1_P,
  "Rotation F" = res_S1_Nt,
  "Orchards I" = resM1p_I,
  "Orchards P" = resM1p_P,
  "Orchards F" = resM1p_Nt,
  "Orchards F" = resM1p_Nt
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
    k = res$k.eff  # Number of effective studies
  )
  results_combined <- rbind(results_combined, model_results)
}

# Back-transform the estimates and confidence intervals
results_combined$Estimate <- exp(results_combined$Estimate)
results_combined$CI.Lower <- exp(results_combined$CI.Lower)
results_combined$CI.Upper <- exp(results_combined$CI.Upper)

# Ensure Model is a factor with the levels in the desired order
results_combined$Model <- factor(results_combined$Model, levels = names(models))

# Clean the Moderator labels
results_combined$Moderator <- gsub("factor\\(([^)]+)\\)(.*)", "\\1\\2", results_combined$Moderator)

# Combine Model and Moderator into one variable
results_combined$Model_Moderator <- factor(paste(results_combined$Model, results_combined$Moderator, sep = " - "), 
                                           levels = unique(paste(results_combined$Model, results_combined$Moderator, sep = " - ")))

# Plotting the estimates and confidence intervals using ggplot2
ggplot(results_combined, aes(x = Model_Moderator, y = Estimate, ymin = CI.Lower, ymax = CI.Upper, color = Moderator)) +
  geom_pointrange() +
  geom_errorbar(aes(ymin = CI.Lower, ymax = CI.Upper), width = 0.2) +
  geom_point(aes(size = k), shape = 21, fill = "white") +  # Points representing the number of studies
  geom_hline(yintercept = 1, linetype = "dashed", color = "#333333", size = 1) +  # Reference line at 1 (assuming 1 is the reference value)
  coord_flip() +  # Flip coordinates to make it horizontal
  labs(title = "Influence of use of inputs on LER",
       x = "Diversification system and Input subgroup",
       y = "LER",
       size = "Number of Studies (k)",
       color = "Moderator") +  # Set legend title for color
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  scale_color_brewer(palette = "Set1")  # Optional: use a color palette for better distinction





# Interactions between inputs
