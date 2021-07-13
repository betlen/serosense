# Figure 3D - ratio supernatant:biosensor

# Loading the packages
source(here::here("R/package_loading.R"))
# Loading the data
load(here::here("data/3D_dataframe.rda"))


# summary calculations

# calculate median for each sample
fig3d_medians <- dataframe_3d %>%
  group_by(strain.1, conc, media, ratio) %>%
  summarize(meds = median(fluo, na.rm = T), dev = sd(fluo, na.rm = T), themean = mean(fluo, na.rm=T))

# calculate mean of the three medians
fig3d_means_of_medians <- fig3d_medians %>%
  group_by(conc, media, ratio) %>%
  summarize(meanofthree = mean(meds, na.rm = T), dev = sd(meds, na.rm = T))

fig3d_means_of_medians$conc <- as.numeric(as.character(fig3d_means_of_medians$conc))
fig3d_means_of_medians <- fig3d_means_of_medians %>% filter(media != "24h SM" && media != "control")

# export data to a csv dataframe
write.csv(fig3d_means_of_medians, "~/Documents/R projects/serosense/outputs/fig3d_ratiosadded.csv")

# data from csv are then imported into prism file
