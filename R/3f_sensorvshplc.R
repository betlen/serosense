# Figure 3F - sensor fluorescence compared to HPLC values

# Loading the packages
source(here::here("R/package_loading.R"))
# Loading the data
load(here::here("data/3F_df.rda"))


# MEDIAN AND MEANS
# SAMPLES
fig3f_sample_medians <- fig3f_dataframe %>%
  group_by(file_name, well, well2) %>%
  summarize(the_median = median(fluo, na.rm = T), dev = sd(fluo, na.rm = T))

fig3f_sample_meanofthree <- fig3f_sample_medians  %>%
  group_by(well) %>%
  summarize(meanofthree = mean(the_median, na.rm = T), dev = sd(the_median, na.rm = T))

fig3f_sample_meanofthree <- fig3f_sample_meanofthree%>%
  arrange(well)


###HPLC data
# load data and rename columns, change Amount column to numeric
HPLC <- read.csv("data/3F_20210409_HPLC.csv", sep = ",")
HPLC <- HPLC %>% rename(well2 = Sample2)
HPLC <- HPLC %>% rename(well = Sample1)
HPLC$Amount_uM <- as.numeric(HPLC$Amount_uM)

# calculate the mean of the HPLC triplicate samples
HPLC_means <- HPLC  %>%
  group_by(well) %>%
  summarize(meanofthree = mean(Amount_uM, na.rm = T), dev = sd(Amount_uM, na.rm = T))

# merge HPLC and flow data into one dataframe and rename columns
fig3f_combined_data <-  inner_join(HPLC_means, fig3f_sample_meanofthree, by = "well")
fig3f_combined_data <- fig3f_combined_data %>%
  rename(HPLC_uM = meanofthree.x, sensor_uM = meanofthree.y,
         HPLC_stdev = dev.x, sensor_stdev = dev.y)

fig3f_combined_data %>% ggplot(aes(HPLC_uM, sensor_uM)) +
  geom_point() +
labs(x = "Serotonin amount in uM, determined by HPLC", y = "fluorescence a.u.", col = "Sample")

write.csv(fig3f_combined_data, "outputs/fig3f_HPLC_fluo_for_prism.csv")

# data from csv are then imported into prism file
