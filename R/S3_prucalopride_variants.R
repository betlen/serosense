source(here::here("R/package_loading.R"))

load(here::here("data/S3_dataframe.Rda"))

#for dose responses, get rid of plate_checks
total_all_dose<-total_all %>% dplyr::filter(!grepl('plate_control', sample_type))


p <- ggplot(total_all_dose, aes(x=conc, y=fluo)) +
  geom_violin(aes(fill = variant), alpha = 0.4, draw_quantiles = c(0.25, 0.5, 0.75), scale = 'count', trim = T)+
  scale_y_continuous(trans = "log10")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  facet_wrap(~ type)
p

#Calculate means for Prism analysis
total_for_means <-total_all_dose

#calculate median for each sample
plot_df_median <- total_all_dose %>%
  group_by(ligand,variant, well,conc) %>%
  summarize(med = median(fluo, na.rm = T), dev = sd(fluo, na.rm = T), themean = mean(fluo, na.rm=T))

# calculate mean of the three medians
plot_df_mean <- plot_df_median  %>%
  group_by(variant,conc) %>%
  summarize(meanofthree = mean(med, na.rm = T), dev = sd(med, na.rm = T))

#get rid of uM
plot_df_mean$conc = as.numeric(gsub("\\uM", "", plot_df_mean$conc))

# concentration curves
plot_df_means_filtered <- plot_df_means
plot_df_means_filtered  %>%
  ggplot(aes(conc, meanofthree)) +
  geom_point() +
  geom_errorbar(ymin = plot_df_means_filtered$meanofthree - plot_df_means_filtered$dev,
                ymax = plot_df_means_filtered$meanofthree + plot_df_means_filtered$dev,
                width=0.3,
                size=0.5) +
  scale_x_continuous(trans = "log10")+
  facet_wrap(. ~ variant, nrow = 4, scales = 'free_x') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x = "conc. in uM", y = "fluorescence (a.u.)")


data_for_prism <- plot_df_mean %>% mutate(assay = "07.07.2021")
write.csv(data_for_prism,"outputs/S3_means_of_medians.csv")
