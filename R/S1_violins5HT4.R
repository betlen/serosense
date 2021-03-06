# Figure S1 - violin plots for 5-HT4 at pH 4.8 and 7.2 (plasmid-based expression)

# Load all the packages needed
source(here::here("R/package_loading.R"))

# Load the dataframe
load(here::here("data/1C_data/df_38.Rda"))
load(here::here("data/1C_data/pH_df_363738.Rda"))


ph_low <-df_38 %>% dplyr::filter(strain != "yBL21") %>%  select(fluo, strain, ligand)
ph_low$type <- "low"
the_ligands <-c(rep("100 uM pH 4.8", 360000), rep("0 uM pH 4.8", 360000))
ph_low$ligand <-the_ligands
ph_high <- ph_363738 %>% dplyr::filter(plasmid == "38") %>%  select(fluo, strain, ligand)
ph_high$type <- "high"
the_ligands2 <-c(rep("100 uM pH 7.2", 360000), rep("0 uM pH 7.2", 360000))
ph_high$ligand <-the_ligands2
ph_all <-rbind(ph_low, ph_high)

plot_S1 <- ph_all %>%
            mutate(strain = factor(strain, levels=c("GPA1",
                                          "Gαs/olf",
                                          "Gα12",
                                          "Gα13",
                                          "Gαi1/2",
                                          "Gαi3",
                                          "Gαz",
                                          "Gα15/16",
                                          "Gαq/11",
                                          "Gα14",
                                          "Gαo",                                        "tGPA1"
  ))) %>%
  ggplot(aes(strain, fluo)) +
  theme_bw() +
  geom_violin(aes(fill = ligand), alpha = 0.4, draw_quantiles = c(0.25, 0.5, 0.75),
              linetype = 'dashed', scale = 'count', trim = T, show.legend = TRUE)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(trans = "log10") +
  facet_wrap( ~ strain, nrow = 2, scales = 'free_x') +
  annotation_logticks(sides="l") +
  labs(x = "strain", y = "fluorescence (a.u.)", fill = "Serotonin conc. [uM] + pH")+
  theme(text = element_text(size=20), axis.text.x=element_blank(), axis.ticks = element_blank())+
  scale_fill_viridis_d()
plot_S1

ggsave("outputs/plot_S1.png", width = 10, height = 10)

