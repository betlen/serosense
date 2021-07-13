# Figure 1C - serotonin library heatmap screen

# Loading the packages
source(here::here("R/package_loading.R"))

## Loading the dataset
load(file="data/1C_induction.Rda")


wide_df <- induction_all_1C %>%
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
                                          "Gαo",
                                          "tGPA1"))) %>%
  mutate(plasmid_2 = factor(plasmid_2, levels=c("5-HT1A",
                                                "5-HT1B",
                                                "5-HT1D",
                                                "5-HT1E",
                                                "5-HT1F",
                                                "5-HT2A",
                                                "5-HT2B",
                                                "5-HT2C",
                                                "5-HT4",
                                                "5-HT5A",
                                                "5-HT6",
                                                "5-HT7"))) %>%
  spread(strain, fold_change)


plot_df <- wide_df %>%
  unite(rnames,c(type, plasmid_2), sep = "_", remove = TRUE, na.rm = FALSE)


plot_df$rnames <-make.names(plot_df$rnames, unique = TRUE)
plot_df  <- plot_df %>% column_to_rownames(var = "rnames")

#Create annotation
annotation <- data.frame(pH = factor(rep(c("4.8", "7.2"), 12)))
rownames(annotation) <- rownames(plot_df) # check out the row names of annotation

#Change annotation colours
pH     <- c("lightgrey", "#41424C")
names(pH) <- c("4.8", "7.2")
anno_colors <- list(pH = pH)



map <- pheatmap(mat = plot_df,
                scale = "none",
                cluster_rows = F, cluster_cols = F,
                color = colorRampPalette(c("#FFFFFF","#55C66755",
                                           "#238A8DFF", "#404788FF"))(20),
                breaks = c(0,1.4,2,3, 4, 5,6, 7, 8, 9, 10, 15, 20, 25, 30, 35, 40, 45, 50,55, 60,65),
                gaps_row = c(2,4,6,8, 10, 12, 14, 16, 18, 20, 22),
                annotation_row = annotation,
                annotation_colors = anno_colors)
map
