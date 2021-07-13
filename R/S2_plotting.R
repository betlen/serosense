# Figure S2 - violin plots of 5-HT4 plasmid-expression vs. integration

# Load all the packages needed
source(here::here("R/package_loading.R"))

# Load the dataframe
load(here::here("data/S2_dataframe.Rda"))

#  Arrange the strains in specific order

S2_dataframe <- S2_dataframe %>% mutate(strain2 = factor(strain2,
levels=c(
  "GPA1",
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
  "tGPA1",
  "yBL21"
)),
type = factor(type, levels=c(
  "plasmid",
  "integration"
)))

# Making ligand column a factor
S2_dataframe <- S2_dataframe %>% mutate(ligand = factor(ligand))

# Excluding strains not needed
S2_dataframe <- S2_dataframe %>% dplyr::filter(strain != "x") %>% dplyr::filter(strain != "yBL21")

### Plotting

plot_S2 <- S2_dataframe %>%
  ggplot(aes(strain2, fluo)) +
  theme_bw()+
  facet_wrap( . ~ strain2,  nrow = 2, scales = 'free_x') +
  geom_violin(aes(fill = ligand), alpha = 0.4, draw_quantiles = c(0.25, 0.5, 0.75),
              linetype = 'dashed', scale = 'count', trim = T, show.legend = TRUE)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  scale_y_continuous(trans = "log10")+
  annotation_logticks(sides="l") +
  labs(x = "", y = "fluorescence (a.u.)", fill = "Serotonin conc. [uM] + type") +
  theme(text = element_text(size=20), axis.text.x=element_blank(), axis.ticks = element_blank())+
  scale_fill_viridis_d()
plot_S2

ggsave("outputs/plot_S2.png", width = 10, height = 10)


