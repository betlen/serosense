# Figure 1C - serotonin library heatmap screen

# Loading the packages
source(here::here("R/package_loading.R"))

# Loading all pH low dataframes and binding dataframes
load(here::here("data/1C_data/df_30_32.rda"))
load(here::here("data/1C_data/df_31_33_34.rda"))
load(here::here("data/1C_data/df_35_36_37.rda"))
load(here::here("data/1C_data/df_38.rda"))
load(here::here("data/1C_data/df_39_40_41.rda"))

heatmap_all_low <- rbind(df_30_32, df_31_33_34, df_35_36_37, df_38, df_39_40_41)%>%
  mutate(type = "4.8")

#### loading all high pH dataframe and binding dataframes
load(here::here("data/1C_data/pH_df_303132.Rda"))
load(here::here("data/1C_data/pH_df_333435.Rda"))
load(here::here("data/1C_data/pH_df_363738.Rda"))
load(here::here("data/1C_data/pH_df_394041.Rda"))

heatmap_all_high <- rbind(ph_303132, ph_333435, ph_363738, ph_394041) %>%
  dplyr::filter(plasmid != "control") %>%
  mutate(type = "7.2")

# bind low and high pH dataframes
heatmap1C_all_df <- rbind(heatmap_all_low, heatmap_all_high)

save(heatmap1C_all_df, file = ("data/1C_heatmap_df.Rda"))





