
#Data was previously filtered for transcripts (see list below) from Human Protein atlas transcript_rna_tissue.tsv (available on proteinatlas.com, version 20.1)

source(here::here("R/package_loading.R"))
load(here::here("data/1A_dataframe.rda"))

#filter for tissue only
data_target_3 <- data_target_2 %>% select(-contains("cell"))
data_target_3 <- data_target_3 %>% select(-contains("neutrophil"))
data_target_3 <- data_target_3 %>% select(-contains("monocyte"))
data_target_3 <- data_target_3 %>% select(-contains("myeloid DC"))
data_target_3 <- data_target_3 %>% select(-contains("T-reg"))
data_target_3 <- data_target_3 %>% select(-contains("gdTCR"))
data_target_3 <- data_target_3 %>% select(-contains("plasmacytoid DC"))
data_target_3 <- data_target_3 %>% select(-contains("total PBMC"))
data_target_3 <- data_target_3 %>% select(-contains("basophil"))
data_target_3 <- data_target_3 %>% select(-contains("eosinophil"))

#change enstid to 5HT name
data_target_3$enstid <- gsub('ENST00000323865', '5HT1A', data_target_3$enstid)
data_target_3$enstid <- gsub('ENST00000369947', '5HT1B', data_target_3$enstid)
data_target_3$enstid <- gsub('ENST00000374619', '5HT1D', data_target_3$enstid)
data_target_3$enstid <- gsub('ENST00000305344', '5HT1E', data_target_3$enstid)
data_target_3$enstid <- gsub('ENST00000319595', '5HT1F', data_target_3$enstid)
data_target_3$enstid <- gsub('ENST00000378688', '5HT2A', data_target_3$enstid)
data_target_3$enstid <- gsub('ENST00000258400', '5HT2B', data_target_3$enstid)
data_target_3$enstid <- gsub('ENST00000276198', '5HT2C', data_target_3$enstid)
data_target_3$enstid <- gsub('ENST00000377888', '5HT4', data_target_3$enstid)
data_target_3$enstid <- gsub('ENST00000287907', '5HT5A', data_target_3$enstid)
data_target_3$enstid <- gsub('ENST00000289753', '5HT6', data_target_3$enstid)
data_target_3$enstid <- gsub('ENST00000336152', '5HT7', data_target_3$enstid)

#spread data
data_wide <- data_target_3[-1]
row.names(data_wide) <- data_target_3$enstid

#get rid of the patient ID suffix on tissue names
colnames(data_wide) <- gsub('\\..*','',colnames(data_wide))

#take mean of tissues
formatted<-t(apply(data_wide, 1, function(x) tapply(x, colnames(data_wide), mean)))

#Cleaning up column names
colnames(formatted)[colnames(formatted) == 'adipose tissue'] <- 'Adipose tissue'
colnames(formatted)[colnames(formatted) == "adrenal gland"] <- "Adrenal gland"
colnames(formatted)[colnames(formatted) == "appendix"] <- "Appendix"
colnames(formatted)[colnames(formatted) == "bone marrow"] <- "Bone marrow"
colnames(formatted)[colnames(formatted) == "breast"] <- "Breast"
colnames(formatted)[colnames(formatted) == "cerebral cortex"] <- "Cerebral cortex"
colnames(formatted)[colnames(formatted) == "cervix, uterine"] <- "Cervix, uterine"
colnames(formatted)[colnames(formatted) == "colon"] <- "Colon"
colnames(formatted)[colnames(formatted) == "duodenum"] <- "Duodenum"
colnames(formatted)[colnames(formatted) == "endometrium 1"] <- "Endometrium"
colnames(formatted)[colnames(formatted) == "epididymis"] <- "Epididymis"
colnames(formatted)[colnames(formatted) == "esophagus"] <- "Esophagus"
colnames(formatted)[colnames(formatted) == "fallopian tube"] <- "Fallopian tube"
colnames(formatted)[colnames(formatted) == "gallbladder"] <- "Gallbladder"
colnames(formatted)[colnames(formatted) == "heart muscle"] <- "Heart muscle"
colnames(formatted)[colnames(formatted) == "kidney"] <- "Kidney"
colnames(formatted)[colnames(formatted) == "liver"] <- "Liver"
colnames(formatted)[colnames(formatted) == "lung"] <- "Lung"
colnames(formatted)[colnames(formatted) == "lymph node"] <- "Lymph node"
colnames(formatted)[colnames(formatted) == "ovary"] <- "Ovary"
colnames(formatted)[colnames(formatted) == "pancreas"] <- "Pancreas"
colnames(formatted)[colnames(formatted) == "parathyroid gland"] <- "Parathyroid gland"
colnames(formatted)[colnames(formatted) == "placenta"] <- "Placenta"
colnames(formatted)[colnames(formatted) == "prostate"] <- "Prostate"
colnames(formatted)[colnames(formatted) == "rectum"] <- "Rectum"
colnames(formatted)[colnames(formatted) == "salivary gland"] <- "Salivary gland"
colnames(formatted)[colnames(formatted) == "seminal vesicle"] <- "Seminal vesicle"
colnames(formatted)[colnames(formatted) == "skeletal muscle"] <- "Skeletal muscle"
colnames(formatted)[colnames(formatted) == "skin 1"] <- "Skin"
colnames(formatted)[colnames(formatted) == "small intestine"] <- "Small intestine"
colnames(formatted)[colnames(formatted) == "smooth muscle"] <- "Smooth muscle"
colnames(formatted)[colnames(formatted) == "spleen"] <- "Spleen"
colnames(formatted)[colnames(formatted) == "stomach 1"] <- "Stomach"
colnames(formatted)[colnames(formatted) == "testis"] <- "Testis"
colnames(formatted)[colnames(formatted) == "thyroid gland"] <- "Thyroid gland"
colnames(formatted)[colnames(formatted) == "tonsil"] <- "Tonsil"
colnames(formatted)[colnames(formatted) == "urinary bladder"] <- "Urinary bladder"

#Reorder the columns by organ type
col_order <- c("Pancreas", "Lymph node", "Bone marrow", "Tonsil", "Spleen", "Appendix", "Skeletal muscle", "Smooth muscle", "Heart muscle", "Liver", "Gallbladder", "Placenta", "Ovary", "Fallopian tube", "Endometrium", "Cervix, uterine", "Breast", "Parathyroid gland", "Adrenal gland", "Thyroid gland", "Cerebral cortex", "Lung", "Urinary bladder", "Kidney", "Prostate", "Seminal vesicle", "Epididymis", "Testis", "Adipose tissue", "Skin", "Salivary gland", "Esophagus", "Rectum", "Small intestine", "Colon", "Duodenum", "Stomach")
formatted_order <- formatted[, col_order]

#Rotate plot
rot_formatted_order<- t(formatted_order)

#pheatmap
my.breaks <- c(seq(0, 6, by=.1))
my.colors <- c(colorRampPalette(colors = c("white","blue"))(length(my.breaks)))
map<-pheatmap(formatted_order, scale = "row",cluster_rows = T, cluster_cols=F, color=my.colors, breaks=my.breaks, main = "RNA expression in human tissue")
map
