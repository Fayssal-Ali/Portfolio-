# Pre-processing the dataset to create 4 individual datasets:
# 1. Metadata
# 2. Amino Acids
# 3. Lipids
# 4. Microbes

library(readxl)
#setwd
setwd("C:/Users/Edwar/Box/MGH_Vibha_internal/")
metadata <- read_xlsx("data/Processed/metadata.xlsx")

# Here, we will pre-process the data to get it
# ready to be analyzed by Tweedieverse and other omics analysis
# software tools which expect data to be vertically aligned
# with sample IDs as row names and features (metadata and microbe count)
# as columns,

# Read in the dataset of counts, rotate to have cols as genuses
# and makes genus names the names of the columns. Then, convert to
# data.frame format
microbe_counts <- read_xlsx("data/Processed/All_Microbe_ASV_Grouped_by_Genus.xlsx")
microbe_counts <- t(microbe_counts)
colnames(microbe_counts) <- microbe_counts[1, ]
microbe_counts <- microbe_counts[-1, ]
microbe_counts <- as.data.frame(microbe_counts)

# Now we modify the metadata so that the row names are the Sample IDs
# and the rest of the data we leave out

# We remove rows without a Microbiome_ID
metadata <- as.data.frame(metadata[!is.na(metadata$Microbiome_ID), ])

# get rid of Microbiome_ID and set it as row names
rownames(metadata) <- metadata$Microbiome_ID
metadata <- metadata[ , -7]

# sorting to common rows between microbe_counts and metadata
common_rows <- intersect(rownames(metadata), rownames(microbe_counts))

# subset each data frame to include only the common row names
metadata_common <- metadata[common_rows, ]
microbe_counts_common <- microbe_counts[common_rows, ]

# now the two of them are in the same order, and we make the microbe
# counts numeric rather than character
microbe_counts_reordered <- microbe_counts_common[rownames(metadata_common), ]
microbe_counts_reordered_num <- sapply(microbe_counts_reordered, as.numeric)

# Combining all of the data into one big dataframe for further analyses
mega_df <- cbind(metadata_common, microbe_counts_reordered_num)

# All of our individual data frames, in the same order of samples
clinical_df <- mega_df[ , c(1:61)]
amino_df <- mega_df[ , c(65:314)]
lipid_df <- mega_df[ , c(318:604)]
microbe_df <- mega_df[ , c(605:730)]


## A high level overview of the data
# Here, we compute some overall statistics about the distribution of our data

library(readxl)

metadata <- read_xlsx("data/Processed/metadata.xlsx")

# presenting some data about the experimental dataset
# 1) age
number_participants <- length(unique(metadata$Initials))
age_min <- round(min(metadata$`Age at Visit`, na.rm = TRUE), 2)
age_max <- round(max(metadata$`Age at Visit`, na.rm = TRUE), 2)
print(paste("There were", number_participants, "participants and their ages ranged from", age_min,"to",age_max))

# 2) bmi
metadata_start <- metadata[metadata$Visit == 0, ]
bmi_min <- round(min(metadata_start$BMI, na.rm = TRUE), 2)
bmi_max <- round(max(metadata_start$BMI, na.rm = TRUE), 2)
print(paste("At the start of the trial, their BMIs ranged from", bmi_min,"to",bmi_max))

metadata_end <- metadata[metadata$Visit == 24, ]
bmi_min <- round(min(metadata_end$BMI, na.rm = TRUE), 2)
bmi_max <- round(max(metadata_end$BMI, na.rm = TRUE), 2)
print(paste("At the end of the trial, their BMIs ranged from", bmi_min,"to",bmi_max))

# 3) group status
library(dplyr)
g0data <- metadata %>% filter(`Group Status` == "Control")
g1data <- metadata %>% filter(`Group Status` == "Sleeve")
g2data <- metadata %>% filter(`Group Status` == "RYGB")
print(paste("There were", length(unique(g0data$Initials)), "members of the Control group,", 
            length(unique(g1data$Initials)), "members of the Sleeve group, and", length(unique(g2data$Initials)),
            "members of the RYGB group."))

