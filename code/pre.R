# START -------------
# If all required packages are installed, start from here

# Clear the environment
rm(list = ls())

# Clear the console
cat("\014")  # This is equivalent to pressing Ctrl+L

# Clear all plots
if (!is.null(dev.list())) dev.off()

# save or not
save <- TRUE

# Load packages needed
my_packages <- c('readxl', 'phyloseq', 'ape', 'Biostrings', 'tidyverse', 'dplyr', 'pairwiseAdonis', 'reshape2', 'ggplot2', 'ggpubr', 'DESeq2', 'GGally', 'igraph', 'vegan', 'ecodist', 'agricolae', 'stats', 'writexl', 'dendextend', 'apeglm', 'pheatmap', 'GOplot','tibble', 'boot', 'chattr', 'SpiecEasi')

lapply(my_packages, library, character.only = TRUE) # load my_packages


theme_set(theme_bw()) # Set plot theme of ggplot2. It can be changed to other themes.
source('code/fun.R') # load own function(s)

# check versions of R and other essential packages
R.Version()
packageVersion('phyloseq') 
packageVersion('DESeq2') 
packageVersion('vegan') 



# ~ Load raw data ----
# prepare 5 files according to required format before proceeding:
# 1. zotu table
# 2. tax info
# 3. tree file
# 4. metadata
# 5. seq data

# The following zotu raw table is from Zhengsheng Yu (co-author), i.e., zotutab_raw.txt. This file contains sample ID used during sequencing, zotu IDs, and reads. It is not scaled/normalized. This file has been edited in excel to change the sample ID to recognized ones, and the reads were scaled/normalized using the smallest read count. The zotutab_raw.txt was obtained using codes in the Supplementary Material "Suppl_Mater_unoise3".


# after obtaining the above 5 files in hands, proceed as follows
# ~~ 1. load zotu table ----
asvtab <- read_xlsx("data/asv_tab_unra.xlsx")
# asvtab_f_tax <- read_xlsx("data/table_filtered_w_tax.xlsx")

see_if_norm <- asvtab %>% 
  summarise(across(W001:Ma104, ~ sum(.x, na.rm = TRUE)))
see_if_norm # not normalized 
min(see_if_norm); max(see_if_norm) # check the smallest read count
rm(see_if_norm) # remove the temp file 'see_if_norm'

# see_if_norm <- asvtab_f_tax %>% 
#   summarise(across(W001:Ma104, ~ sum(.x, na.rm = TRUE)))
# see_if_norm # not normalized 
# min(see_if_norm); max(see_if_norm) # check the smallest read count
# rm(see_if_norm) # remove 



# ~~ 2. load tax info ----
tax <- read_xlsx('data/tax.xlsx') 

# ~~ 3. load tree file ----
tree <- read.tree('data/rooted_tree.tre') # ape package is needed

# ~~ 4. load metadata ----
metadata <- read_xlsx('data/metadata.xlsx')

# ~~ 5. load seqs data ----
# 'dna-seq.fasta' is extracted from 'zotu_rep-seqs.qza') # obtained using qiime2
# The format is like >Zotu1
# ATTGGACAATGGGCGCAAGCCTGATCCAGCCATGCCG....
# There are xxxx zotus in total
# seq <- readDNAStringSet('data/dna-seq.fasta', format="fasta",
#                         nrec=-1L, skip=0L, seek.first.rec=FALSE, use.names=TRUE)

# ~~ Prepare zotu table matrix ----
# should be matrix, rownames are zotu IDs, colnames are sample IDs
# it is required if need to use phyloseq package.
# convert to matrix
# get the column zotu_ID, then assign as row names
asvtab <- column_to_rownames(asvtab, var = "ASV_ID")

# convert data frame to matrix
asvtab_mat <- data.matrix(asvtab)

rm(asvtab)

# ~~ Prepare tax file matrix ----
# dealing with tax info
# get the column zotu_ID, then assign as row names
tax2 <- column_to_rownames(tax, var = "ASV_ID")
# convert data frame to matrix
# use as.matrix rather than data.matrix
# otherwise, text will be deleted
tax_mat <- as.matrix(tax2) 
rm(tax2) # remove temp file 'tax2'
rm(tax) # remove temp file 'tax'


# ~~ Prepare metadata dataframe ----
metadata <- as.data.frame(metadata)

metadata_df <- column_to_rownames(metadata, var = "sample_ID")

# convert things in metadata_df to factors
metadata_df$cd <- as.factor(metadata_df$cd)
metadata_df$group <- as.factor(metadata_df$group)


# set factor order. The later plotting shall follow the set order
# Assuming metadata_df is your dataframe
metadata_df$cd <- factor(metadata_df$cd, levels = c("0", "10", "100"))

metadata_df$group <- factor(metadata_df$group, levels = c("Ctrl", "Nutrient", "Medick", "Tomato", "Maize"))

# Check the levels of the different factors
levels(metadata_df$cd)
levels(metadata_df$group)

rm(metadata) # remove temp file


# ~~ Prepare tree and seqs data ----
# no need to prepare(?)


# CHECK POINT 1 ---------------------------------------------------------------
# 1. zotu_mat - has otu id and sample id -- OK ******** it should be a matrix
head(asvtab_mat)
class(asvtab_mat)


# 2. tax_mat - has otu id and taxon info   ******** it should be a matrix
head(tax_mat)
class(tax_mat)

# 3. metadata - has sample id and treatments -- OK ******** it should be a data.frame
head(metadata_df)
class(metadata_df)


# 4. tree - has otu id -- OK(?) how to check(?)
taxa_names(tree)
class(tree)

# 5. seq - has DNA sequences and otu id -- OK(?) how to check(?)
# taxa_names(seq)
# class(seq)

### the above 5 items can be combined as a phylo object for further analysis


# IMPORTANT: telling how to combine the 5 items #### 
ASV <- otu_table(asvtab_mat, taxa_are_rows = TRUE) # IMPORTANT step ***
rm(asvtab_mat)


TAX <- tax_table(tax_mat) # IMPORTANT step ***
rm(tax_mat)

metadata <- sample_data(metadata_df) # IMPORTANT step ***
rm(metadata_df) # remove temp file

# TREE <- phy_tree(tree) # actually no need, seems 'tree' =? 'TREE'

# using the following to check type

class(ASV)
class(TAX)
class(metadata)
class(tree)
# class(seq)

# Now in the 'Global Environment' panel, there should be 5 objects: ZOTU, TAX, metadata, tree, seq

# Combine to construct phyloseq object ----------------------------------------
pl_unra <- phyloseq(ASV, TAX, metadata, tree) # no seq data also can construct the phyloseq object

# CHECK POINT 2 ---------------------------------------------------------------
# inspect different names and variables

pl_unra
sample_names(pl_unra)
rank_names(pl_unra)
sample_variables(pl_unra)
taxa_names(pl_unra)
# refseq(pl_unra)


# FINISHED constructing phyloseq object ---------------------------------------
