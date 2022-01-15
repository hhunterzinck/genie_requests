# Description: Create TMB estimates and plot by site for 100k GENIE data.  
# Author: Haley Hunter-Zinck
# Date: 2022-01-14

# pre-setup  ---------------------------

library(optparse)

waitifnot <- function(cond, msg) {
  if (!cond) {
    
    for (str in msg) {
      message(str)
    }
    message("Press control-C to exit and try again.")
    
    while(T) {}
  }
}

# user input ----------------------------

option_list <- list( 
  make_option(c("-i", "--synid_file_input"), type = "character",
              help="Synapse ID of input file"),
  make_option(c("-o", "--synid_folder_output"), type = "character",
              help="Synapse ID of output folder"),
  make_option(c("-v", "--verbose"), action="store_true", default = FALSE, 
              help="Output script messages to the user.")
)
opt <- parse_args(OptionParser(option_list=option_list))
waitifnot(!is.null(opt$synid_file_input) && !is.null(opt$synid_folder_output),
          msg = "Rscript template.R -h")

synid_file_input <- opt$synid_file_input
synid_folder_output <- opt$synid_folder_output
verbose <- opt$verbose

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(RColorBrewer)
library(synapser)
synLogin()

# synapse
synid_file_sample <- "syn7517674"

# files
outplot <- "tmb.pdf"

# functions ----------------------------

#' Download and load data stored in csv or other delimited format on Synapse
#' into an R data frame.
#' 
#' @param synapse_id Synapse ID
#' @version Version of the Synapse entity to download.  NA will load current
#' version
#' @param set Delimiter for file
#' @param na.strings Vector of strings to be read in as NA values
#' @param header TRUE if the file contains a header row; FALSE otherwise.
#' @param check_names TRUE if column names should be modified for compatibility 
#' with R upon reading; FALSE otherwise.
#' @return data frame
get_synapse_entity_data_in_csv <- function(synapse_id, 
                                           version = NA,
                                           sep = ",", 
                                           na.strings = c("NA"), 
                                           header = T,
                                           check_names = F) {
  
  if (is.na(version)) {
    entity <- synGet(synapse_id)
  } else {
    entity <- synGet(synapse_id, version = version)
  }
  
  data <- read.csv(entity$path, stringsAsFactors = F, 
                   na.strings = na.strings, sep = sep, check.names = check_names,
                   header = header)
  return(data)
}

#' Store a file on Synapse with options to define provenance.
#' 
#' @param path Path to the file on the local machine.
#' @param parent_id Synapse ID of the folder or project to which to load the file.
#' @param file_name Name of the Synapse entity once loaded
#' @param prov_name Provenance short description title
#' @param prov_desc Provenance long description
#' @param prov_used Vector of Synapse IDs of data used to create the current
#' file to be loaded.
#' @param prov_exec String representing URL to script used to create the file.
#' @return Synapse ID of entity representing file
save_to_synapse <- function(path, 
                            parent_id, 
                            file_name = NA, 
                            prov_name = NA, 
                            prov_desc = NA, 
                            prov_used = NA, 
                            prov_exec = NA) {
  
  if (is.na(file_name)) {
    file_name = path
  } 
  file <- File(path = path, parentId = parent_id, name = file_name)
  
  if (!is.na(prov_name) || !is.na(prov_desc) || !is.na(prov_used) || !is.na(prov_exec)) {
    act <- Activity(name = prov_name,
                    description = prov_desc,
                    used = prov_used,
                    executed = prov_exec)
    file <- synStore(file, activity = act)
  } else {
    file <- synStore(file)
  }
  
  return(file$properties$id)
}

# read ----------------------------

data <- get_synapse_entity_data_in_csv(synid_file_input, sep = "\t")

query <- glue("SELECT SAMPLE_ID, ONCOTREE_CODE FROM {synid_file_sample}")
sam <- as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = T))

# main ----------------------------

mut <- data %>% 
  mutate(center = unlist(lapply(strsplit(SAMPLE_ID, split = "-"), function(x) {return(x[[2]])}))) %>%
  left_join(sam, by = "SAMPLE_ID")

u_bin <- unlist(data %>% select(tmb_bin) %>% distinct())
agg_tab <- aggregate(factor(tmb_bin, levels = u_bin) ~ center, data = mut, FUN = table)
agg_med <- aggregate(tmb ~ center, data = mut, FUN = median)
agg_n <- aggregate(tmb ~ center, data = mut, FUN = length)

agg_med_cancer <- aggregate(tmb ~ ONCOTREE_CODE, data = mut, FUN = median)
agg_n_cancer <- aggregate(tmb ~ ONCOTREE_CODE, data = mut, FUN = length)
agg_tab_cancer <- aggregate(factor(tmb_bin, levels = u_bin) ~ ONCOTREE_CODE, data = mut, FUN = table)

idx <- which(agg_n_cancer[[2]] > 1000)
agg_tab_cancer_subset <- list(agg_tab_cancer[[1]][idx], agg_tab_cancer[[2]][idx,])

# plot -------------------

pdf(outplot)

# high/med/low by center
#colors = setNames(c("gray80", "gray50", "gray20"), colnames(agg_tab[[2]]))
colors = setNames(brewer.pal(n = ncol(agg_tab[[2]]), name = "Dark2"), colnames(agg_tab[[2]]))
barplot(t(agg_tab[[2]] / rowSums(agg_tab[[2]])), horiz = T, names = agg_tab[[1]], las = 1, col = colors[colnames(agg_tab[[2]])],
        xlab = "Fraction of samples in TMB bin", main = "Sample TMB bin by center")
legend(x = "topright", legend = colnames(agg_tab[[2]]), pch = 15, col = colors)

# compare sample number and fraction of high/med/low
frac_bin <- agg_tab[[2]] / rowSums(agg_tab[[2]])
n_sample <- rowSums(agg_tab[[2]])
plot(n_sample, frac_bin[,1], col = colors[colnames(frac_bin)[1]], xlab = "Number of samples per center",
     ylab = "Fraction of samples in bin", main = "Fraction of samples in bin by number of samples")
for (i in 2:ncol(frac_bin)) {
  points(n_sample, frac_bin[,i], col = colors[colnames(frac_bin)[i]])
}

# get median tmb by sample count
plot(agg_n[[2]], log10(agg_med[[2]]), xlab = "Number of samples per center", ylab = "Median TMB", 
     main = "Median TMB per center", pch = 16)

# tmb by oncotree code
plot(agg_n_cancer[[2]], log10(agg_med_cancer[[2]]), xlab = "Number of samples per cancer", 
     ylab = "Log10(TMB)", main = "TMB by cancer type")

colors = setNames(brewer.pal(n = ncol(agg_tab_cancer_subset[[2]]), name = "Set3"), colnames(agg_tab_cancer_subset[[2]]))
barplot(t(agg_tab_cancer_subset[[2]] / rowSums(agg_tab_cancer_subset[[2]])), horiz = T, names = agg_tab_cancer_subset[[1]], las = 1, col = colors[colnames(agg_tab[[2]])],
        xlab = "Fraction of samples in TMB bin", main = "Sample TMB bin by OncoTree code")
legend(x = "topleft", legend = colnames(agg_tab_cancer_subset[[2]]), pch = 15, col = colors)

graphics.off()

# write -------------------------------

synid_file_output <- save_to_synapse(path = outplot, 
                parent_id = synid_folder_output,
                prov_name = "tmb plots", 
                prov_desc = "plot tmb for genie release 9.1-public by center and oncotree code", 
                prov_used = synid_file_input, 
                prov_exec = "https://github.com/hhunterzinck/genie_requests/blob/main/2022-01-14_100k_tmb_by_site.R")

file.remove(outplot)

# close out ----------------------------

print(glue("Plots loaded to {synid_file_output}."))

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
