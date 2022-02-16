# Description: explore issues with cBioPortal files for PANC cohort
# Author: Haley Hunter-Zinck
# Date: 2022-02-14

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_file_der <- "syn22296816"
synid_file_survival <- "syn26349089"

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
#' @param comment.char character designating comment lines to ignore
#' @return data frame
get_synapse_entity_data_in_csv <- function(synapse_id, 
                                           version = NA,
                                           sep = ",", 
                                           na.strings = c("NA"), 
                                           header = T,
                                           check_names = F,
                                           comment.char = "#") {
  
  if (is.na(version)) {
    entity <- synGet(synapse_id)
  } else {
    entity <- synGet(synapse_id, version = version)
  }
  
  data <- read.csv(entity$path, stringsAsFactors = F, 
                   na.strings = na.strings, sep = sep, check.names = check_names,
                   header = header, comment.char = comment.char)
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

der <- get_synapse_entity_data_in_csv(synid_file_der)
survival <- get_synapse_entity_data_in_csv(synid_file_survival, sep = "\t")

# main ----------------------------

setdiff(tolower(colnames(survival)), colnames(der))

pt <- unlist(survival %>% 
  select(PATIENT_ID) %>%
  distinct())
n_pt <- length(pt)

fil <- unlist(survival %>% 
  filter(!is.na(PFS_I_ADV_STATUS) & PFS_I_ADV_STATUS != "") %>%
  select(PATIENT_ID) %>%
  distinct())
n_fil = length(fil)

discrep <- setdiff(pt, fil)
discrep_preview <- survival %>% 
  filter(is.element(PATIENT_ID, discrep)) 

fil2 <- unlist(survival %>% 
                 filter(!is.na(PFS_I_ADV_STATUS) & PFS_I_ADV_STATUS != "" | !duplicated(PATIENT_ID)) %>%
                 select(PATIENT_ID) %>%
                 distinct())

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
