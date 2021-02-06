install.packages("dpylr")
library(dplyr)
library(tidyverse)
library(tidyr)
library(tibble)
library(kohonen)
library(SAICE)
library(ggplot2)
library(Hmisc)
library(kohonen)
library(dummies)
library(rgdal)
library(sp)
library(maptools)
library(reshape2)
library(rgeos)
library(rgdal)

# Set up credentials. You should only need to do this once!
## setup_sso(reset_sso = TRUE)
# Set up connection. You need to do this once per session
SAICE::initialize_connection(entimice_env = "PROD")
# read in your data. Just as with read_bce() you can read a full
#path or just a file. You will have to replace this path
# with one you have access to! note that the path is case sensitive


read_entimice_delay <- function (source_path, recurse = FALSE, filename_regex = NULL,
                                 encoding = NULL, read_entimice_call = TRUE)
{
  function_return <- list(exit_status = 0, summary = NULL,
                          details = NULL, data = NULL)
  function_return$summary <- "Error in function configuration (read_entimice)"
  if (is.null(getOption("SAICE"))) {
    function_return$details <- "Please make sure the connection to entimICE is initialized and please execute function \"initialize_connection()\"!!!"
    function_return$exit_status <- 1
    cat(crayon::red(paste0("\n", function_return$summary,
                           "\n", function_return$details, "\n")))
    return(function_return)
  }
  config <- getOption("SAICE")
  file_type <- "sas7bdat"
  
  function_return <- get_entimice_delay(source_path, file_type,
                                        recurse = recurse, filename_regex = filename_regex, read_entimice_call = read_entimice_call)
  
  if (function_return$exit_status != 0)
    return(function_return)
  cat("Converting sas7bdat file to tibble...\n")
  lfiles <- function_return$data
  dataset <- lapply(seq_along(lfiles), function(idx) {
    tryCatch({
      haven::read_sas(lfiles[idx][[1]], encoding = encoding)
    }, error = function(e) {
      file_name <- gsub("^\\s+|\\s+$", "", names(lfiles[idx]))
      function_return$exit_status <<- 1
      function_return$summary <<- "Error during converting files to tibble"
      if (is.null(function_return$details)) {
        function_return$details <<- paste0(" Error to convert file \"",
                                           file_name, "\": ", e)
      }
      else {
        function_return$details <<- paste0(" ", function_return$details,
                                           " Error to convert file \"", file_name, "\":",
                                           e)
      }
      NULL
    })
  })
  if (length(dataset) == 1) {
    dataset <- dataset[[1]]
  }
  else {
    if (recurse == FALSE) {
      names(dataset) <- sub(pattern = paste0("\\.", file_type,
                                             "$"), replacement = "", x = basename(names(lfiles)))
    }
    else {
      names(dataset) <- names(lfiles)
    }
  }
  if (function_return$exit_status == 1) {
    cat(crayon::red(paste0("\n", function_return$summary,
                           "\n", function_return$details, "\n")))
  }
  function_return$data <- dataset
  if (config$mini_output)
    function_return <- function_return$data
  cat("Reading data is finished.\n")
  return(function_return)
}



get_entimice_delay <- function (source_path = NULL, file_type = NULL, recurse = FALSE,
                                filename_regex = NULL, output_source_path = TRUE, read_entimice_call = FALSE)
{
  function_return <- list(exit_status = 0, summary = NULL,
                          details = NULL, data = NULL)
  function_return$summary <- "Error in function configuration (get_entimice)"
  if (is.null(file_type)) {
    file_type <- ""
  }
  else {
    file_type <- gsub("^\\s+|\\s+$", "", file_type)
    if (file_type == "") {
      function_return$details <- "Type of file should not be empty!!!"
      function_return$exit_status <- 1
      cat(crayon::red(paste0("\n", function_return$summary,
                             "\n", function_return$details, "\n")))
      return(function_return)
    }
  }
  if (is.null(source_path)) {
    function_return$details <- "Please input a proper path for entimICE!!!"
    function_return$exit_status <- 1
    cat(crayon::red(paste0("\n", function_return$summary,
                           "\n", function_return$details, "\n")))
    return(function_return)
  }
  source_path <- gsub("^\\s+|\\s+$", "", source_path)
  if (source_path == "") {
    function_return$details <- "Source data file path should not be empty!!"
    function_return$exit_status <- 1
    cat(crayon::red(paste0("\n", function_return$summary,
                           "\n", function_return$details, "\n")))
    return(function_return)
  }
  if (is.null(filename_regex)) {
    filename_regex <- ""
  }
  if (is.null(getOption("SAICE"))) {
    function_return$details <- "Please make sure the connection to entimICE is initialized and please execute function \"initialize_connection()\"!!!"
    function_return$exit_status <- 1
    cat(crayon::red(paste0("\n", function_return$summary,
                           "\n", function_return$details, "\n")))
    return(function_return)
  }
  config <- getOption("SAICE")
  target_path <- paste0(Sys.getenv("HOME"), "/")
  action <- "get_entimice"
  if (read_entimice_call)
    action <- "read_entimice"
  request_headers <- c(para1 = config$user_name, para2 = config$credentials[1],
                       para3 = config$credentials[2], para4 = config$app_session_ID,
                       file_type = file_type, filename_regex = filename_regex,
                       source_path = source_path, target_server = config$target_server,
                       target_path = target_path, action = action, recurse = recurse,
                       version = "0.3.0", working_env = config$working_env)
  Sys.sleep(0.1)
  function_return <- SAICE:::request_to_knife(request_headers, config$knife_server,
                                              config$request_port)
  Sys.sleep(0.1)
  if (function_return$exit_status != 0)
    return(function_return)
  prefix <- function_return$data[1]
  target_path <- function_return$data[2]
  sync_headers <- c(para1 = config$user_name, para4 = config$app_session_ID,
                    para5 = prefix, action = action)
  if (config$connection_type == "pass")
    sync_headers["para2"] <- config$credentials[1]
  cat("Reading signal has already been sent...\n")
  cat("Caching remote file...\n")
  Sys.sleep(0.1)
  function_return <- SAICE:::request_sync(sync_headers, config$knife_server,
                                          config$sync_port, source_path)
  
  
  if (function_return$exit_status != 0)
    return(function_return)
  Sys.sleep(5)
  tmp_function_return <- decrypt_files(target_path, prefix,
                                       source_path, config)
  if (tmp_function_return$exit_status == 0) {
    filepath_str <- tmp_function_return$data
    path_to_return <- filepath_str$local_path
    if (output_source_path == TRUE) {
      names(path_to_return) <- filepath_str$file_path
    }
    function_return$data <- path_to_return
  }
  else {
    function_return <- tmp_function_return
  }
  if (!read_entimice_call && config$mini_output)
    function_return <- function_return$data
  return(function_return)
}

decrypt_files <- function (target_path, prefix, source_path, config)
{
  function_return <- list(exit_status = 0, summary = NULL,
                          details = NULL, data = NULL)
  path <- target_path
  cat("Preparing the cached data for reading...\n")
  request_headers = c(para1 = config$user_name, para4 = config$app_session_ID,
                      action = "retrieve_key")
  function_return <- SAICE:::request_to_knife(request_headers, config$knife_server,
                                              config$decrypt_port)
  if (function_return$exit_status != 0)
    return(function_return)
  cmd_decrypt_pass = "openssl enc -base64 -d -aes-256-cbc -md md5 -k"
  passphrase <- system(paste(paste(cmd_decrypt_pass, config$app_session_ID,
                                   "<<< \""), function_return$data, "\"", sep = ""), intern = TRUE)
  lfiles <- list.files(path, paste("[^\\/]*", prefix, "_[^\\/]+",
                                   sep = ""), all.files = TRUE, full.names = TRUE, recursive = TRUE)
  lfiles <- lfiles[!file.info(lfiles)$isdir]
  file_source_path <- c()
  temporary_path <- c()
  for (file_path in lfiles) {
    tmp_str <- tempfile()
    file_path_str <- paste("\"", file_path, "\"", sep = "")
    cmd_encrypt_data <- paste("openssl enc -d -aes-256-cbc -md md5 -k",
                              passphrase, "-in", file_path_str, "| gzip -d >",
                              tmp_str, sep = " ")
    system(cmd_encrypt_data)
    file_path <- gsub("\\._\\.", "/", file_path)
    file_path <- gsub("//", "/", file_path)
    file_source_path <- c(file_source_path, sub(".gz$", "",
                                                sub(paste(prefix, "_", sep = ""), "", sub(paste(target_path,
                                                                                                prefix, "/", sep = ""), "", file_path))))
    temporary_path <- c(temporary_path, tmp_str)
  }
  request_headers = c(para1 = config$user_name, para2 = config$credentials[1],
                      para3 = config$credentials[2], para4 = config$app_session_ID,
                      target_server = config$target_server, target_path = target_path,
                      action = "remove")
  function_return <- SAICE:::request_to_knife(request_headers, config$knife_server,
                                              config$request_port)
  if (function_return$exit_status != 0)
    return(function_return)
  cat("Preparation is done!!!\n")
  file_path_list <- data.frame(file_path = file_source_path,
                               local_path = temporary_path, stringsAsFactors = FALSE)
  function_return$data <- file_path_list
  if (function_return$exit_status != 0) {
    cat(crayon::red(paste0("\n", function_return$summary,
                           "\n", function_return$details, "\n")))
  }
  return(function_return)
}


one_file <- read_entimice_delay("root/clinical_studies/RO7017773/CDPT7816/BP40331/data_analysis/CSRInterim_Wk2/data/sdtmv")
all_files <- read_entimice_delay("root/clinical_studies/RO7017773/CDPT7816/BP40331/data_analysis/CSRInterim_Wk2/data/sdtmv/")

adsl <- read_entimice_delay("root/clinical_studies/RO7017773/CDPT7816/BP40331/data_analysis/CSRInterim_Wk2/qa/outdata_vad/adsl.sas7bdat")
# Might not need these - Ask Dietmar !!# 
adqs <- read_entimice_delay("root/clinical_studies/RO7017773/CDPT7816/BP40331/data_analysis/CSRInterim_Wk2/qa/outdata_vad/adqs.sas7bdat")
adsub <- read_entimice_delay("root/clinical_studies/RO7017773/CDPT7816/BP40331/data_analysis/CSRInterim_Wk2/qa/outdata_vad/adsub.sas7bdat")
dm <- read_entimice_delay("root/clinical_studies/RO7017773/CDPT7816/BP40331/data_analysis/CSRInterim_Wk2/data/sdtmv/dm.sas7bdat")

df_new <- adqs %>% filter(VISIT == "Baseline") %>% select(USUBJID, PARAM, AVAL) %>% spread(PARAM, AVAL) %>% column_to_rownames("USUBJID")


df_new1 = df_new[!duplicated(df_new[,1:2]), ]
df_new2 = df_new1 %>% spread(PARAM,AVAL) %>% dplyr::select(-USUBJID)


# strip missings (0.30 is a good compromise)
df_new = df_new[, -which(colMeans(is.na(df_new)) > 0.30)]  
df_new = df_new[complete.cases(df_new), ]

# Z-transform scales (edit sumofsqaure)
df_n1_tr = as.matrix(df_new, ncol = length(adqs))
df_n1_tr = scale(df_n1_tr, center = TRUE, scale = TRUE) # z-transform scales

som_data = t(df_n1_tr) #transposing the data

#set.seed(123)
xdim = 4
ydim = 3

som_scales = som(som_data, grid = somgrid(xdim = xdim, ydim = ydim, topo = "hexagonal", neighbourhood.fct = "gaussian", toroidal = FALSE),
                 rlen=5000, alpha = c(0.2,0.01), dist.fcts = "sumofsquares")

#som_scales = som(som_data, grid = somgrid(xdim = xdim, ydim = ydim, topo = "hexagonal", neighbourhood.fct = "gaussian", toroidal = FALSE),
# rlen=5000, alpha = c(0.2,0.01), dist.fcts = "sinesq")

labels = vector()

for (i in seq(1:(xdim*ydim))) {labels[i] = paste(colnames(df_n1_tr)[which(som_scales$unit.classif == i)], collapse ="\n")}

som_pts = tibble(x = som_scales$grid$pts[,1],
                 y = som_scales$grid$pts[,2],
                 lab = labels) %>% 
  mutate(vector=paste0("V",row_number()))



library(raster)


distfun<-function(data){
  outdist<-data.frame(Distance=as.numeric(), x1=as.numeric(), y1=as.numeric(),x2=as.numeric(), y2=as.numeric(), coord1=as.character(), coord2=as.character())
  n<-nrow(data)
  for (i in 1:(n-1)){
    outdist1<-data.frame(Distance=as.numeric(), x1=as.numeric(), y1=as.numeric(), x2=as.numeric(), y2=as.numeric(), coord1=as.character(), coord2=as.character())
    
    for (j in (i+1):n) {
      outdist2<-data.frame(Distance=raster::pointDistance(c(data[i,]$x, data[i,]$y),c(data[j,]$x, data[j,]$y),lonlat=FALSE),
                           x1=data[i,]$x, y1=data[i,]$y, x2=data[j,]$x, y2=data[j,]$y, coord1=data[i,]$vector,coord2=data[j,]$vector)
      outdist1<-rbind(outdist1,outdist2)
      
    }
    outdist<-rbind(outdist,outdist1)
  }
  return(outdist)
  
}

raster::pointDistance(c(2, 3),c(4, 5),lonlat=FALSE)

Coordist <- distfun(som_pts)

newdata <- Coordist[ which(Coordist$Distance<=1.414), ] 

distance_matrix = dist(as.data.frame(som_scales$codes), upper = FALSE)

dm = as.tibble(data.table::setDT(as.data.frame(as.matrix(distance_matrix)), keep.rownames=TRUE)) %>% 
  gather(key="rn1", value="Value", -rn) %>% 
  dplyr::select(coord1=rn, coord2=rn1, Value)

newdata$coord1<-as.character(newdata$coord1)   
newdata$coord2<-as.character(newdata$coord2)   

Coordfin<-left_join(newdata,dm, by=c("coord1", "coord2"))   

#Create a function to generate a continuous color palette
rwbPal <- colorRampPalette(c('blue','white', 'red'))


#This adds a column of color values
# based on the y values
Coordfin$Col <- rwbPal(15)[as.numeric(cut(Coordfin$Value,breaks = 15))]


#plot with nodes connected

pdf("somcolour.pdf", width=25, height=20)
ggplot()+
  geom_point(data=som_pts, aes(x = x, y = y), shape=10, size=1, color="blue")+
  geom_segment(aes(x=x1, y=y1, xend=x2, yend=y2), size=3, data=Coordfin, colour=Coordfin$Col)+
  #geom_text(data=som_pts, aes(x = x, y = y, label = lab), inherit.aes = TRUE, check_overlap = FALSE, size = 2.5, vjust = 0, nudge_y = -0.1)+
  geom_label(data=som_pts, aes(x = x, y = y, label = lab), fill="white",alpha = 0.5, label.padding=unit(0.5,"lines"), size=4, colour="black")+ #for white boxes under labels
  theme(panel.background = element_rect(fill = 'grey38'), panel.border = element_blank(), #decide which grey to use
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),)+
  expand_limits(x = c(0.5, 5), y = c(0, 3.8))

dev.off()


