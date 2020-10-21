##### *********************************************************************************************#####
##### set up#####
##clean environment
rm(list = ls())

## set the working directory were the script is located
current_path = here::here()# rstudioapi::getActiveDocumentContext()$path
root_path=current_path

##install common packages
library("devtools") #make sure you have the library
github_token=rio::import("/Users/manubetin/Dropbox/Manuel/Professionnel/github_token/github_token.txt")

#install_github("manuelbetin/SetUpProject",auth_token=github_token[[1]])
install_github("manuelbetin/TextMiningCrisis",auth_token=github_token[[1]])

packages <- c("dplyr"
              , 'tictoc'
              , "rio"
              , "purrr"
              , "tidytext"
              , "stringr"
              , "stringi"
              , "tidyr"
              , "ggplot2"
              , "lubridate"
              , 'crayon'
              , "DT"
              , "plotly"
              , "TextMiningCrisis"
              , "SetUpProject",
              "filesstrings",
              "stargazer",
              "countrycode",
              "rnaturalearth",
              "rnaturalearthdata",
              "rgeos")

## load common packages
SetUpProject::load.my.packages(packages)

# Load all packages from TextMiningCrisis:

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
######## SECTION OF INSTRUCTIONS AND PARAMETERS ##########

## Description:
## The script takes as input a dataframe containing the urls where to download the 
## documents and perform a text analysis  using a lexicon of economic crisis to 
## observe the type of crisis that the text mention

## Instructions:
## replace 1) and 2)
apply_tf_on_new_ctry=F
update_tf=F

usb_drive="/Volumes/Elements/IMF documents"
delete_pdfs=F
rm_short_docs=T
min_words=500
engine = pdf_text
##Remove comment to use all available word groups
#keyword_list=names(key_words_crisis())

##Remove comment to next line to use all available categories
keyword_list=names(lexicon())

##Manual selection

#keyword_list=c('Currency_crisis',"Balance_payment_crisis")#,'Severe_recession',"Banking_crisis")

#keyword_list=c('Currency_crisis',"Currency_crisis_severe")#,'Severe_recession',"Banking_crisis")

# keyword_list=c('Reform_agenda','Political_crisis','Balance_payment_crisis','World_outcomes',
#                 'Contagion','Expectations','Currency_crisis',
#                 'Financial_crisis','Severe_recession','Soft_recession',
#                 'Fiscal_outcomes','Banking_crisis','Commodity_crisis','Inflation_crisis',
#                 'Sovereign_default','Fiscal_consolidation')

#***************************************************************************************####
output=list()
output[["Session_info"]]=sessionInfo()

#***************************************************************************************####

correct_url=function(link){
  #ad hoc function that to correct the links for the IMF url when url does not refer to the pdf document
  path_file=link[[1]]
  html_file <- try(read_html(path_file), silent=T)
  if("try-error" %in% class(html_file)) {
    warning(paste(path_file,": Error in path file",sep=""))
    content=NA
  }else {
    content=try({
      content<- html_file %>% html_nodes(xpath=  '//*[@id="ais-detail-container"]/div[2]/div/ul') %>%html_nodes("a") %>% html_attr("href")
      content[(str_detect(content,"pdf")|str_detect(content,"PDF"))]
      })
    if("try-error" %in% class(content)) {
      warning(paste(path_file,": Error in path file",sep=""))
      content=NA
    }else{
      content=ifelse(identical(content,character(0)),NA,content)
      return(content)
    }
    #content=data.frame(content)
  }
  
}

ctries=c('VEN','VNM','ZAF','ZMB')
          #'ZWE')

#ctries=c("USA","DEU","URY","GRC","MEX","ARG","THA","IDN")

if(apply_tf_on_new_ctry==T){
  
  #run the tf chunk by chunk, it will create a temp folder with the corpus made out of the
  #documents selected and the tf computed.
  #when documents have not a valid url it will find the correct one on the IMF website

  lapply(ctries,function(x){
  path_external_usb=paste0(usb_drive,"/",x)
  #Load the database of urls output of the script 1. consolidate_urls.R
  url_links=IMF_docs_urls %>%
    mutate(name_file=paste0(ID,"_",period,"_",type_doc_programs)) %>% filter(ID==x)
  #filter(str_detect(pdf,".pdf") | str_detect(pdf,".PDF"))

  # correct incorrect urls by rescarping the imf website to get the link to the pdf
  if(!file.exists(paste0(path_external_usb,"/updated urls/updated_urls_",x,".RData"))){
  for(i in 1:dim(url_links)[1]){
    if(!(str_detect(url_links[i,"pdf"],".pdf") | str_detect(url_links[i,"pdf"],".PDF"))){
      print(url_links[[i,"name_file"]])
      url_links[i,"pdf"]=correct_url(url_links[i,"pdf"])
    }
  }
  
  dir.create(paste0(path_external_usb,"/updated urls/"))
  rio::export(url_links,paste0(path_external_usb,"/updated urls/updated_urls_",x,".RData"))
  }else{
  url_links=rio::import(paste0(path_external_usb,"/updated urls/updated_urls_",x,".RData"))
  }
  run_tf_by_chunk(urls=url_links,
                    keyword_list=keyword_list,
                    extract_number=x,
                    delete_pdfs = delete_pdfs,
                    ENGINE = engine,
                    rm_short_docs=rm_short_docs,
                    min_words=100,loc_temp =path_external_usb)
  })
}


################################### Update existing indexes:
# When doing it, the text file with date and indexes updated is going to be overwritten. Commit it to Git with an explanation
# of the update and eventual problems that remain unsolved.


if(update_tf == T){

# List of all directories in external drive:

mytfs <- setdiff(dir(usb_drive),c("download_docs.r","urls_Requests_Reviews_articleIV.RData","0. logs","0. Old extraction","tf_idf.RData"))
#ctries
# Set again the list of categories with only the ones you will update:

# keyword_list = c("Epidemics")

# Nested list with all paths necessary for run_tf_update function:

path_to_update <- mytfs %>% 
    map(function(x) {
    # For every country, check if directory corpus and tf exits i.e. previous extraction performed
    if(dir.exists(paste0(usb_drive,"/",x,"/","corpus")) & dir.exists(paste0(usb_drive,"/",x,"/","tf"))){
      # If yes, get all the paths needed for the run_tf_update and return as list 
    path_tf_to_update <- paste0(usb_drive,"/",x,"/","tf","/","tf_crisis_words_",x,".RData")
    corpus_tf_to_update <- paste0(usb_drive,"/",x,"/","corpus","/","corpus_",x,".RData")
    export_path <- path_tf_to_update
                  return(list(path_tf_to_update = path_tf_to_update, 
                              path_corpus_to_update = corpus_tf_to_update,
                              export_path = export_path))
      }
  }) %>% 
  # Remove NULL elements from the list
  compact()


# Run_tf_update for all countries previously downloaded:

path_to_update %>% 
map(function(x){
  run_tf_update(path_tf_to_update = x[[1]], 
                corpus_path = x[[2]], 
                export_path = x[[3]],
                keyword_list = keyword_list,
                store_old = T, 
                store_old_path = paste0(usb_drive,"/0. Old extraction/tf"))
})


keyword_list=c("Fiscal_outcomes")
x=list("../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf.RData",
       "../Betin_Collodel/2. Text mining IMF_data/datasets/corpus/corpus.RData",
       "../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf_update.RData",
        "../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf_old.RData")

run_tf_update(path_tf_to_update = x[[1]], 
              corpus_path = x[[2]], 
              export_path = x[[3]],
              keyword_list = keyword_list,
              store_old = T, 
              store_old_path = x[[4]])

# Write txt file with details of update:

# Directory with txt

if(!dir.exists("2.updates")){
  dir.create("2.update")
}

# Txt writing

file.create("2.update/update_details.txt", overwrite = TRUE)
sink("2.update/update_details.txt")
cat("Update details:","\n")
cat("\n")
cat("date of update: ", as.character(Sys.time()),"\n")
cat("updated indexes:", paste0(keyword_list, collapse = "  "),"\n")
closeAllConnections()

cat(crayon::green("Indexes succesfully updated."))

}


# #################################################


#consolidate into a single database the tf matrix of all countries

mytfs=setdiff(dir(usb_drive),c("download_docs.r","urls_Requests_Reviews_articleIV.RData","0. logs","0. Old extraction","tf_idf.RData"))

#mytfs[165]
#i=0
mytfs=lapply(mytfs,function(x){
  #i<<-i+1
  #print(i)
  if(dir.exists(paste0(usb_drive,"/",x,"/tf"))){
    tryCatch({
    y=rio::import(paste0(usb_drive,"/",x,"/tf/tf_crisis_words_",x,".RData"))
    data.frame(y)},
    error = function(e){
      cat(crayon::red(x,":","Folder existing, but for some reason missing tf.","\n"))
      return(NULL)}
  )}
  })

mytfs = mytfs %>% compact()
mytfs=try(do.call(plyr::rbind.fill,mytfs))


if("try-error" != class(mytfs)){
  cat(crayon::green("Consolidated tfs succesfully."))
}


# Extract from the names of the files, the country, date and hierarchy of the document.

dt <- mytfs %>% mutate(
  ISO3_Code=substr(file,1,3),
  year=substr(file,5,8),
  Period=as.Date(str_match(mytfs$file,"\\d{4}-\\d{2}-\\d{2}")),
  type_doc=substr(file,str_length(ISO3_Code)+str_length(Period)+3,str_length(file)))

## Compute the idf ----------

LoI_idf=idf(dt)

myidf_plot=idf_barplot(LoI_idf,vars_type=c("economic_shock","debt_outcomes","non_economic_shock","adjustment_program"),idf_trans = T)
myidf_plot$fig+ggsave("2.graphs/tagged docs/tf-idf/idf.png")

## compute the tf-idf ----------
### tf-idf table ------

LoI_tf_idf=tf_idf(dt,weight_method="brut_frequency")

#export the tf_idf
output[["tf_idf_table"]]=LoI_tf_idf
rio::export(LoI_tf_idf,"../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf.RData")
rio::export(LoI_tf_idf,paste0(usb_drive,"/tf_idf.RData"))

### tf-idf by type of crisis ------------------

mytf_plot=tf_barplot(dt,vars_type=c("economic_shock","non_economic_shock","debt_outcomes"))

mytf_plot$tf_fig_avg+ggsave("2.graphs/tagged docs/tf-idf/tf_avg.png")

mytf_plot$tf_fig_avg_prop+ggsave("2.graphs/tagged docs/tf-idf/tf_avg_prop.png")

# Compute the Cosinus similarity of each Crisis -----------------------------

LoI_cos_sim=cosim_matrix(LoI_tf_idf)
output[["cos_sim_table_crisis"]]=LoI_cos_sim
#export the cosinus similariry
rio::export(LoI_cos_sim,"../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/cos_sim.RData")
LoI_cos_sim=data.frame(LoI_cos_sim)
LoI_cos_sim$Crisis=rownames(LoI_cos_sim)
select_cols=names(LoI_cos_sim)[!names(LoI_cos_sim) %in% c("Crisis")] 

output[["cos_sim_fig_crisis"]]=lapply(select_cols,function(x){
  plot_cos_sim(LoI_cos_sim,x)+
    ggsave(paste0("2.graphs/tagged docs/cos_sim/cos_sim_",x,".png"))
})

#Export the final output -----
final_destination="../Betin_Collodel/2. Text mining IMF_data/output/tagged docs/Output_Run_Text_mining.RData"
save(output,file=final_destination)

print(paste0("All the Output of the script has been saved in the following directory:"))
print(final_destination)

#-------------------------------------------


