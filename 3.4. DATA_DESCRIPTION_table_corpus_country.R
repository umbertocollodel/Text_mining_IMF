######### Description: the script generates
######### 1) table with first year and total the number of documents by country
######### 2) corresponding map of geographical distribution.
######### All the output saved in Dropbox folder.

documents_ctry <- rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf.RData") %>% 
  group_by(ISO3_Code) %>% 
  count() 



# Table ----

# First and last document by country


first_year <- rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf.RData") %>% 
  group_by(ISO3_Code) %>% 
  filter(year == min(year)) %>% 
  filter(!duplicated(year)) %>% 
  select(ISO3_Code, year)

rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf.RData") %>% 
  group_by(ISO3_Code) %>% 
  count() %>% 
  merge(first_year) %>% 
  mutate(`Country name` = countrycode(ISO3_Code, "iso3c","country.name")) %>% 
  select(ISO3_Code, `Country name`, year, n) %>% 
  rename(ISO3 = ISO3_Code,`First document` = year, `N. of documents` = n) %>% 
  stargazer(summary = F, out = "../Betin_Collodel/2. Text mining IMF_data/output/tables/Corpus/ctry_detail.tex")


# Globe map -----

library("rnaturalearth")


world <-ne_countries(scale = "medium", returnclass = "sf") %>% 
  rename(ISO3_Code = wb_a3) %>% 
  left_join(documents_ctry, by = "ISO3_Code")

ggplot(data = world) +
  geom_sf(aes(fill = n),col = "black") +
  scale_fill_gradient(low='white', high='green4',name = "Number of documents") +
  theme_minimal() +
  labs(title=myyear)+
  theme(legend.title = element_text(size = 15),
        legend.text = element_text(size = 15))
  
#ggsave("../Betin_Collodel/2. Text mining IMF_data/output/figures/Corpus/ctry_detail.png")

 


