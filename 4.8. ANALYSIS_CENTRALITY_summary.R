#' @title summary figures of probability
#' @description script to analyze how each crisis moves 
#' within the macroeconomic system during the last 70 years 
#' i.e. centrality of the shocks and evolution of whole system
#' @author Manuel Betin, Umberto Collodel
#' @return figures in the folder Probability

#INSTRUCTIONS: To run this file separatly please first run 4.ANALYSIS_source.R from line 1 to ligne 51 to load the 
#packages and functions


path_data_directory="../Betin_Collodel/2. Text mining IMF_data"

# Pre-process: ------
# Creation of a nested list with income group first level and time bucket second level

# Import: 

mydata <- rio::import(paste0(path_data_directory,"/datasets/tagged docs/tf_idf.RData")) %>% 
  mutate(year = as.numeric(year))%>% 
  select(-Soft_recession, -Banking_crisis) %>% 
  mutate_at(vars(Epidemics:World_outcomes), funs(norm = (. - mean(.,na.rm=T))/sd(.,na.rm=T))) %>% 
  ungroup() 

# Classification data:


income_groups <- c("High income","Upper middle income","Low income")

classification <- rio::import(paste0(path_data_directory,"/datasets/comparison/other_data.RData")) %>% 
  select(ISO3_Code,Income_group,group) %>% 
  filter(!duplicated(ISO3_Code)) %>% 
  mutate(Income_group = ifelse(Income_group == "Lower middle income","Low income",Income_group))

# Final and different income groups df:

mydata <- mydata %>% 
merge(classification) 

mydata_income <- income_groups %>% 
  map(~ mydata %>% filter(Income_group == .x)) 


names(mydata_income) <- income_groups


# Select normalized variable 

vars_norm <- vars_select(names(mydata), ends_with('norm'))


# Lists:
# All countries, list with elements time buckets dfs

mydata <- mydata %>% mutate(bucket = case_when(year >= 1950 & year <= 1976 ~ "1950:1976",
                                         year >= 1976 & year <= 1992 ~ "1976:1992",
                                         year >= 1992 & year <= 2003 ~ "1992:2003",
                                         year >= 2003 & year <= 2013 ~ "2003:2012",
                                         year >= 2013 & year <= 2019 ~ "2013:2019"))

mydata <- split(mydata,mydata$bucket)
    
# Nested list: first element income group, second time bucket

final <- mydata_income %>% 
  map(~ .x %>% mutate(bucket = case_when(year >= 1950 & year <= 1976 ~ "1950:1976",
                                         year >= 1976 & year <= 1992 ~ "1976:1992",
                                         year >= 1992 & year <= 2003 ~ "1992:2003",
                                         year >= 2003 & year <= 2013 ~ "2003:2013",
                                         year >= 2013 & year <= 2019 ~ "2013:2019"
  ))) %>% 
  map(~ split(.x, .x$bucket))



# Network evolution general characteristics: -----
###### Degree - number of links in the network

# Set function - in correlation matrix exclude all values less than a minimum

set_threshold <- function(x,min_cor = 0.2){
  ifelse(x > min_cor, x, 0)
}

# Param for vectorization over different minima - change at need

vector_min_cor <- c(0.2,0.3,0.4)

# Dataframe creation:

corr_final <- vector_min_cor %>% 
  map(function(y){
    final %>%  
    modify_depth(2, ~ .x %>% select(vars_norm)) %>% 
    modify_depth(2, ~ .x %>% cor(use = "complete.obs")) %>% 
    modify_depth(2, ~ .x[lower.tri(.x, diag = F)]) %>% 
    modify_depth(2, ~ data.frame(links = .x) %>% mutate_all(set_threshold,y) %>% filter(links != 0)) %>% 
    modify_depth(2, ~ .x %>% count()) %>% 
    map(~ bind_rows(.x,.id = "period")) %>% 
    bind_rows(.id = "group")}
    ) %>% 
  map2(vector_min_cor, ~ .x %>% mutate(min_cor = .y)) %>% 
  bind_rows()
  
# Plot for a single value of minimum correlation:

corr_final %>%
  filter(min_cor == 0.2) %>% 
  ggplot(aes(period, n, col = group, group = 1)) +
  geom_line() +
  facet_wrap(~ group) +
  theme_minimal() +
  scale_color_grey() +
  xlab("") +
  ylab("Number of edges") +
  theme(legend.position = "none", 
        axis.text=element_text(size=14), axis.text.x = element_text(size =14,angle=90),
        axis.title.y = element_text(size=14),
        strip.text = element_text(face="bold", size=14))
  

ggsave(paste0(path_data_directory,"/output/figures/Complexity/Evolution/complexity_evolution.png"),
       dpi = "retina")


# Table:


corr_final %>% 
  spread("period","n") %>% 
  rename(`Income group` = group, `Min. Corr.` = min_cor) %>% 
  stargazer(summary = F, out = paste0(path_data_directory,"/output/tables/Complexity/Evolution/complexity_evolution.tex"))


# Footnote export:

footnote=c("Minimum correlation indicates that pairwise correlations lower than the respective value are set equal to 0 when building the adjacency matrix.")

cat(footnote,file=paste0(path_data_directory,"/output/tables/Complexity/Evolution/complexity_evolution_footnote.tex"))



#### Average path length

avg_path_length <- vector_min_cor %>% 
  map(function(y){
  final %>%  
      modify_depth(2, ~ .x %>% select(vars_norm)) %>% 
      modify_depth(2, ~ .x %>% cor(use = "complete.obs")) %>% 
      modify_depth(2, ~ ifelse(.x < y, 0, .x)) %>% 
      modify_depth(2, ~ graph_from_adjacency_matrix(.x, mode = "undirected", diag = F, weighted = T)) %>%
      modify_depth(2, ~ mean_distance(.x, unconnected = F)) %>% 
      map(~ bind_rows(.x)) %>% 
      bind_rows(.id = "Income group")}) %>% 
  map2(vector_min_cor, ~ .x %>% mutate(min_cor = .y)) %>% 
  bind_rows()

avg_path_length %>% 
  mutate_if(is.double, round, 2) %>%
  rename(`Min. Corr` = min_cor) %>% 
  select(`Income group`,`Min. Corr`,everything()) %>% 
  arrange(`Income group`) %>% 
  stargazer(summary = F, out = paste0(path_data_directory,"/output/tables/Complexity/Evolution/average_path_length.tex"))

# Export footnote:

footnote=c("Minimum correlation indicates that pairwise correlations lower than the respective value are set equal to 0 when building the adjacency matrix.
           If two nodes are not connected, their shortest distance is set equal to the number of nodes in the network.")

cat(footnote,file=paste0(path_data_directory,"/output/tables/Complexity/Evolution/average_path_length_footnote.tex"))

#### Degree distribution

degree_distribution <- vector_min_cor %>% 
  map(function(y){
  final %>%  
  modify_depth(2, ~ .x %>% select(vars_norm)) %>% 
  modify_depth(2, ~ .x %>% cor(use = "complete.obs")) %>% 
  modify_depth(2, ~ ifelse(.x < y, 0, .x)) %>% 
  modify_depth(2, ~ graph_from_adjacency_matrix(.x, mode = "undirected", diag = F, weighted = T)) %>%
  modify_depth(2, ~ degree(.x)) %>% 
  modify_depth(2, ~ stack(.x)) %>% 
  modify_depth(2, ~ hist(.x$values, plot = F)$count) %>% 
  modify_depth(2, ~ kurtosis(.x)) %>% 
  map(~ bind_rows(.x)) %>% 
  bind_rows(.id = "Income group")}) %>% 
  map2(vector_min_cor, ~ .x %>% mutate(min_cor = .y)) %>% 
  bind_rows()
  
degree_distribution %>% 
  mutate_if(is.double, round, 2) %>% 
  rename(`Min. Corr` = min_cor) %>% 
  select(`Income group`,`Min. Corr`,everything()) %>% 
  arrange(`Income group`) %>% 
  stargazer(summary = F, out = paste0(path_data_directory,"/output/tables/Complexity/Evolution/degree_distribution.tex"))

footnote=c("Minimum correlation indicates that pairwise correlations lower than the respective value are set equal to 0 when building the adjacency matrix.
          Value is left blank when no edges in the network.")

cat(footnote,file=paste0(path_data_directory,"/output/tables/Complexity/Evolution/degree_distribution_footnote.tex"))


# Network graph evolution -----

names_col <- c("Epidemics","Nat. disaster","Wars","BoP","Banking","Commodity","Contagion","Currency",
               "Expectations","Financial","Housing","Inflation","Migration","Political","Eco. recession",
               "Social","Sovereign","Trade","World")

titles <- c("1950:1976","1976:1992","1992:2003","2003:2013","2013:2019")

size_nodes <- mydata %>% 
  map(~ .x %>% select(vars_norm)) %>% 
  map(~ .x %>% rename_all(~ names_col)) %>% 
  map(~ .x %>% cor(use = "complete.obs")) %>% 
  map(~ .x %>% graph_from_adjacency_matrix(mode = "undirected", diag = F, weighted = T)) %>% 
  map(~ eigen_centrality(.x)$vector) %>% 
  map(~ .x %>% stack()) %>% 
  map(~ .x %>% rename(value = values,  id = ind)) %>% 
  map(~ .x %>% mutate(font.size = 22))


vis_net <- mydata %>%  
      map( ~ .x %>% select(vars_norm)) %>% 
      map(~ .x %>% rename_all(~ names_col)) %>% 
      map( ~ .x %>% cor(use = "complete.obs")) %>% 
      map( ~ ifelse(.x < 0.1, 0, .x)) %>% 
      map( ~ graph_from_adjacency_matrix(.x, mode = "undirected", diag = F, weighted = T)) %>% 
      map( ~ toVisNetworkData(.x)) %>% 
      map2(size_nodes, ~ list(nodes = merge(.x$nodes, .y), edges = .x$edges)) %>% 
      map(~ list(nodes = .x$nodes, edges = .x$edges %>% mutate(color = case_when(weight >= 0.4 ~ "darkred",
                                                                                 weight <= 0.4 & weight >= 0.2 ~ "darkorange",
                                                                                 TRUE ~ "gold")) %>%
                                                        mutate(width = case_when(weight >= 0.4 ~ 6,
                                                                                 weight <= 0.4 & weight >= 0.2 ~ 3,
                                                                                 TRUE ~ 1)),
                 ledges = data.frame(color = c("darkred","darkorange","gold"), label = c("> 0.4","0.4 - 0.2","< 0.2"), arrows = c("undirected"),
                                     width = c(6,3,1), font.align = "top"))) %>% 
      map2(titles, ~ visNetwork(.x$nodes,.x$edges, main = .y) %>% 
             visNodes(color = list(background = "gray", border = "black")) %>% 
             visPhysics(solver = "forceAtlas2Based",forceAtlas2Based = list(gravitationalConstant = -50)) %>%
             visLegend(addEdges = .x$ledges, position = "right") %>% 
             visLayout(randomSeed = 346))


# Footnote

footnote=c("Adjacency matrix built from pairwise correlations between term-frequencies: minimum correlation to display 
           edge equal to 0.1. Size of nodes proportional to their eigencentrality. Legend indicates correlations between categories.
           Visualization of the network through the ForceAtlas2 algorithm.")

cat(footnote,file=paste0(path_data_directory,"/output/figures/Complexity/Evolution/network_footnote.tex"))



# Problems with automation saving! To do

# Calculation eigencentrality by time bucket (all countries): ------

network <- mydata %>% 
  map(~ .x %>% select(vars_norm)) %>% 
  map(~ .x %>% rename_all(~ names_col)) %>% 
  map(~ .x %>% cor(use = "complete.obs")) %>% 
  map(~ .x %>% graph_from_adjacency_matrix(mode = "undirected", diag = F, weighted = T))


centrality <- network %>%
  map(~ eigen_centrality(.x)$vector) %>% 
  map(~ .x %>% stack()) %>% 
  map(~ .x %>% rename(eigencentrality = values, category = ind)) %>% 
  map(~ .x %>% select(category, everything())) %>% 
  map(~ .x %>% arrange(-eigencentrality))


centrality %>% 
  bind_rows(.id = "period") %>%
  mutate(category = factor(category, c("Contagion","World","Banking","BoP","Currency","Expectations","Financial","Sovereign",
                                       "Commodity","Eco. recession","Eco. slowdown","Housing","Inflation","Trade",
                                      "Epidemics","Migration","Nat. disaster","Political","Social","Wars"))) %>% 
        ggplot(aes(period, category, fill= eigencentrality, alpha = eigencentrality)) +
        geom_tile(col = "black") +
        theme_minimal() +
        ylab("") +
        xlab("") +
        labs(fill = "Eigencentrality") +
        theme(axis.text.x = element_text(size =13,angle=90, vjust=0.5, hjust=1), axis.text.y = element_text(size = 14), 
              axis.title.y = element_text(size = 14),
              legend.position = "none") +
        scale_fill_gradient(low = "white",high = "red") +
        coord_fixed(ratio = .6)

ggsave(paste0(path_data_directory,"/output/figures/Complexity/Eigencentrality/Eigencentrality_All.png"),
       height = 4,
       width = 5,
       dpi = "retina")

# Calculation eigencentrality by income group and time bucket: -----

corr_final <- final %>% 
      modify_depth(2, ~ .x %>% select(vars_norm)) %>% 
      modify_depth(2, ~ .x %>% rename_all(~names_col)) %>% 
      modify_depth(2, ~ .x %>% cor(use = "complete.obs"))

network <- corr_final %>% 
  modify_depth(2, ~ graph_from_adjacency_matrix(.x, mode = "undirected",diag = F, weighted = T))

centrality <- network %>% 
  modify_depth(2, ~ eigen_centrality(.x)$vector) %>% 
  modify_depth(2, ~ .x %>% stack()) %>% 
  modify_depth(2, ~ .x %>% rename(eigencentrality = values, category = ind)) %>% 
  modify_depth(2, ~ .x %>% select(category, everything())) %>% 
  modify_depth(2, ~ .x %>% arrange(-eigencentrality)) %>% 
  modify_depth(2,~ .x %>% mutate(category = str_remove(category,"_norm"))) %>% 
  modify_depth(2, ~ .x %>% mutate(category = str_replace_all(category,"_"," ")))

centrality[["Low income"]][["1950:1976"]]$eigencentrality <- 0 


# Heatmap plot:

heatmap_eigencentrality <- centrality %>% 
  map(~ .x) %>% 
  map(~ bind_rows(.x, .id = "period")) %>%
  map(~ .x %>% mutate(category = factor(category, c("Contagion","World","Banking","BoP","Currency","Expectations","Financial","Sovereign",
                                                      "Commodity","Eco. recession","Eco. slowdown","Housing","Inflation","Trade",
                                                      "Epidemics","Migration","Nat. disaster","Political","Social","Wars")))) %>% 
  map(~ .x %>% 
        ggplot(aes(period, category, fill= eigencentrality, alpha = eigencentrality)) +
        geom_tile(col = "black") +
        theme_minimal() +
        ylab("") +
        xlab("") +
        labs(fill = "Eigencentrality") +
        theme(axis.text.x = element_text(size =13,angle=90,vjust=0.5, hjust=1), axis.text.y = element_text(size = 14), 
              axis.title.y = element_text(size = 14),
              legend.position = "none") +
        scale_fill_gradient(low = "white",high = "red") +
        coord_fixed(ratio = .6)
)

heatmap_eigencentrality %>% 
  map2(names(heatmap_eigencentrality), ~ ggsave(paste0(path_data_directory,"/output/figures/Complexity/Eigencentrality/Eigencentrality_",.y,".png"),
               plot = .x,
               height = 4,
               width = 5,
               dpi = "retina"))

# Footnote export:

footnote=c("Scales of red indicate the eigenvector centrality of a specific category during a precise time period, where a brighter red indicates higher
           eigencentrality. The adjacency matrix is built from the correlation matrix of all categories within the period under consideration. The algorithm does not converge
           for the period 1950-1976 in low income countries because of the low pairwise correlations, hence eigentrality is not displayed.")

cat(footnote,file=paste0(path_data_directory,"/output/figures/Complexity/Eigencentrality/Eigencentrality_footnote.tex"))




# Distribution network. ranking is not the same with highly distributed network and uniform one.
# Unfortunately, the analogy of snapshots to a motion picture also reveals the main difficulty with this approach: the time steps employed are very rarely suggested by the network and are instead arbitrary. 
# Using extremely small time steps between each snapshot preserves resolution, but may actually obscure wider trends which only become visible over longer timescales. 
# Conversely, using larger timescales loses the temporal order of events within each snapshot
  
  
# Interesting that for middle income it seems not stable over time. Investigate more on this:

centrality %>% 
  map(~ .x) %>% 
  map(~ bind_rows(.x, .id = "period")) %>% 
  map(~ .x %>% group_by(category) %>% summarise(sum_eigen =sum(eigencentrality))) %>% 
  bind_rows(.id = "Income group") %>% 
  ggplot(aes(x = sum_eigen, fill = `Income group`, alpha = 0.5)) +
  geom_density() +
  theme_minimal()
  

centrality %>% 
  map(~ .x) %>% 
  map(~ bind_rows(.x, .id = "period")) %>% 
  map(~ .x %>% group_by(category) %>% summarise(sum_eigen =sum(eigencentrality))) %>% 
  bind_rows(.id = "Income group") %>% 
  group_by(`Income group`) %>% 
  summarise(stability = mean(sum_eigen, na.rm = T))

# Same thing with animation:

animated_df <- centality %>%
  map(~ .x %>% mutate(category = fct_reorder(category, eigencentality))) %>% 
  bind_rows(.id = "Time span") 

plot <- animated_df %>% 
        ggplot(aes(category, eigencentality, fill= type)) +
        geom_col() +
        coord_flip() +
        theme_bw() +
        ylab("") +
        xlab("") +
        labs(fill = "Type of shock:")

plot + transition_states(`Time span`, state_length = 6, transition_length = 4)





