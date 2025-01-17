# Author: Henry Overos
# Date: Wed Dec  4 23:58:18 2024
# Title: Descriptive Statistics plots for working paper on Indonesia election data
# Notes: 
rm(list=ls())

pacman::p_load(tidyverse, rio, here, sf, readr, reshape2, summarytools, xtable, stargazer, kableExtra, scales, geomtextpath)

filenames <- list.files(here("Data from Hanna"))
csv <- grepl(".csv",filenames)
filenames_csv <- filenames[csv]

for (i in filenames_csv) {
  # Extract the name of the file without the path and extension
  file_name <- tools::file_path_sans_ext(basename(i))
  
  # Dynamically create a variable name using the file name
  assign(file_name, rio::import(here("Data from Hanna", i)))
}

# Cleaning each data set

## dpd
clean_DPD_candidates_2014 <- DPD_candidates_2014 %>%
  select(year = Year, province = "Province name",
         valid_votes = "Valid votes in district",
         name = "Candidate name",
         cand_number = "Candidate number",
         cand_votes = "Candidate number of votes",
         cand_elected = "Candidate elected to the DPD") %>%
  dplyr::mutate(
    cand_number = as.numeric(cand_number),  # Convert cand_number to numeric
    cand_votes = as.numeric(cand_votes),
    cand_elected = if_else(is.na(cand_elected), 0, 1)
  ) %>% 
  group_by(province) %>%          # Group by the province column
  fill(valid_votes, cand_votes, .direction = "down") %>% # Fill the missing values for the specified columns
  ungroup()    

clean_DPD_candidates_2019 <- DPD_candidates_2019 %>%
  select(year = Year, province = "Province name",
         valid_votes = "Valid votes in district",
         name = "Candidate Name",
         cand_number = "Candidate Number",
         cand_votes = "Number of Party Votes (for each party)",
         cand_elected = "Candidate elected to the DPD") %>%
  dplyr::mutate(
    cand_number = as.numeric(cand_number),  # Convert cand_number to numeric
    cand_votes = as.numeric(cand_votes),
    cand_elected = if_else(is.na(cand_elected), 0, 1)
  ) %>% 
  group_by(province) %>%          # Group by the province column
  fill(valid_votes, cand_votes, .direction = "down") %>% # Fill the missing values for the specified columns
  ungroup()    

clean_DPD_candidates_2024 <- DPD_candidates_2024 %>%
  select(year = Year, province = "Province name",
         valid_votes = "Valid votes in district",
         name = "Candidate name",
         cand_number = "Candidate number",
         cand_votes = "Candidate number of votes",
         cand_elected = "Candidate elected to the PD") %>%
  dplyr::mutate(
    cand_number = as.numeric(cand_number),  # Convert cand_number to numeric
    cand_votes = as.numeric(cand_votes),
    cand_elected = if_else(is.na(cand_elected), 0, 1)
  ) %>% 
  group_by(province) %>%          # Group by the province column
  fill(valid_votes, cand_votes, .direction = "down") %>% # Fill the missing values for the specified columns
  ungroup()    

#DPD_bind_rows <- bind_rows(clean_DPD_candidates_2014,clean_DPD_candidates_2019,clean_DPD_candidates_2024) %>% na.omit()

## dpr candidates

clean_DPR_candidates_2019 <- DPR_candidates_2019

DPR_cand_bind_rows <- bind_rows(DPR_candidates_2019, DPR_candidates_2024)

## dpr parties
clean_DPR_parties_2014 <- DPR_parties_2014 %>%
  mutate(`Number of DPR seats` = if_else(is.na(`Number of DPR seats`), 0, `Number of DPR seats`)) %>% 
  group_by(`Party name`) %>% 
  summarise(total_dpr_seats = sum(`Number of DPR seats`, na.rm = TRUE), .groups = "drop") #%>% 
  #fill(`Registered voters in district`, `Valid votes in district`, `Invalid votes in district`, .direction = "down")

# Generate the LaTeX table
latex_table <- clean_DPR_parties_2014 %>%
  kable("latex", booktabs = TRUE, longtable = TRUE, caption = "Number of DPR Seats by Party, 2014 election") %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))

# Write the LaTeX table to a file
writeLines(latex_table, here("dpr_seats_2014_table.tex"))

#2019
## dpr parties
clean_DPR_parties_2019 <- DPR_parties_2019 %>%
  mutate(`Number seats` = if_else(is.na(`Number seats`), 0, `Number seats`)) %>% 
  group_by(`Party name`) %>% 
  summarise(total_dpr_seats = sum(`Number seats`, na.rm = TRUE), .groups = "drop") #%>% 
#fill(`Registered voters in district`, `Valid votes in district`, `Invalid votes in district`, .direction = "down")

# Generate the LaTeX table
latex_table <- clean_DPR_parties_2019 %>%
  kable("latex", booktabs = TRUE, longtable = TRUE, caption = "Number of DPR Seats by Party, 2019 election") %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))

# Write the LaTeX table to a file
writeLines(latex_table, here("dpr_seats_2019_table.tex"))

## dpr parties
clean_DPR_parties_2024 <- DPR_parties_2024 %>%
  mutate(`Number of DPR seats` = if_else(is.na(`Number of DPR seats`), 0, `Number of DPR seats`)) %>% 
  group_by(`Party name`) %>% 
  summarise(total_dpr_seats = sum(`Number of DPR seats`, na.rm = TRUE), .groups = "drop") #%>% 
#fill(`Registered voters in district`, `Valid votes in district`, `Invalid votes in district`, .direction = "down")

# Generate the LaTeX table
latex_table <- clean_DPR_parties_2024 %>%
  kable("latex", booktabs = TRUE, longtable = TRUE, caption = "Number of DPR Seats by Party, 2024 election") %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))

# Write the LaTeX table to a file
writeLines(latex_table, here("dpr_seats_2024_table.tex"))

## Jakarta example
dpr_2024_jakarta <- DPR_parties_2024 %>% 
  filter(`Province name` == "DKI Jakarta", `District number` == 1) %>% 
  mutate(`Number of DPR seats` = if_else(is.na(`Number of DPR seats`), 0, `Number of DPR seats`)) %>% 
  group_by(`Party name`) %>% 
  summarise(total_dpr_seats = sum(`Number of DPR seats`, na.rm = TRUE), .groups = "drop") #%>% 

latex_table <- dpr_2024_jakarta %>%
  kable("latex", booktabs = TRUE, longtable = TRUE, caption = "Number of DPR Seats by Party, 2024 election") %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))

# Write the LaTeX table to a file
writeLines(latex_table, here("dpr_seats_2024_jakarta_table.tex"))


# Gender tables

xtable(table(DPR_candidates_2014$`Party Name`, DPR_candidates_2014$`Candidate Gender L=Male, P=Female`))
xtable(table(clean_DPR_candidates_2019$`Party Name`, clean_DPR_candidates_2019$`Candidate Gender L=Male, P=Female`))
xtable(table(DPR_candidates_2024$Party, DPR_candidates_2024$`Candidate Gender L=Male, P=Female`))
## President and VP

## 2014

clean_pres_2014 <- President_and_VP_2014 %>% 
  group_by(`Province name`) %>% 
  fill(`Registered voters in district`, `Valid votes in district`, `Invalid votes in district`) %>% 
  ungroup() %>% 
  rename(registered_voters = `Registered voters in district`, valid_votes = `Valid votes in district`,
         invalid_votes = `Invalid votes in district`)

## 2019

clean_pres_2019 <- President_and_VP_2019 %>% 
  group_by(`Province name`) %>% 
  fill(`Valid votes`) %>% 
  ungroup() %>% 
  rename(valid_votes = `Valid votes`, candidate_votes = `V11`)

## 2024

clean_pres_2024 <- President_and_VP_2024 %>% 
  group_by(`Province name`) %>% 
  fill(`Registered voters in province`, `Valid votes in province`, `Invalid votes in province`) %>% 
  ungroup() %>% 
  rename(registered_voters = `Registered voters in province`, valid_votes = `Valid votes in province`,
         invalid_votes = `Invalid votes in province`)

pres_bind_rows <- bind_rows(clean_pres_2014, clean_pres_2024)

export(pres_bind_rows, file = here("president_bind.csv"))
export(DPR_cand_bind_rows, file = here("dpr_cand_bind.csv"))

# Tables

# 2014 DPR
#votes_by_party_province_2014 <- clean_DPR_parties_2014 %>%
#  group_by(`Province name`, `Party name`) %>%
#  summarise(Total_Votes = sum(`Party number of votes`, na.rm = TRUE)) %>%
#  arrange(`Province name`, desc(Total_Votes)) %>% filter(Total_Votes != 0)
# 
# print(votes_by_party_province_2014)
# 
# dpr_parties_latex_2014 <- xtable(summary(clean_DPR_parties_2014))

# print(dpr_parties_latex_2014, include.rownames = FALSE)
# 
# # 2019 DPR
# votes_by_party_province_2014 <- clean_DPR_parties_2014 %>%
#   group_by(`Province name`, `Party name`) %>%
#   summarise(Total_Votes = sum(`Party number of votes`, na.rm = TRUE)) %>%
#   arrange(`Province name`, desc(Total_Votes)) %>% filter(Total_Votes != 0)
# 
# dpr_parties_latex_2019 <- xtable(summary(clean_DPR_parties_2019))
# 
# print(dpr_parties_latex_2019, include.rownames = FALSE)
# 
# dpr_candidates_latex_2019 <- xtable(summary(clean_DPR_candidates_2019))
# 
# print(dpr_candidates_latex_2019, include.rownames = FALSE)
# 
# # 2024 DPR
# votes_by_party_province_2014 <- clean_DPR_parties_2014 %>%
#   group_by(`Province name`, `Party name`) %>%
#   summarise(Total_Votes = sum(`Party number of votes`, na.rm = TRUE)) %>%
#   arrange(`Province name`, desc(Total_Votes)) %>% filter(Total_Votes != 0)
# 
# dpr_parties_latex_2024 <- xtable(summary(clean_DPR_parties_2024))
# 
# print(dpr_parties_latex_2024, include.rownames = FALSE)
# 
# dpr_candidates_latex_2024 <- xtable(summary(clean_DPR_candidates_2024))
# 
# print(dpr_candidates_latex_2024, include.rownames = FALSE)
# 
# 2014 DPD

dpd_candidates_latex_2014 <- xtable(summary(clean_DPD_candidates_2014))

print(dpd_candidates_latex_2014, include.rownames = FALSE)

# 2019 DPD

dpr_candidates_latex_2019 <- xtable(summary(clean_DPR_candidates_2019))

print(dpr_candidates_latex_2019, include.rownames = FALSE)

# 2024 DPD

dpr_candidates_latex_2024 <- stargazer(DPR_candidates_2024)

print(dpr_candidates_latex_2024, include.rownames = FALSE)

# Maps

provinces <- st_read(here("Shape files", "2014", "gadm41_IDN_1.shp"))

winners_pres_2014 <- clean_pres_2014 %>% 
  group_by(`Province name`) %>% 
  slice_max(`Percentage of valid votes`, n = 1, with_ties = FALSE) %>% 
  ungroup()

winners_pres_2014 <- winners_pres_2014 %>%
  mutate(`Province name` = case_when(
    `Province name` == "DI. Yogyakarta" ~ "Yogyakarta",
    `Province name` == "DKI Jakarta" ~ "Jakarta Raya",
    TRUE ~ `Province name`  # Keep the name unchanged if no match
  ))

map_d_2014 <- provinces %>% 
  left_join(winners_pres_2014, by = c("NAME_1" = "Province name"))


map_d_2014 <- map_d_2014 %>%
  mutate(vote_percentage = `Percentage of valid votes`) %>%
  mutate(vote_percentage_label = percent(vote_percentage, accuracy = 1))

ggplot(data = map_d_2014) + 
  geom_sf(aes(fill = `Candidate names, President and Vice President`,
              alpha = vote_percentage)) +
  geom_sf_label(aes(label = vote_percentage_label), 
                size = 2, 
                check_overlap = TRUE, 
                color = "black") +
  scale_fill_manual(values = c(
    "H. Prabowo Subianto - Ir. H. M. Hatta Rajasa" = "orange",
    "Ir. H. Joko Widodo - Drs. H. M. Jusuf Kalla" = "red"
  ), name = "Winner") +
  
  # Adjust alpha range for vote percentages
  scale_alpha_continuous(range = c(0.5, 1), guide = "none") +
  
  theme_minimal() +
  labs(title = "Election Winners by Province: 2014", subtitle = "Shading presents provincial voteshare of winner") + 
  theme(legend.position = "bottom")



## 2019

winners_pres_2019 <- clean_pres_2019 %>% 
  group_by(`Province name`) %>% 
  slice_max(candidate_votes, n = 1, with_ties = FALSE) %>% 
  ungroup()

winners_pres_2019 <- winners_pres_2019 %>%
  mutate(`Province name` = case_when(
    `Province name` == "DI. Yogyakarta" ~ "Yogyakarta",
    `Province name` == "DKI Jakarta" ~ "Jakarta Raya",
    TRUE ~ `Province name`  # Keep the name unchanged if no match
  ))

map_d_2019 <- provinces %>% 
  left_join(winners_pres_2019, by = c("NAME_1" = "Province name"))

map_d_2019 <- map_d_2019 %>%
  mutate(vote_percentage = (candidate_votes / valid_votes) * 100) %>%
  mutate(vote_percentage_label = percent(vote_percentage/100, accuracy = 1))

ggplot(data = map_d_2019) + 
  geom_sf(aes(fill = `Candidate names, President and Vice President`,
              alpha = vote_percentage)) +
  geom_sf_label(aes(label = vote_percentage_label), 
                size = 2, 
                check_overlap = TRUE, 
                color = "black") +
  scale_fill_manual(values = c("Prabowo Subianto-Sandiaga Uno" = "orange", 
                               "Joko Widodo-Ma'ruf Amin" = "red"), name = "Winner") +
  scale_alpha_continuous(range = c(0.5, 1), guide = "none") +
  theme_minimal() +
  labs(title = "Election Winners by Province: 2019", 
       subtitle = "Shading presents provincial voteshare of winner in percentage") + 
  theme(legend.position = "bottom")

## 2024

provinces_2024 <- st_read(here("Shape files", "2024", "Provinsi.shp"))

# Create a mapping table for mismatched names
province_name_mapping <- data.frame(
  PROVINSI = c("Daerah Istimewa Yogyakarta", "Kepulauan Bangka Belitung",
               "Kepulauan Riau"
               ),
  Province = c("D.I.Yogyakarta", "Bangka Belitung", "KEP.Riau")
)

winners_pres_2024 <- clean_pres_2024 %>% 
  group_by(`Province name`) %>% 
  slice_max(`Number of valid votes for each candidate pair`, n = 1, with_ties = FALSE) %>% 
  ungroup()

provinces_2024 <- provinces_2024 %>%
  left_join(province_name_mapping, by = "PROVINSI") %>%
  mutate(PROVINSI = if_else(is.na(Province), PROVINSI, Province)) %>%
  select(-Province)  # Drop the temporary column

map_d_2024 <- provinces_2024 %>% 
  left_join(winners_pres_2024, by = c("PROVINSI" = "Province name"))


#validity_check <- st_is_valid(map_d_2024)
#table(validity_check)

map_d_2024 <- st_zm(map_d_2024, drop = TRUE)

#problematic_geometry <- map_d_2024[16514, ]
#print(problematic_geometry)

map_d_2024 <- map_d_2024 %>%
  mutate(vote_percentage = (`Number of valid votes for each candidate pair` / valid_votes) * 100) %>%
  mutate(vote_percentage_label = percent(vote_percentage/100, accuracy = 1))

# Manually adjust colors for candidates and NA
ggplot(data = map_d_2024) + 
  geom_sf(aes(fill = `Candidate names, President and Vice President`), alpha = 0.7) + 
  # Layer 1: Text outline (slightly larger, in contrasting color)
  geom_sf_label(aes(label = vote_percentage_label), 
                size = 2, 
                check_overlap = TRUE, 
                color = "black") +  # Nudging labels to avoid overlap
  scale_fill_manual(values = c("H. ANIES RASYID BASWEDAN, Ph.D. & Dr. (H.C.) H. A. MUHAIMIN ISKANDAR" = "red", 
                               "H. PRABOWO SUBIANTO & GIBRAN RAKABUMING RAKA" = "orange"), name = "Winner") +
  scale_alpha_continuous(range = c(0.5, 1), guide = "none") +  # Remove alpha legend
  theme_minimal() +
  labs(title = "Election Winners by Province: 2024", 
       subtitle = "Shading presents provincial voteshare of winner in percentage") + 
  theme(legend.position = "bottom")
