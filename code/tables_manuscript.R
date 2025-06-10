## Code to reproduce the tables in the main manuscript

# Settings -----------------------------------------------------------------
## load the libraries
library(tidyverse)
library(knitr)
library(kableExtra)

theme_set(theme_bw())
tabs_path <- "./results/tables/"

## load the data
# data <- readRDS("not-available")


# Table2 ------------------------------------------------------------------
data_t2 <- data |> 
  select(Name, Temporada, lesio, Equip, CategoriaDef, Age_final_season, Talla, Pes, BMI) |>  
  mutate(Equip = factor(recode(Equip,
                               Aleví    = "U12",
                               Infantil = "U14",
                               Cadet    = "U16",
                               Juvenil  = "U18",
                               `2n Equip` = "2ndTeam",
                               `1r Equip` = "1stTeam"), 
                        levels = c("U12", "U14", "U16", "U18", "2ndTeam", "1stTeam"))) |> 
  unique()
## nrow(data_t1)  
data_t2_aux <- data_t2 |> 
  group_by(Equip, Temporada) |>
  summarise(n = n_distinct(Name)) |> 
  ungroup() |> 
  summarise(n_jug_season = sum(n), .by = Equip) |> 
  mutate(n_jug_season_pct = round(n_jug_season/sum(n_jug_season)*100, 1))

data_t2 <- data_t2 |> 
  group_by(Equip) |> 
  summarise(age_mean = mean(Age_final_season) |> round(1),
            age_range = paste0(min(Age_final_season), " - ", max(Age_final_season)),
            height_mean = mean(Talla, na.rm = T) |> round(1),
            height_range = paste0(min(Talla, na.rm = T), " - ", max(Talla, na.rm = T)),
            weight_mean = mean(Pes, na.rm = T) |> round(1),
            weight_range = paste0(min(Pes, na.rm = T), " - ", max(Pes, na.rm = T)),
            BMI_mean = mean(BMI, na.rm = T) |> round(1),
            BMI_range = paste0(round(min(BMI, na.rm = T), 1), " - ", round(max(BMI, na.rm = T), 1))) 
table2 <- left_join(data_t2_aux,
          data_t2) |>
  kable(format = "html", 
        caption = "Anthropometric characeristics of players stratified by player category and team levels") |> 
  kable_styling(full_width = TRUE)
save_kable(x = table2, file = paste0(tabs_path, "table2.html"))



# Table3 ------------------------------------------------------------------
data_ama <- data |> 
  filter(CategoriaDef == "Youth") |> 
  droplevels()
tbl <- table(data_ama$Bodypart, data_ama$Tissue, dnn = c("Injury Location", "Type of injury"))
pct.tbl <- round(prop.table(tbl, margin = 2)*100, 2)

tbl_final <- matrix(mapply(mypaste, tbl, pct.tbl), byrow = F,
                    nrow = nrow(tbl), dimnames = dimnames(tbl))      

totals <- addmargins(tbl)
tbl_final <- cbind(tbl_final, as.character(totals[,"Sum"]))
tbl_final <- rbind(tbl_final, paste0(as.character(totals["Sum",])," (100\\%)"))
dimnames(tbl_final) <- list(c(rownames(tbl), "TOTAL"), c(colnames(tbl), "TOTAL"))

table3 <- tbl_final |>
  kable(format = "html", 
        caption = "Distribution of injuries, frequencies and percentages (by column) by tissue and body part in Youth players") |> 
  kable_styling(full_width = TRUE)
save_kable(x = table3, file = paste0(tabs_path, "table3.html"))



# Table4 ------------------------------------------------------------------
data_prof <- data |> 
  filter(CategoriaDef == "Professional") |> 
  droplevels()
tbl <- table(data_prof$Bodypart, data_prof$Tissue, dnn = c("Injury Location", "Type of injury"))
pct.tbl <- round(prop.table(tbl, margin = 2)*100, 2)

mypaste <- function(x,y) paste0(x, " (", y, "\\%)")
tbl_final <- matrix(mapply(mypaste, tbl, pct.tbl), byrow = F,
                    nrow = nrow(tbl), dimnames = dimnames(tbl))

totals <- addmargins(tbl)
tbl_final <- cbind(tbl_final, as.character(totals[,"Sum"]))
tbl_final <- rbind(tbl_final, paste0(as.character(totals["Sum",])," (100\\%)"))
dimnames(tbl_final) <- list(c(rownames(tbl), "TOTAL"), c(colnames(tbl), "TOTAL"))

table4 <- tbl_final |>
  kable(format = "html", 
        caption = "Distribution of injuries, frequencies and percentages (by column) by tissue and body part in Senior players") |> 
  kable_styling(full_width = TRUE)
save_kable(x = table4, file = paste0(tabs_path, "table4.html"))




