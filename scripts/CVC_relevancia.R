# used packages
library(psych)
library(vcd)
library(irr)
library(readxl)
library(readr)
# library(statBasics)
library(tidyverse)

# Reading data
dados <- readxl::read_xlsx("data/processed/dados.xlsx", sheet = "dados_relevancia")

f_rename <- function(vetor) {
  seq_along(vetor) %>%
    purrr::map_chr(~paste0("Item ", .x))
}

df <- dados %>% # recoding
  dplyr::rename_with(f_rename) %>%
  tidyr::pivot_longer(cols = tidyselect::everything(), names_to = "Item", values_to = "Avaliação") %>%
  dplyr::mutate(`Avaliação` = stringr::str_replace(`Avaliação`, "\\s+$", "")) %>% # remove trailing spaces
  dplyr::mutate(`Recodificação` = dplyr::recode(`Avaliação`,
                                    "nada relevante" = 1,
                                    "pouco relevante" = 2,
                                    "muito relevante" = 3,
                                    "totalmente relevante" = 4))

df_summary_relevancia <- df %>%
  dplyr::group_by(Item) %>%
  dplyr::summarise(Mx = mean(Recodificação), Pe = 0.25, Vmax = max(Recodificação),
            `CVC -- relevância` = Mx / Vmax - Pe) %>%
  dplyr::arrange(`CVC -- relevância`)

openxlsx::write.xlsx(df_summary_relevancia, file = "output/calculoCVC_relevancia.xlsx", row.names = F)
