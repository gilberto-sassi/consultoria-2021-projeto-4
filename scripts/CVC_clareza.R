# used packages
library(psych)
library(vcd)
library(irr)
library(readxl)
library(readr)
library(writexl)
library(tidyverse)

# Reading the data
dados <- readxl::read_xlsx("data/processed/dados.xlsx", sheet = "dados_clareza")

f_rename <- function(vetor) {
seq_along(vetor) %>%
    purrr::map_chr(~ paste0("Item ", .x))
}

# recodificação
df <- dados %>%
  dplyr::rename_with(f_rename) %>%
  tidyr::pivot_longer(cols = tidyselect::everything(), names_to = "Item",
                      values_to = "Avaliação") %>%
  dplyr::mutate(`Recodificação` = recode(`Avaliação`, "muito claro" = 3,
                                  "nada claro" = 1,
                                  "pouco claro" = 2,
                                  "totalmente claro" = 4))

df_summary_clareza <- df %>%
  dplyr::group_by(Item) %>%
  dplyr::summarise(Mx = mean(Recodificação), Pe = 0.25,
                    Vmax = max(Recodificação),
            `CVC -- clareza e compreensão` = Mx / Vmax - Pe) %>%
  dplyr::arrange(`CVC -- clareza e compreensão`)

writexl::write_xlsx(df_summary_clareza, path = "output/calculoCVC_clareza.xlsx")
