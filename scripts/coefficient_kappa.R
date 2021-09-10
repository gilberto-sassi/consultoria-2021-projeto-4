# used packages
library(psych)
library(DescTools)
library(fmsb)
library(irr)
library(readxl)
library(readr)
# library(statBasics)
library(tidyverse)

# Reading the data
dados <- readxl::read_xlsx("data/processed/dados.xlsx", sheet = "dados_concordancia") %>%
    purrr::map_dfc(~ stringr::str_replace(.x, "\\s+$", "")) 

f_rename <- function(vetor) {
seq_along(vetor) %>% 
    purrr::map_chr(~ paste0("Item ", .x))
}

dados <- dados %>% dplyr::rename_with(f_rename) %>% as.matrix()
rownames(dados) <- paste0("Juiz ", 1:4)

tab_coef_cohen <- matrix(NA, nrow = 6, ncol = 7)
colnames(tab_coef_cohen) <- c("Juiz 1", "Juiz 2", "Coeficiente Kappa", "Limite inferior IC", "Limite superior IC", "Coeficiente de confiança", "valor-p")

linha  <- 1
for(i in 1:3) {
    for(j in (i + 1):4) {
        tab_coef_cohen[linha, 1] <- i
        tab_coef_cohen[linha, 2] <- j

        coef_cohen <- fmsb::Kappa.test(base::table(dados[i, ], dados[j, ]), conf.level = 0.95)$Result

        tab_coef_cohen[linha, 3] <- coef_cohen$estimate
        tab_coef_cohen[linha, 4] <- max(0, coef_cohen$conf.int[1])
        tab_coef_cohen[linha, 5] <- min(1, coef_cohen$conf.int[2])
        tab_coef_cohen[linha, 6] <- 0.95
        tab_coef_cohen[linha, 7] <- coef_cohen$p.value

        linha <- linha + 1
    }
}

openxlsx::write.xlsx(tab_coef_cohen, file="output/cohen_kappa_pair.xlsx", row.names = F)

teste_geral <- irr::kappam.fleiss(t(dados))
coef_kappa_general  <- cbind(teste_geral$value, teste_geral$p.value)
colnames(coef_kappa_general) <- c("Fleiss' Kappa", "valor p")

openxlsx::write.xlsx(coef_kappa_general, file="output/cohen_kappa.xlsx", row.names = F)
