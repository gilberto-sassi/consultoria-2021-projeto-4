# used packages
library(psych)
library(vcd)
library(irr)
library(readxl)
library(readr)
library(statBasics)
library(tidyverse)

# Reading the data
dados <- readxl::read_xlsx("data/processed/dados.xlsx", sheet = "dados_concordancia")

f_rename <- function(vetor) {
  seq_along(vetor) %>% 
    purrr::map_chr(~ paste0("Item ", .x))
}

f_concordancia <- function(referencia, vetor) {
  seq_along(vetor) %>% 
    purrr::map_dbl(function(i) {
      
      if(referencia[i] %in% 1:10) {
        if(vetor[i] == "Conhecimento") {
          return(1)
        } else {
          return(0)
        }
      }
      
      if(referencia[i] %in% 11:17) {
        if(vetor[i] == "Atitude") {
          return(1)
        } else {
          return(0)
        }
      }
      
      if(referencia[i] %in% 18:22) {
        if(vetor[i] == "Prática") {
          return(1)
        } else {
          return(0)
        }
      }
      
      return(99)
    })
}

f_area <- function(vetor) {
  seq_along(vetor) %>% 
    purrr::map_chr(function(i) {
      if(vetor[i] %in% 1:10) {
        return("Conhecimento")
      } else if (vetor[i] %in% 11:17) {
        return("Atitude")
      } else if(vetor[i] %in% 18:22) {
        return("Prática")
      }
      
      return("error")
    })
}

df <- dados %>% 
  dplyr::rename_with(f_rename) %>% 
  dplyr::mutate(Juizes = paste0("Juiz ", 1:4)) %>% 
  tidyr::pivot_longer(cols = -Juizes, names_to = "Item", values_to = "Respostas") %>% 
  dplyr::mutate(Respostas = stringr::str_replace(Respostas, "\\s+$", "")) %>% 
  dplyr::mutate(itemNum = substr(Item, 6, 7) %>% as.numeric()) %>% 
  dplyr::mutate(concordancia = f_concordancia(itemNum, Respostas)) %>% 
  dplyr::mutate(area = f_area(itemNum))

df_summary_teoria <- df %>% 
  dplyr::group_by(Item) %>% 
  dplyr::summarise(`Número de concordância dos juízes` = sum(concordancia)) %>% 
  dplyr::mutate(itemNum = substr(Item, 6, 7) %>% as.numeric()) %>% 
  dplyr::mutate(area = dplyr::recode(f_area(itemNum),
                       "Atitude" = "Itens pertinentes à atittude",
                       "Conhecimento" = "Itens pertinentes ao conhecimento",
                       "Prática" = "Itens pertinentes à prática"))

ggplot2::ggplot(df_summary_teoria) +
  ggplot2::geom_bar(ggplot2::aes(x = factor(itemNum), y = `Número de concordância dos juízes`, fill = area),
           stat = "identity", width = 0.6) +
  ggplot2::labs(y = "Número de juízes concordantes", x = "Item") + 
  ggplot2::theme_classic() +
  ggplot2::theme(legend.title = ggplot2::element_blank(),
        legend.position = "bottom",
        axis.text = ggplot2::element_text(size = 6)) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 3))

ggsave('figures/grafico3.png')
ggsave('figures/grafico3.jpeg')
ggsave('figures/grafico3.pdf')
ggsave('figures/grafico3.eps')
