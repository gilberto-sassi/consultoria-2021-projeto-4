# used packages
library(psych)
library(vcd)
library(irr)
library(readxl)
library(readr)
# library(statBasics)
library(tidyverse)


# reading the data
df_cvc_clareza <- readxl::read_xlsx("output/calculoCVC_clareza.xlsx")
df_cvc_relevancia <- readxl::read_xlsx("output/calculoCVC_relevancia.xlsx")

df_cvc_clareza <- df_cvc_clareza %>% dplyr::select(-Mx, -Vmax, -Pe)
df_cvc_relevancia <- df_cvc_relevancia %>% dplyr::select(-Mx, -Vmax, -Pe)

df_cvc <- dplyr::inner_join(df_cvc_clareza, df_cvc_relevancia, by = "Item") %>% 
  dplyr::arrange(`CVC -- clareza e compreensão`, `CVC -- relevância`)

openxlsx::write.xlsx(df_cvc, file = "output/calculoCVC_total.xlsx", row.names = F)

legenda_fill <- c(
  "CVC -- clareza e compreensão" = "Clareza e compreensão",
  "CVC -- relevância" = "Relevância"
)

legenda_color <- c(
  "CVC -- clareza e compreensão" = "Clareza e compreensão",
  "CVC -- relevância" = "Relevância"
)

df_cvc <- df_cvc %>% 
  tidyr::pivot_longer(cols = tidyselect::starts_with("CVC"), names_to = "Divisão", values_to = "CVC") %>% 
  dplyr::mutate(ItemNum = substr(Item, 6, 7) %>% as.numeric()) %>% 
  dplyr::arrange(CVC)

# Gráfico CVC -- primeira versão (benchmark: 80% de 0,75)

df_hline <- tibble::tibble(x = seq(from = 0, 23, by = 0.01)) %>% 
  dplyr::mutate(y = rep(1 - 1 / 52, length(x)))

ggplot2::ggplot(df_cvc, ggplot2::aes(x = ItemNum)) +
  ggplot2::geom_bar(ggplot2::aes(y = CVC, fill = `Divisão`, color = `Divisão`),
           stat = "identity", position = position_dodge(width = 0.6), width = 0.5,
           size = 0.1) +
  ggplot2::geom_line(data = df_hline, ggplot2::aes(x = x, y = y, linetype = "corte"),
            size = 0.5, color = "darkblue") +
  ggplot2::labs(x = "Item") +
  ggplot2::scale_fill_manual(values = c("CVC -- clareza e compreensão" = "gray", "CVC -- relevância" = "orange"),
                    labels = legenda_fill) +
  ggplot2::scale_color_manual(values = c("CVC -- clareza e compreensão" = "blue",
                                "CVC -- relevância" = "white"),
                     labels = legenda_color) +
  
  ggplot2::scale_linetype_manual(values = c("corte" = "solid"),
                        labels = "Valor mínimo recomendado") +
  ggplot2::guides(fill = guide_legend(nrow = 3))+
  ggplot2::theme_classic() +
  ggplot2::scale_x_continuous(breaks = 1:22,
                     labels = 1:22) +
  ggplot2::scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.1), limits = c(0,1)) +
  ggplot2::theme(legend.title = ggplot2::element_blank(),
        legend.position = "bottom",
        axis.text = ggplot2::element_text(size = 5),
        legend.text = ggplot2::element_text(size = 7))

ggplot2::ggsave('figures/grafico2_80_perc.png')
ggplot2::ggsave('figures/grafico2_80_perc.jpeg')
ggplot2::ggsave('figures/grafico2_80_perc.pdf')
ggplot2::ggsave('figures/grafico2_80_perc.eps')

# Gráfico CVC -- primeira versão (benchmark: 0,8)

df_hline <- tibble::tibble(x = seq(from = 0, 23, by = 0.01)) %>% 
  dplyr::mutate(y = rep(1 -  1 / 52, length(x)))

ggplot2::ggplot(df_cvc, ggplot2::aes(x = ItemNum)) +
  ggplot2::geom_bar(ggplot2::aes(y = CVC, fill = `Divisão`, color = `Divisão`),
           stat = "identity", position = position_dodge(width = 0.6), width = 0.5,
           size = 0.1) +
  ggplot2::geom_line(data = df_hline, ggplot2::aes(x = x, y = y, linetype = "corte"),
            size = 0.5, color = "darkblue") +
  ggplot2::labs(x = "Item") +
  ggplot2::scale_fill_manual(values = c("CVC -- clareza e compreensão" = "gray", "CVC -- relevância" = "orange"),
                    labels = legenda_fill) +
  ggplot2::scale_color_manual(values = c("CVC -- clareza e compreensão" = "blue",
                                "CVC -- relevância" = "white"),
                     labels = legenda_color) +
  
  ggplot2::scale_linetype_manual(values = c("corte" = "solid"),
                        labels = "Valor mínimo recomendado") +
  ggplot2::guides(fill = guide_legend(nrow = 3))+
  ggplot2::theme_classic() +
  ggplot2::scale_x_continuous(breaks = 1:22,
                     labels = 1:22) +
  ggplot2::scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.1), limits = c(0,1)) +
  ggplot2::theme(legend.title = ggplot2::element_blank(),
        legend.position = "bottom",
        axis.text = ggplot2::element_text(size = 5),
        legend.text = ggplot2::element_text(size = 7))

ggplot2::ggsave('figures/grafico2_0_8.png')
ggplot2::ggsave('figures/grafico2_0_8.jpeg')
ggplot2::ggsave('figures/grafico2_0_8.pdf')
ggplot2::ggsave('figures/grafico2_0_8.eps')

