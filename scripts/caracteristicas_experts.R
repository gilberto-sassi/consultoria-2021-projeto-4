# used packages
library(psych)
library(vcd)
library(irr)
library(readxl)
library(readr)
# library(statBasics)
library(tidyverse)

# Distribuição das características dos experts

tabela <- tribble(~Juiz, ~`Atributo 1`, ~`Atributo 2`, ~`Atributo 3`, ~`Atributo 4`,
                  'Juiz 1', 1, 1, 0, 0,
                  'Juiz 2', 1, 1, 1, 0, 
                  'Juiz 3', 1, 1, 0, 0,
                  'Juiz 4', 1, 1, 1, 0)

tab <- tabela %>% 
  pivot_longer(!Juiz, names_to = 'Atributos', values_to = 'Frequência')

legenda <- c(
  "Atributo 1" = "Habilidades/conhecimentos adquiridos pela experiência na área de Educação Especial",
  "Atributo 2" = "Característica que tornam a(o) profissional com autoridade na área de Educação Especial",
  "Atributo 3" = "Característica que tornam a(o) profissional possuidor(a) de habilidade especial em estudos de validação ",
  "Atributo 4" = "Características que tornam a(o) profissional com alta classificação atribuída por uma autoridade "
)

ggplot(tab) +
  geom_bar(aes(x = Juiz, fill = Atributos, y = `Frequência`), stat = 'identity') +
  theme_minimal() +
  scale_y_continuous("", breaks = NULL) +
  labs(x = 'Juízes') +
  scale_fill_discrete(labels = legenda) +
  guides(fill = guide_legend(nrow = 4)) +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 10),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 5))

ggsave('figures/grafico1.png')
ggsave('figures/grafico1.jpeg')
ggsave('figures/grafico1.pdf')
ggsave('figures/grafico1.eps')
