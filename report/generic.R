#' Convert numeric to character
#' 
#' @description \code{cortar} divides the range of x into intervals and codes
#' the values in x according to which interval they fall. The leftmost
#' interval corresponds to level one, the next leftmost to level two and so on.
#' 
#' @details \code{cortar} is similar to \code{base::cut}, but we have modify to use
#' with the package \code{bookdown}
#' 
#' @usage cortar(x)
#' 
#' @param x  numeric vector which is to be converted to a character vector by
#' cutting.
#' @param breaks either a numeric vector of two or more unique cut points or a
#' single number (greater than or equal to 2) giving the number of intervals
#' into which x is to be cut.
#' @param open_left logical, \code{TRUE} indicates to include in the interval the
#' left limit
#' @param open_right logica, \code{TRUE} indicates to include in the interval the
#' right limit
#' @param dig_lab nteger which is used when labels are not given. It determines
#' the number of digits used in formatting the break numbers.
#' 
#' @return a character vector with the new codification
#' 
#' @examples 
#' x <- rnorm(1000)
#' cortar(x, breaks = -10:10)
cortar <- function(x, breaks, open_left = T, open_right = F, dig_lab = 3){
  
  cut(x, breaks = breaks, include.lowest = !open_left,
      right = !open_right, dig.lab = dig_lab) %>% 
    map_chr(function(u){
      valor <- u
      if (str_detect(valor, '^\\[') & str_detect(valor, '\\)$')) {
        valor <- str_replace_all(valor, ',', ' |=== ')
      } else if (str_detect(valor, '^\\[') & str_detect(valor, '\\]$')){
        valor <- str_replace_all(valor, ',', ' |===| ')
      } else if (str_detect(valor, '^\\(') & str_detect(valor, '\\]$')) {
        valor <- str_replace_all(valor, ',', ' ===| ')
      }
      str_replace_all(valor, '(\\[|\\]|\\(|\\))', '')
    })
}

#' Distribution table
#' 
#' \code{table_distribution} computes the distribution table for a character,
#' integer or discrete vector
#' 
#' @param df a tibble 
#' @param variable a scalar character 
#' @param rotulo a label for the distribution table
#' @param na_rm remove rows with NA values
#' 
#' @return a tibble that is distribution table
#' 
#' @usage table_distribution(df, variable, rotulo)
#' 
#' @examples 
#' n <- 1000
#'df <- tibble(nome = sample(c('A', 'B', 'C'), size = n, replace = T,
#'                           prob = c(0.6, 0.2, 0.2)),
#'            numero = rnorm(n))
#'table_distribution(df, 'nome', 'Nome')
table_distribution <- function(df, variable, rotulo, legenda = "Tabela de Distribuição de Frequências.", na_rm = TRUE) {
  
  suppressWarnings({
    
    
    if(na_rm) {
      df_clean <- df %>% filter(.[[variable]]  != "")
    } else {
      df_clean <- df 
    }

    tab <- df_clean %>%
      group_by_(variable) %>%
      summarise(frequencia = n(), .groups = 'drop') %>% 
      mutate(frequencia_relativa = frequencia / sum(frequencia),
             porcentagem = frequencia_relativa * 100) %>%
      arrange(desc(porcentagem))
  })
  
  tab <- tab %>%
    add_case(frequencia = sum(tab$frequencia),
             frequencia_relativa = sum(tab$frequencia_relativa),
             porcentagem = sum(tab$porcentagem))
  
  v_log <- is.na(tab[[1]])
  tab[[1]][v_log] <- 'Total'
  
  colnames(tab) <- c(rotulo, 'Frequência', 'Frequencia relativa', 'Porcentagem')  
  
  knitr::kable(tab, digits = 2, format.args = list(decimal.mark = ","),
               align = rep("c", 4), format = 'pipe', caption = legenda)
}

#' Concatenating strings
#' 
#' @inherit 
#' 
#' @examples 
#' "string a" %colar% "string b"
`%colar%` <- function(vetor1, vetor2) paste0(vetor1, vetor2)

#' Graphs
#' 
#' \code{grafico} builds the graph for a variable.
#' 
#' @param df a tibble 
#' @param variable a scalar character 
#' @param rotulo label of y axis
#' @param na_rm remove rows with NA values
#' 
#' @return a tibble that is distribution table
#' 
#' @usage grafico(df, variable, rotulo)
#' 
#' @examples 
#' n <- 1000
#'df <- tibble(nome = sample(c('A', 'B', 'C'), size = n, replace = T,
#'                           prob = c(0.6, 0.2, 0.2)),
#'            numero = rnorm(n))
#'table_distribution(df, 'nome', 'Nome')
grafico <- function(df, variable, rotulo, na_rm = TRUE) {

  if(na_rm) {
    df_clean <- df %>% filter(.[[variable]] != "")
  } else {
    df_clean <- df 
  }
  
  
    df_clean %>%
      ggplot(aes_string(x = variable)) +
      geom_bar(aes(y = ..prop.. * 100, group = 1), fill = "blue") +
      labs(x = rotulo, y = "Porcentagem") +
      theme(axis.title = element_text(size = 20),
            axis.text = element_text(size = 15)) 
  
}
