#' @title Intervalo Bootstrap para vendas
#'
#' @description
#' Função recebe um vetor com vendas por categorias e retorna o intervalo de bootstrap, para a média
#' de vendas para cada uma das regiões de vendas.

#' @param n  numero de reamostragem bootstrap.
#' @param amostra amostra de vendas de uma região, argumento deve ser um vetor.
#'
#' @returns Um vetor contendo um intervalo superior e inferior de 95% e a mediana.
#'
#' @export
#'
#' @examples
#' IBoots_Vendas(100, as.data.frame(Dados)$NA_Sales)
#'
IBoots_Vendas <- function(n, amostra){

  tam_amostra <-length(amostra)
  matrix_boot <- matrix(sample(amostra, size = n*tam_amostra, replace = T) ,
                        ncol = n , nrow = tam_amostra)

  esta_boot <- cbind(quantile(colMeans(matrix_boot) , prob = 0.025),
                    quantile(colMeans(matrix_boot) , prob = 0.50),
                    quantile(colMeans(matrix_boot) , prob = 0.975))
  return(esta_boot)
}

