#' @title Intervalo de confiança para vendas
#'
#' @description ´
#' Função recebe um vetor com vendas por categorias e retorna o intervalo de confiança bootstrap, para a média
#' de vendas para cada uma das categorias.

#' @param x  um vetor que contém número de vendas
#' @param alpha Nível de significancia do intervalo de confiança.
#'
#' @returns Um vetor  contendo , intervalo superior e inferior do ic de vendas, e a media para cada categoria.
#'
#' @export
#'

ic_medias <- function(n, amostra){

  tam_amostra <-length(amostra)
  matrix_boot <- matrix(sample(amostra, size = n*tam_amostra, replace = T) ,
                        ncol = n , nrow = tam_amostra)
  esta_boot <- cbind(quantile(colMeans(matrix_boot) , prob = 0.25),
                    quantile(colMeans(matrix_boot) , prob = 0.50),
                    quantile(colMeans(matrix_boot) , prob = 0.75))
  return(esta_boot)
}

