calculate_metrics <- function(conf_matrix, class1, class2) {
  
  # Certificar-se de que a matriz de confusão está correta
  conf_matrix <- as.table(conf_matrix)
  
  # Calcular as métricas para class1 como classe positiva
  TP1 <- conf_matrix[class1, class1]
  TN1 <- conf_matrix[class2, class2]
  FP1 <- conf_matrix[class1, class2]
  FN1 <- conf_matrix[class2, class1]
  
  precision1 <- TP1 / (TP1 + FP1)
  recall1 <- TP1 / (TP1 + FN1)
  f1_score1 <- 2 * (precision1 * recall1) / (precision1 + recall1)
  
  # Calcular as métricas para class2 como classe positiva
  TP2 <- conf_matrix[class2, class2]
  TN2 <- conf_matrix[class1, class1]
  FP2 <- conf_matrix[class2, class1]
  FN2 <- conf_matrix[class1, class2]
  
  precision2 <- TP2 / (TP2 + FP2)
  recall2 <- TP2 / (TP2 + FN2)
  f1_score2 <- 2 * (precision2 * recall2) / (precision2 + recall2)
  
  # Calcular a Accuracy global
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  
  # Retornar os resultados como uma lista
  return(list(
    Precision_class1 = round(precision1, 4),
    Recall_class1 = round(recall1, 4),
    F1_Score_class1 = round(f1_score1, 4),
    
    Precision_class2 = round(precision2, 4),
    Recall_class2 = round(recall2, 4),
    F1_Score_class2 = round(f1_score2, 4),
    
    Accuracy = round(accuracy, 4)
  ))
}