## Verifica a necessidade de instalar o pacote do Excel
if (!require(xlsx)) {
  install.packages('xlsx');
}
library(xlsx);

## Verifica a necessidade de instalar o pacote do Tools
if (!require(tools)) {
  install.packages(tools);
}
library(tools);

## Faz o parser dos arquivos CVS gerados pelo JMeter a partir de um determinado diretório. 
## O padrão para percental máximo de requisições com erro é de 15%. O número padrão de requisições
## com o maior tempo de resposta é 5.
parseFile <- function(directory, maxPercentageError=15, numOfSlowRequest=5) {

  resultColumns <- c("sampler_label",
                     "aggregate_report_count", 
                     "aggregate_report_error.",
                     "average",
                     "aggregate_report_min",
                     "aggregate_report_max");
  
  if (!is.null(directory) & file.exists(directory)) {
    setwd(directory);
    
    test <- list_files_with_exts(directory,"csv");
    for(i in 1:length(test)) {
      currentFile <- test[i];
      targetFileName <- paste0("test-result-",i,".xlsx");
      result <- read.csv(currentFile);
      
      ## Selecionando as colunas para apresentar o resultado
      result <- result[resultColumns];
      
      ## Selecionando a linha do totalizador para uso futuro e removendo o registro dos dados
      total <- result[nrow(result),];
      result <- result[-c(nrow(result)),];
      
      if (total$aggregate_report_error. > maxPercentageError) {
        warning(paste0("O percentual máximo de requisições com erro foi ultrapassado. Limite: ",
                       maxPercentageError," Aferido: ", total$aggregate_report_error.));
      }
      
      ## Ordenando as requisições por tempo de resposta
      index <- with(result, order(result$average, decreasing = TRUE));
      result <- result[index,];
      result <- rbind(result,total);
  
      message(paste0(numOfSlowRequest, " Requisições com o maior tempo de resposta do arquivo: ",currentFile));
      for (i in 1:numOfSlowRequest) {
        message(paste0(i," ",result[i,]$sampler_label," ",result[i,]$average));
      }
  
      colnames(result) <- c("Destino","Qtde Requisições","% de Erro","Média","Mínimo","Máximo");
      
      if (file.exists(targetFileName)) {
        file.remove(targetFileName);
      }
      
      write.xlsx(result,targetFileName,row.names = FALSE);
    }
  } else {
    stop(paste0("O diretório de trabalho não existe: "),directory);
  }
}