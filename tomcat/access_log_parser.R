## Verifica a necessidade de instalar o pacote do Excel
if (!require(ggplot2)) {
  install.packages('ggplot2');
}
library(ggplot2);

parse_access_log <- function(filename) {
  log <- read.table(filename);
  
  colnames(log) <- c("host","date", "gmt", "port", "type", "request", "http", "status", "hash", "duration");

  # Removendo o caracter [ do campo data e transformando no tipo Date
  fixDate <- function (date) {
    stringDate <- gsub("[\\[]","",date);
    result <- as.Date(stringDate, "%d/%B/%Y:%H:%M:%OS2");
    return (result);
  }
  
  log$date <- lapply(log$date, fixDate);

  log[12] <- NULL;
  log[11] <- NULL;
  log[3] <- NULL;

  head(log);
  
  p <- qplot(x = request , y = duration, data=log );
  p <- p + geom_line();

  png("requests-duration-by-time.png");
  dev.off();
}