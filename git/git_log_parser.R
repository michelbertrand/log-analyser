# 
# Faz o parse do git log e cria 4 variaveis a partir do comando abaixo:
# git --no-pager log --pretty=format:'%H%x09%an%x09%ai%x09%s' --grep="subject"
#
# branchWorkDir o diretorio de trabalho da branch onde os commits serao localizados
# subject a descricao do commit a ser localizado
# filename processo o git log a partir de um arquivo, por exemplo, git --no-pager log --pretty=format:'%H%x09%an%x09%ai%x09%s' --grep="subject" > c:\temp\git.log
#
parse_git_log <- function(subject, branchWorkDir, filename=NULL, noMerges=FALSE) {
  
  cleanSingleQuotes <- function(string) {
    return (gsub("([\'])","", string));
  }

  lastCommit <- 1;
  
  diffToSearch <- character(0);  
  
  commits <- character(0);
  
  log <- data.frame(hash=character(0),
                    author=character(0),
                    date=character(0),
                    subject=character(0),
                    stringsAsFactors=FALSE);
  
  logColumns <- c("hash","author","date","subject");
  
  searchResult <- data.frame(hash=character(0),
                             author=character(0),
                             date=character(0),
                             subject=character(0),
                             file=character(0),
                             stringsAsFactors=FALSE);
  
  resultColumns <- c("hash","author","date","subject","file");
  
  # Define o diretorio de trabalho para o local de clone da branch
  setwd(branchWorkDir);

  print(paste0("Processando o git log de ",branchWorkDir," e buscando por ",subject," ..."));
  if (is.null(filename)) {
    if (!noMerges) {
      gitLogResult <- system(paste0("git --no-pager log --pretty=format:'%H%x09%an%x09%ai%x09%s' --grep=\"",subject,"\""), intern = TRUE, wait = TRUE);
    } else {
      gitLogResult <- system(paste0("git --no-pager log --no-merges --pretty=format:'%H%x09%an%x09%ai%x09%s' --grep=\"",subject,"\""), intern = TRUE, wait = TRUE);
    }  
    Encoding(gitLogResult) <- "UTF-8";
    for (i in 1:length(gitLogResult)) {
      tmp <- unlist(strsplit(gitLogResult[i],"\t"));
      log[i,1] <- cleanSingleQuotes(tmp[1]);
      log[i,2] <- tmp[2];
      log[i,3] <- tmp[3];
      log[i,4] <- cleanSingleQuotes(tmp[4]);
    }
  } else {
    log <- read.table(filename, sep = "\t", quote = "", fill=TRUE, encoding="UTF-8", 
                      comment.char = "", stringsAsFactors = FALSE);
  }
  
  colnames(log) <- logColumns;
  
  #dim(log);
  #str(log);
  #head(log);

  for (i in 1:nrow(log)) {
    commit <- log[i,];

    #Obter os arquivos alterados destes commits
    #git diff-tree --no-commit-id --name-only -r HASH
    diff <- system(paste0("git diff-tree --no-commit-id --name-only -r ",commit$hash), intern = TRUE, wait = TRUE); 
    diffToSearch <- c(diffToSearch,diff);
  }
  
  numberOfChanges <- 0;
  
  #Procurar por commits posteriores que alteraram os mesmos arquivos    
  for (y in 1:length(diffToSearch)) { 
    target <- diffToSearch[y]; 
    
    #Pesquisando a lista de commits encontrados na branch de trabalho
    changes <- system(paste0("git log --pretty=format:'%H%x09%an%x09%ai%x09%s' -- ",target), intern = TRUE);

    for (z in 1:length(changes)) {
      tmp <- unlist(strsplit(changes[z],"\t"));
      
      if (any(grepl(tmp[1],searchResult[,1]))) {
        next;
      }
      
      numberOfChanges <- numberOfChanges + 1; 
      searchResult[numberOfChanges,1] <- tmp[1];
      searchResult[numberOfChanges,2] <- tmp[2];
      searchResult[numberOfChanges,3] <- tmp[3];
      searchResult[numberOfChanges,4] <- tmp[4];
      searchResult[numberOfChanges,5] <- target;
    } 
  }
  
  # Adiciona os nomes das colunas no data frame do resultado
  colnames(searchResult) <- resultColumns;   

  # Commits a serem investigados
  resultSize <- nrow(searchResult);
  if (resultSize > 1) {
    for (w in 1:resultSize) {
      message(paste0("Verificar commit: ",searchResult[w,1]," Author: ",searchResult[w,2]," Arquivo: ",searchResult[w,5]," 
                     Alterado em: ",searchResult[w,3],"\n"));
    }
  } else if (resultSize == 1) { 
    result <- searchResult[1,];
    message(paste0("verificar o commit: ", result$hash, "Author: ",searchResult$author," Arquivo: ",searchResult$file," \n"));
  } else { 
    message("Nenhuma alteracao em comum foi encontrada nos commits.");
  }
  
}