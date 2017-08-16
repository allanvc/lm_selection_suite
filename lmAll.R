# utf - 8
# projeto suite de seleção de modelos 
# função lmAll() ajusta todos os modelos lineares possíveis para um dataframe de covariaveis (cov.df)...
# ... e uma variavel resposta (y)

lmAll<-function(y, cov.df){
  # para saber quantas covariáveis existem:
  p <- dim(cov.df)[2]
  names(cov.df) <- paste("x", c(1:p), sep="")
  
  # extraindo as covariáveis como vetores
  for ( i in (1:p)){
    assign(paste("x",i, sep=""), cov.df[,i])
  }
  #str(x1)
  
  ## encontrando todos os modelos possíveis:
  #help(combn)
  
  # criando lista de modelos:
  nomes_mod.l <- list()
  res_mod.l <- list()
  aov_mod.l <- list()
  
  
  i = 0
  while(i <= p){ # 'i' conta o número de covariáveis que entrará no modelo
    # 'j' controla a posição nas listas que salvam os resultados
    if (i == 0){ # caso do modelo sem covariável
      j = 1 
      nomes_mod.l[[j]] <- "lm(y~1)"
      res_mod.l[[j]] <- lm(y~1)
      aov_mod.l[[j]] <- anova(res_mod.l[[j]])
      j = j+1
      i = i+1
    }
    
    comb <- combn(1:p,i)
    comb <- matrix(paste("x", comb, sep=""), nrow=nrow(comb))
    #i=3
    
    for(c in 1:ncol(comb)){ # c percorre as colunas da matriz de combinações
      nomes_mod.l[[j]] <- paste("lm(y~", paste(comb[,c], collapse = "+"), ")", sep="" )
      # temos que deixar uma string completa para depois usar o eval(parse)
      # se for picotado, por exemplo tentando retirar cada variável não funciona
      
      # nomes_comb <- paste ("comb[", 1:i,",", c, "]", sep="")
      # nomes_comb_mais <- paste(nomes_comb, collapse ="+")
      res_mod.l[[j]] <- eval(parse(text=nomes_mod.l[[j]]))
      
      aov_mod.l[[j]] <- anova(res_mod.l[[j]])
      
      
      j=j+1
    }
    i = i+1
  }
  return(list(nomes_mod.l, res_mod.l, aov_mod.l) )
}


# melhor apresentar o summary da lm()

# rm(res_mod.l, nomes_mod.l, aov_mod.l)
resultados <- lmAll(y, cov.df)
