# utf - 8
# projeto suite de seleção de modelos 
# função lmAllCrit() pega a saída da função lmAll() - lista e calcula alguns critérios para
#... seleção 'manual' de modelos

lmAllCrit <- function(lm.all.l){
  #help(lapply)
  #j = 1 # controla para qual modelo estamos calculando os resultados
  R2 <- c()
  R2a <- c()
  C <- c()
  AIC <- c()
  AICc <- c()
  BIC <- c()
  
  # pegando um vetor de soma de quadrados:
  SQRes.v <- sapply(lm.all.l[[3]], function(x){
    x$`Sum Sq`[length(x$`Sum Sq`)] # a última entrada dos SQRes sempre são os resíduos
  })
  # ok!
  
  # pegando um vetor de quadrados médios:
  MSRes.v <- sapply(lm.all.l[[3]], function(x){
    x$`Mean Sq`[length(x$`Mean Sq`)] # a última entrada dos SQRes sempre são os resíduos
  })
  # ok!
  
  # OBS: para a seleção AUTOMÁTICA de modelos, provavelmente usaremos a mesma técnica de indexação
  
  # precisaremos retornar o numero de covariaveis em cada modelo
  # na verdade, da para calcular usando a length do retorno da tabela da ANOVA pro SQRes
  
  p.v <- sapply(lm.all.l[[3]], function(x){
    length(x$`Sum Sq`)-1 # o número de variáveis é o tamanho menos 1
  })
  
  SQT = SQRes.v[1]
  MST = MSRes.v[1]
  MSRes.comp <- MSRes.v[length(MSRes.v)] # MSRes do modelo completo a ser utilizado no C de Mallows
  mod.comp <- as.data.frame(lm.all.l[[3]][length(lm.all.l[[3]])]) # para depois pegarmos os graus de liberdade
  
  n = sum(mod.comp$Df) +1
  
  for (j in 1:length(lm.all.l[[3]])){
    #j=1
    
    # revisar as fórmulas pelos slides da aula:
    R2[j] <- 1-(SQRes.v[j]/SQT)
    
    R2a[j] <- 1 - (MSRes.v[j]/MST)
    
    C[j] <- (SQRes.v[j]/MSRes.comp)  - (n-2*p.v[j]-2)
    
    AIC[j] <- n*log(SQRes.v[j]) - n*log(n) + 2*(p.v[j]+1 )
    
    AICc[j] <- AIC[j] + (2*(p.v[j]+1)*(p.v[j]+2))/( n-p.v[j]-2)  
    
    BIC[j] <- n*log(SQRes.v[j]) - n*log(n) + (p.v[j]+1) * log(n)
    
    # PRESS a implementar
    
  }
  # juntando tudo em um data frame:
  #help(Reduce)
  nome.mod <- Reduce('c', lm.all.l[[1]])
  res_crit.df <- data.frame("modelo" = nome.mod, "R2" = R2, "R2a" = R2a,
                            "C_Maslow" = C, "AIC" = AIC, "AICc" = AICc,
                            "BIC" = BIC)
  return(res_crit.df)
  
}

# MELHOR USAR UMA LM.ALL AQUI DENTRO, AÍ A FUNÇÃO FICA MENOS AMARRADA
#... E PODEMOS CALCULAR UMA MATRIZ DE CORRELAÇÃO E DAR UM WARNING PARA MULTICOLINEARIDADE


# podemos acrescentar depois uma sugestão do melhor modelo