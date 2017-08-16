# utf - 8
# projeto suite de seleção de modelos 
# função fwdSelect() realiza a seleção autmática de modelos lineares utilizando o método stepwise COM P VALOR
#... recebe um data frame de covariáveis (cov.df), um vetor de variável resposta (y) e o alpha a ser
#.. utilizado na comparação


####################################
## ideia já para modificar:
#para o stepwise, o importante é pegar os pvalores do summary(lm()) porque ele testa
#... cada um contra todos.

# ma lmAll() poderíamos usar esse summary tb nos testes


###################################
#outra ideia - colocar todas as funções de seleção dentro de uma única e deixar que o usuária decida o método...
#..... informando o parâmetro


fwdSelect<-function(y, cov.df, alpha){
  
  mod.select<-list()
  aov.mod.select<-list()
  
  p <- dim(cov.df)[2]
  #names(cov.df) <- paste("x", c(1:p), sep="")
  
  # extraindo as covariáveis como vetores
  for ( i in (1:p)){
    assign(paste("x",i, sep=""), cov.df[,i])
  }
  #str(x1)
  
  vars <- 1:p # será usado na seleção
  
  # teste para uma variável:
  
  mod_parcial.l<-list()
  aov_mod_parcial.l<-list()
  
  vars.nome <- paste("x", vars, sep="")
  
  for(k in vars){
    nome_mod <- paste("lm(y~", vars.nome[k], ")", sep="" )
    # temos que deixar uma string completa para depois usar o eval(parse)
    # se for picotado, por exemplo tentando retirar cada variável não funciona
    
    mod_parcial.l[[k]] <- eval(parse(text=nome_mod))
    
    aov_mod_parcial.l[[k]] <- anova(mod_parcial.l[[k]])
  
  }
  
  # comparando pvalores
  pvalor_simp.v <- sapply(aov_mod_parcial.l, function(x){
    x$`Pr(>F)`[1]
  })
  
  #alpha = 0.1
  if(any(pvalor_simp.v < alpha)){
    var.select <- which.min(pvalor_simp.v) # para cada avaliação nossa var.select irá aumentar
    
    i=1
    
    # SEPARAR EM ANOVA E MODELO
    mod.select[[i]] <- mod_parcial.l[[var.select]]
    
    aov.mod.select[[i]] <- aov_mod_parcial.l[[var.select]]
  
    if (p > 1 ){ # se tiver pelo menos duas variaveis a selecionar
      i = 2 # já vai para o teste de duas variáveis
      for(i in (2:p)){ # conta o numero de variaveis a selecionar, vou precisar disso sim!
        # i = 3
        #vars.to.test <- vars[vars != var.select]
        vars.to.test <- vars[!(vars %in% var.select)] # tem que ser assim para indexar depois que var.select
        #.. vira um vetor
        
        vars.to.test.nome <- paste("x", vars.to.test, sep="")
        
        j = 1
        
        mod_parcial.l <- list()
        aov_mod_parcial.l <- list()
        
        var.select.nome <- paste("x", var.select, sep="" )
        
        
        for(c in 1:length(vars.to.test)){ # c percorre os elementos do vetor
          
          
          nome_mod <- paste("lm(y~", paste(paste(var.select.nome, collapse="+"), vars.to.test.nome[c], sep = "+") , ")", sep="" )
          # usamos ainda o collapse, mas de forma diferente
          # temos que deixar uma string completa para depois usar o eval(parse)
          # se for picotado, por exemplo tentando retirar cada variável não funciona
          
          mod_parcial.l[[j]] <- eval(parse(text=nome_mod))
          aov_mod_parcial.l[[j]] <- anova(eval(parse(text=nome_mod)))
          
          
          j=j+1
        } # ok!
        
        # i=3
        # agora pegar os p-valores
        pvalor_parcial.v <- sapply(aov_mod_parcial.l, function(x){
          # se não colocar o parêntese no (p+1), vai indexar errado
          x$`Pr(>F)`[i]
          # sempre da última variável que é o que está sendo testada -- usamos o contador de variáveis i
        })
        
        # selecionar o melhor:
        if(any(pvalor_parcial.v < alpha )){
          # var.select <- c(4,1)
          var.select <- c(var.select, vars.to.test[which.min(pvalor_parcial.v)] ) # para cada avaliação nossa var.select irá aumentar
          
          mod.select[[i]] <- mod_parcial.l[[which.min(pvalor_parcial.v)]] 
          
          aov.mod.select[[i]] <- aov_mod_parcial.l[[which.min(pvalor_parcial.v)]] 
          # vou salvar todos os modelos ajustados e depois retorno só o último
          
          
        } else {
          #i = p+1 # sai do loop de seleção de variáveis
          return( list(summary(mod.select[[length(mod.select)]]), aov.mod.select[[length(aov.mod.select)]]) )
        }
      }    

    } else {
      return( list(summary(mod.select[[length(mod.select)]]), aov.mod.select[[length(aov.mod.select)]]) )
    }    
  } else {
    return(c("Regression is not significant."))
  }
}    
 
# poderíamos colocar opções para olhar para o p-valor ou para o R2 ou AIC separando em
#.. funções distintas que chamaríamos de dentro da fwdSelection()

fwdSelection(y, cov.df, 0.05)     
# parece que funcionou!!!
      
fwdSelection(y, cov.df, 0.1)     

fwdSelection(y, cov.df, 0)     

fwdSelection(y, cov.df, 0.3)     

cov.df2<-cov.df[,c(4,3,2,1)]      

fwdSelection(y, cov.df2, 0.05)      
# legal, parece funcionar mesmo! Ao invertermos x4 e x1 o resultado é exatamente o inverso, 
#..como esperado!

      