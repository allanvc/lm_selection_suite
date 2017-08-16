# utf - 8
# projeto suite de seleção de modelos 
# função bwdSelect() realiza a seleção autmática de modelos lineares utilizando o método backward COM P VALOR
#... recebe um data frame de covariáveis (cov.df), um vetor de variável resposta (y) e o alpha a ser
#.. utilizado na comparação

bwdSelect<-function(y, cov.df, alpha){
  
  # mod.select<-list()
  # aov.mod.select<-list()
  
  p <- dim(cov.df)[2]
  #names(cov.df) <- paste("x", c(1:p), sep="")
  
  # extraindo as covariáveis como vetores
  for ( i in (1:p)){
    assign(paste("x",i, sep=""), cov.df[,i])
  }
  #str(x1)
  
  vars <- 1:p # será usado na seleção
  
  # ajustando modelo com todas as variáveis:
  
  mod_parcial.l<-list()
  aov_mod_parcial.l<-list()
  
  vars.nome <- paste("x", vars, sep="")
  
  var.fica <- vars
  
  var.sai = 0

  # vamos repetir o modelo completo, só para termos um retorno no caso em que temos apenas 1 variável
  # não precisa testar se há mais de uma variável!!
  
  i = 1
  for( k in (p:1)){ # faremos uma contagem ao contrário
      
    var.fica <- vars[!vars%in%var.sai]
    # var.fica<-vars[!c(1,2,3,4)]
    if( length(var.fica) == 0 ){ # para retornar que a regressão não é significante -- ou seja,
      #... qdo todas as variáveis devem sair do modelo
      return("Regression is not significant.")
    }
        
    var.fica.nome <- paste("x", var.fica, sep="")     
    # k =4  
      
    nome_mod <- paste("lm(y~", paste(var.fica.nome, collapse = "+"), ")", sep="" )
      
    mod_parcial.l[[i]] <- eval(parse(text=nome_mod))
      
    aov_mod_parcial.l[[i]] <- anova(mod_parcial.l[[i]])
      
    # comparando pvalores para ver quem sai
    pvalor_parcial.v <- aov_mod_parcial.l[[i]]$`Pr(>F)`[-length(aov_mod_parcial.l[[i]]$`Pr(>F)`)]
    # fazemos menos length para retirar o NA que vem dos resíduos
      
    #alpha = 0.05
    if(any(pvalor_parcial.v > alpha)){ # agora buscamos maiores que alpha
      var.sai <- c(var.sai, which.max(pvalor_parcial.v)) # para cada avaliação nossa var.select irá aumentar
      i = i+1 # atualiza índice da lista para salvar os modelos
        
    } else { # caso nenhum seja significativo
      return(list( mod_parcial.l[[length(mod_parcial.l)]], aov_mod_parcial.l[[length(aov_mod_parcial.l)]]))
    }
      
  }  
    
}