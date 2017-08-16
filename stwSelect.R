# utf - 8
# projeto suite de seleção de modelos 
# função stepSelect() realiza a seleção autmática de modelos lineares utilizando o método stepwise COM P VALOR
#... recebe um data frame de covariáveis (cov.df), um vetor de variável resposta (y) e o alpha a ser
#.. utilizado na comparação

####################################
## ideia já para modificar:
#para o stepwise, o importante é pegar os pvalores do summary(lm()) porque ele testa
#... cada um contra todos.

# ma lmAll() poderíamos usar esse summary tb nos testes


stwSelect<-function(y, cov.df, alpha){
  
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
  
  #flag = 0
  
  #help(exists)
  #while(flag != 1){ # grande laço de seleção
  if( !exists("var.select") ){ # se não existir ainda a variável de seleção é pq está no primeiro estágio
    l = 2 # servirá para contar a aprtir de quantas variáveis deverá começar o próximo loop
    # no caso de estarmos partindo da primeira verificação, l será = 2
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
      
    #alpha = 0.05
    if(any(pvalor_simp.v < alpha)){
      var.select <- which.min(pvalor_simp.v) # para cada avaliação nossa var.select irá aumentar
        
      i=1
        
      # SEPARAR EM ANOVA E MODELO
      mod.select[[i]] <- mod_parcial.l[[var.select]]
      
      aov.mod.select[[i]] <- aov_mod_parcial.l[[var.select]]
      
    } else {
      return(c("Regression is not significant."))
    } # fim if verif. pvalores para uma var.
  }    
      
      ## teste para mais variáveis:
      if (p > 1 ){ # se tiver pelo menos duas variaveis a selecionar
        i = 2
        # tivemos que mudar o loop principal para while para podermos controlar o valor de 'i'
        while(i <= p){ # conta o numero de variaveis a selecionar, vou precisar disso sim!
          print(i)
          # i = 3
          #vars.to.test <- vars[vars != var.select]
          vars.to.test <- vars[!(vars %in% var.select)] # tem que ser assim para indexar depois que var.select
          #.. vira um vetor
          
          vars.to.test.nome <- paste("x", vars.to.test, sep="")
          
          j = 1
          
          mod_parcial.l <- list()
          aov_mod_parcial.l <- list()
          
          var.select.nome <- paste("x", var.select, sep="" )
          
          # ajustando os modelos a serem testados
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
            # aqui estamos guardando o que foi selecionado em cada passo, caso decidamos retornar
              
            aov.mod.select[[i]] <- aov_mod_parcial.l[[which.min(pvalor_parcial.v)]] 
            # vou salvar todos os modelos ajustados e depois retorno só o último
              
          } else {
            return(list(summary(mod.select[[length(mod.select)]]), aov.mod.select[[length(aov.mod.select)]]) )
          } # final do laço de seleção para pvalores anova significantes
          
          
          # passo stepwise -- verificar se alguma pode ser retirada:
          pvalor_t.v <- summary(mod.select[[i]])[[4]][-1,4] # elimino o intercepto e pego o resto
          
          if(any(pvalor_t.v > alpha )){ # se algum não for significativo
            var.sai.nome <- names(pvalor_t.v[which.max(pvalor_t.v)])
            
            #help(grep)
            var.sai <- as.numeric(gsub("x","",var.sai.nome))
            
            # atualizar de novo a var.select -- retirando a variavel:
            # var.select = c(4,1,2)
            var.select <- var.select[var.select!=var.sai]
            
            
            # voltando no modelo:
            
            var.select.nome <- paste("x", var.select, sep="" )
            nome_mod <- paste("lm(y~", paste(paste(var.select.nome, collapse="+"), ")", sep="" ), sep="")
            
            mod.select[[i]] <- eval(parse(text=nome_mod))
            # aqui estamos guardando o que foi selecionado em cada passo, caso decidamos retornar
            
            aov.mod.select[[i]] <- anova(mod.select[[i]])
            # vou salvar todos os modelos ajustados e depois retorno só o último
            
            # o ideal talvez seria chamar recursivamente uma função do loop -- desse jeito que eu fiz não dá
            
            #se entrou nesse if, temos que voltar o contador l:
            i = length(var.select)+1
          } else {
            i = i +1 # se nao houver variável a retirar, atualiza o contador para ficar mais próximo de sair do laço while
          }
      
        }# final do laço for de seleção 
        
      } else { # se não tiver mais de uma variável no cov.df
        #i = p+1 # sai do loop de seleção de variáveis
        return( list(summary(mod.select[[length(mod.select)]]), aov.mod.select[[length(aov.mod.select)]]) )
      } 
    return( list(summary(mod.select[[length(mod.select)]]), aov.mod.select[[length(aov.mod.select)]]) )
  }
