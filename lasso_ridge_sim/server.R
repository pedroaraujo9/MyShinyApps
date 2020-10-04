library(shiny)
library(tidyverse)

#' Função que gera os dados para o exercício
#'
#' @param n número de observações
#' @param p número de variáveis explicativas (são p+1 parâmetros + sigma)
#' @param p1 número de variáveis com efeito (beta) igual a 1
#' @param sigma desvio padrão do erro
#'
#' @return data.frame com a resposta (y) e as covariáveis X1, X2, .., Xp
#'
gera_dados = function(n, p, p1, sigma) {
    Beta = matrix(
        c(1, rep(1, p1), rep(0, p-p1)),
        nrow = p + 1,
        ncol = 1
    )
    
    x = replicate(n = p, rnorm(n))
    X = cbind(1, x)
    y = as.numeric(X%*%Beta) + rnorm(n, 0, sigma)
    
    data.frame(y, X[,-1])
    
}

calcula_erro = function(modelo, x, y) {
    #matriz com predições para cada lambda considerado no modelo
    predito = predict(modelo, newx = x)
    #erro para cada coluna dos preditos
    erro = apply(predito, MARGIN = 2, y=y, FUN = function(pred, y){
        mean((pred - y)^2)
    })
    #vetor com erros
    erro
}


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    
    erro_lasso = reactive({
        n = input$n
        p = input$p
        p1 = input$p1
        sigma = input$sigma
        
        df = gera_dados(n, p, p1, sigma)
        #separando treino e validação para o lasso
        index_treino = sample(1:n, floor(0.7*n))
        
        x_treino = df[index_treino, -1] %>% as.matrix()
        x_teste = df[-index_treino, -1] %>% as.matrix()
        
        y_treino = df[index_treino, 1] %>% as.numeric()
        y_teste = df[-index_treino, 1] %>% as.numeric()
        
        
        lasso = glmnet(x = x_treino, y = y_treino, alpha = 1, 
                       family = "gaussian", lambda = lambda_grid_lasso)
        
        #modelos regularizados 
        lambda_grid_lasso = seq(0, input$lambda_max, 0.1)
        
        lasso = glmnet(x = x_treino, y = y_treino, alpha = 1, 
                       family = "gaussian", lambda = lambda_grid_lasso)
        
        
        #sum |beta|
        sum_Beta = lasso %>% coef() %>% as.matrix() %>%
            .[-1,] %>% #retirando intercepto da conta da soma dos coeficientes
            abs() %>% #aplicando o módulo
            colSums() #somando coefs
        
        erro_treino = calcula_erro(lasso, x_treino, y_treino)
        erro_teste = calcula_erro(lasso, x_teste, y_teste)
        erro_treino_regularizado = erro_treino + sum_Beta
        num_variaveis = data.frame(nvar = lasso[[3]], lambda = rev(lambda_grid_lasso))
        
        erro_lasso_df = data.frame(
            erro_treino, erro_teste, erro_treino_regularizado, 
            lambda = rev(lambda_grid_lasso), nvar = num_variaveis$nvar
        ) 
        
        erro_lasso_df
        
    })
    
    
    
    output$lasso_lambda = renderPlot({
        erro_lasso() %>%
            select(-nvar) %>%
            gather(tipo, erro, -lambda) %>%
            mutate(tipo = str_replace_all(tipo, "_", " ")) %>%
            ggplot(aes(x=lambda,y=erro,col=tipo)) +
            geom_line() + 
            labs(x=expression(lambda), y="Erro", col='Tipo:') + 
            theme(legend.position = "top")
    })
    
    output$lasso_nvar = renderPlot({
        erro_lasso() %>%
            select(-lambda) %>%
            gather(tipo, erro, -nvar) %>%
            mutate(tipo = str_replace_all(tipo, "_", " ")) %>%
            ggplot(aes(x=nvar,y=erro,col=tipo)) +
            geom_line() + 
            geom_vline(xintercept = input$p1, col = 'red', alpha = 0.4, linetype = 2) + 
            labs(x = "Número de variáveis selecionadas", y = "Erro", col="Tipo") + 
            theme(legend.position = "top")
    })
    
    
    
})
