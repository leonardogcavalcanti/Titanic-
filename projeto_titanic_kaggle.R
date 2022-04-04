#DICIONÁRIO DE DADOS ------

#VARIÁVEIS:

#Survived (Sobreviveu): 0 = Não, 1 = Sim
#Pclass (Classe): Classe de ingresso 1 = 1º, 2 = 2º, 3 = 3º
#Sex (Sexo): Sexo do passageiro
#Age (Idade): Idade em anos
#Sibsp: Quantidade de irmãos / cônjuges a bordo do Titanic
#Parch: Quantidade de pais / crianças a bordo do Titanic
#Ticket (Bilhete): Número do bilhete de embarque
#Fare (Tarifa/Valor pago): Tarifa paga pelo Passageiro
#Cabin (Cabine): Número de cabine
#Embarked (Local de Embarque): Porto de Embarque (C = Cherbourg, Q = Queenstown, S = Southampton)


# Pacotes --------
install.packages("tidyverse")
library("tidyverse")


# Wrangling ---------------------------------------------

head(gender_submission) # Serve apenas como um modelo de exemplo para exportar no Kaggle

# Mudar os nomes para facilitar

# Tabela de teste
names(test)
teste <- test %>% rename(id_passageiro = 1,
                         tipo_classe = 2,
                         nome = 3,
                         sexo = 4,
                         idade = 5,
                         irmaos_conjuges = 6,
                         pais_criancas = 7,
                         bilhete = 8,
                         valor_pago = 9,
                         cabine = 10,
                         local_embarque = 11) 

names(teste)

# Tabela de treino
names(train)
treino <- train %>% rename(id_passageiro = 1,
                           sobreviveu = 2,
                           tipo_classe = 3,
                           nome = 4,
                           sexo = 5,
                           idade = 6,
                           irmaos_conjuges = 7,
                           pais_criancas = 8,
                           bilhete = 9,
                           valor_pago = 10,
                           cabine = 11,
                           local_embarque = 12)

names(treino)

# Substituir o elemento 0 para N(não) e 1 para Y(sim)

str(treino)

treino <- mutate(treino,
                 sobreviveu = replace(sobreviveu, sobreviveu == 0, 'N'),
                 sobreviveu = replace(sobreviveu, sobreviveu == 1, 'Y'))


colSums(is.na(treino))
colSums(is.na(teste))
colSums(treino=='')
colSums(teste=='')

# Criando uma coluna para identificar se o dado é treino ou teste --------------

# Definir treino_log como TRUE se o elemento for do dataset "treino"

treino$treino_log <- TRUE  # É do conjunto do dataset treino? SIM

# Definir treino_log como FALSE se o elemento for do dataset teste

teste$treino_log <- FALSE  # É do conjunto treino? NÃO

# Agrupando dataset 

titanic_set <- treino %>% bind_rows(teste)
table(titanic_set$treino_log) # 418 de teste e 891 de treino
str(titanic_set)

# Descritiva do conjunto -------------------------------

summary(titanic_set)
table(is.na(titanic_set$sobreviveu))
table(is.na(titanic_set$sexo))
table(is.na(titanic_set$irmaos_conjuges))
table(is.na(titanic_set$pais_criancas))
table(is.na(titanic_set$local_embarque))
table(titanic_set$local_embarque)


# Limpando dados faltantes  ---------------------------------------

# Mudando a variável "sobreviveu" para classe factor
titanic_set$sobreviveu <- as.factor(titanic_set$sobreviveu)
# Mudando a variável "tipo_classe" para classe factor
titanic_set$tipo_classe <- as.factor(titanic_set$tipo_classe)
# Mudando a variável "sexo" para classe factor
titanic_set$sexo <- as.factor(titanic_set$sexo)
# Tirando a mediana da variável "idade" e substituindo as NA pela mediana
titanic_set$idade[is.na(titanic_set$idade)] <- median(titanic_set$idade, na.rm = T)
# Confirmando a variável "irmaos_conjuges" na classe numérica
titanic_set$irmaos_conjuges <- as.numeric(titanic_set$irmaos_conjuges)
# Confirmando a variável "pais_criancas" na classe numérica
titanic_set$pais_criancas <- as.numeric(titanic_set$pais_criancas)
# Tirando a mediana da variável "valor_pago" e substituindo as NA's pela mediana
titanic_set$valor_pago[is.na(titanic_set$valor_pago)] <- median(titanic_set$valor_pago, na.rm = T)

table(titanic_set$local_embarque)
table(is.na(titanic_set$local_embarque))

# Preencher os 2 dados faltantes no conjunto da cidade de S = Southampton

# Não funcionou
titanic_set$local_embarque[titanic_set$local_embarque==" "] <- "S"
colSums(is.na(titanic_set))
table(is.na(titanic_set$local_embarque))

# Não funcionou 

titanic_set <- titanic_set %>%  mutate(local_embarque = replace(local_embarque, local_embarque == " ", "S"))
colSums(is.na(titanic_set))
table(is.na(titanic_set$local_embarque))

# Gerando uma função Moda
moda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Encontrando o resultado da moda
resultado <- moda(titanic_set$local_embarque)
print(resultado)

# Incluindo o resultado moda nos não faltantes da variável "local_embarque
titanic_set$local_embarque[is.na(titanic_set$local_embarque)] <- moda(resultado)

# Agora sim!
table(is.na(titanic_set$local_embarque))
colSums(is.na(titanic_set))

titanic_set <- select(titanic_set, everything(), -cabine)
titanic_set <- select(titanic_set, everything(), -nome)
colSums(is.na(titanic_set))

# Mudando a variável "local_embarque" para classe factor
titanic_set$local_embarque <- as.factor(as.character(titanic_set$local_embarque))
str(titanic_set$local_embarque)
table(titanic_set$local_embarque)
table(is.na(titanic_set$local_embarque))

colSums(is.na(titanic_set))
colSums(titanic_set=='')

# Agora sim!
table(is.na(titanic_set$local_embarque))
colSums(is.na(titanic_set))

####################################################################
# Vamos fazer uma breve descritiva gráfica -------------------------
# Vamos criar uma base temporária para manter a base original intacta.

tmp <- treino
tmp$sobreviveu <- as.integer(treino$sobreviveu=="Y")  # 1 = SIM (sobreviveu)

str(treino)

##########################################
# Função para fazer a análise descritiva #
# Vamos avaliar adistribuição de sobreviventes por cada variável X
# Sumarizamos então y por categoria de X e montamos um gráfico de perfis

descritiva <- function(var){
  # Sumariza a taxa de sobreviventes por categoria da variável em análise
  tgc <- Rmisc::summarySE(tmp, measurevar="sobreviveu", groupvars=c(var))
  
  ggplot(tgc) + 
    # Plota o gráfico de barras com as frequências
    geom_bar(aes(x=tgc[,var], weight=N/891, fill=as.factor(tgc[,var]))) + 
    # Plota as barras de erro     # Média dos sobreviventes menos o desvio padrão e mais o desvio padrão
    geom_errorbar(aes(x=tgc[,var], y=sobreviveu, ymin=sobreviveu-se, ymax=sobreviveu+se, colour='1'), width=.1) +
    # Plota as médias de cada grupo
    geom_point(aes(x=tgc[,var], y = sobreviveu, colour = '1', group='1')) +
    # Plota as linhas que conectam as médias
    geom_line(aes(x=tgc[,var], y=sobreviveu, colour='1', group='1')) +
    # Escala de cores do gráfico de médias
    scale_color_viridis_d(direction=-1, begin=0, end=.25) +
    # Escala de cores do gráfico de barras
    scale_fill_viridis_d(direction=-1, begin=.85, end=.95) +
    # Estética mais 'leve' do gráfico
    theme(panel.background = element_rect(fill = "white", colour = "grey", linetype = "solid"),
          panel.grid.major = element_line(size = 0.15, linetype = 'solid', colour = "grey")) + 
    # Remove a legenda
    theme(legend.position = "none") +
    # Rótulo dos eixos
    xlab(var) + ylab("Taxa de sobreviventes") + 
    # Marcas do eixo secundário
    scale_y_continuous(sec.axis = sec_axis(~.*891, name = "Frequencia"), labels = scales::percent)
}

descritiva("sexo")
descritiva("tipo_classe")
descritiva("local_embarque")
descritiva("irmaos_conjuges")
descritiva("pais_criancas")

# Vamos categorizar as variáveis contínuas para analisar
tmp$categoria_idade <- quantcut(tmp$idade, 20)
descritiva("categoria_idade")  # Crianças tem maior probabilidade de sobreviver

tmp$categoria_valor_pago <- quantcut(tmp$valor_pago, 10)
descritiva("categoria_valor_pago")  # Quem pagou mais caro tem a maior porcentagem de sobreviventes


# Construindo o modelo ------------------------------------------

# Recolocando a variável treino = TRUE com todas as correções feitas

titanic_treino <- titanic_set[titanic_set$treino_log == T, ]

# Recolocando a variável teste = FALSE com todas as correções feitas

titanic_teste <- titanic_set[titanic_set$treino_log == F, ]



# Cross validation -----------------------------------

# Buscar reprodutibilidade
set.seed(2360873)
# Gera 80% de 1´s e 20% de 2´s para separar as amostras
n <- sample(1:2, # vamos amostrar elementos do conjunto c(1,2)
            size=nrow(titanic_treino), # O tamanho da amostragem é 891
            replace=TRUE, # Amostragem com reposição (de c(1,2))
            prob=c(0.7,0.3)) # A probabilidade de ser 1 é 80%, de ser 2 é 20%
n %>% length()

######################################
# Dividir amostras de treino e teste #

# Amostra de treino: n==1 (os 80%)
treino_cross <- titanic_treino[n==1,]
# Amostra de teste: n==2 (os 20%)
teste_cross <- titanic_treino[n==2,]

n %>% table()

#levels(treino_cross$sobreviveu) <- make.names(levels(factor(treino_cross$sobreviveu)))

#table(treino_cross$sobreviveu)

######################################
# Treinar a Random Forest            #

# Semente aleatória para buscar a reprodutibilidade
set.seed(2360873)

# Rodar o algoritmo
treino_rf <- randomForest::randomForest(
  sobreviveu ~ ., 
  data = treino_cross, 
  ntree = 500,
  mtry = 4, 
  importance = T)

treino_rf

############################
# Avaliar o modelo         #

# Base de treino
avalia <- function(modelo, nome_modelo="titanic_kaggle"){
  p_treino <- predict(modelo, treino_cross, type='prob') # Probabilidade predita
  c_treino <- predict(modelo, treino_cross)              # Classificação
  
  #Base de teste
  p_teste <- predict(modelo, teste_cross, type='prob')
  c_teste <- predict(modelo, teste_cross)
  
  # Data frame de avaliação (Treino)
  aval_treino <- data.frame(obs=treino_cross$sobreviveu, 
                            pred=c_treino,
                            Y = p_treino[,2],
                            N = 1-p_treino[,2]
  )
  
  # Data frame de avaliação (Teste)
  aval_teste <- data.frame(obs=teste_cross$sobreviveu, 
                           pred=c_teste,
                           Y = p_teste[,2],
                           N = 1-p_teste[,2]
  )
  
  tcs_treino <- caret::twoClassSummary(aval_treino, 
                                       lev=levels(aval_treino$obs))
  tcs_teste <- caret::twoClassSummary(aval_teste, 
                                      lev=levels(aval_teste$obs))
  ##########################
  # Curva ROC              #
  
  CurvaROC <- ggplot2::ggplot(aval_teste, aes(d = obs, m = Y, colour='1')) + 
    plotROC::geom_roc(n.cuts = 0, color="blue") +
    plotROC::geom_roc(data=aval_treino,
                      aes(d = obs, m = Y, colour='1'),
                      n.cuts = 0, color = "red") +
    scale_color_viridis_d(direction = -1, begin=0, end=.25) +
    theme(legend.position = "none") +
    ggtitle(paste("Curva ROC | ", nome_modelo, " | AUC-treino=",
                  percent(tcs_treino[1]),
                  "| AUC_teste = ",
                  percent(tcs_teste[1]))
    )
  
  print('Avaliação base de treino')
  print(tcs_treino)
  print('Avaliação base de teste')
  print(tcs_teste)
  CurvaROC
  
}
avalia(treino_rf, nome_modelo="Random Forest")

# Cross Validation
###########################################
# Usando o Caret para fazer o grid-search #

# number: number of folds for training
# repeats: keep the number for training

# O objeto gerado por trainControl vai controlar o algoritmo 
controle <- caret::trainControl(
  method='repeatedcv', # Solicita um K-Fold com repetições
  number=50, # Número de FOLDS (o k do k-fold)
  repeats=2, # Número de repetições
  search='grid', # especifica o grid-search
  summaryFunction = twoClassSummary, # Função de avaliação de performance
  classProbs = TRUE # Necessário para calcular a curva ROC
)

# agora vamos especificar o grid
grid <- base::expand.grid(.mtry=c(1:10))

#levels(treino_cross$sobreviveu) <- make.names(levels(factor(treino_cross$sobreviveu)))

#table(treino_cross$sobreviveu)

# Vamos treinar todos os modelos do grid-search com cross-validation
gridsearch_rf <- caret::train(sobreviveu ~ .,         # Fórmula (todas as variáveis)
                              data = treino_cross,       # Base de dados
                              method = 'rf',        # Random-forest
                              metric='ROC',         # Escolhe o melhor por essa métrica
                              trControl = controle, # Parâmetros de controle do algoritmo
                              ntree=100,            # Numero de árvores
                              tuneGrid = grid)      # Percorre o grid especificado aqui

print(gridsearch_rf)
plot(gridsearch_rf)




###################################
# Avaliar o modelo tunado         #

avalia(gridsearch_rf, nome_modelo='RF Tunado')
str(titanic_teste)



#modelo_titanic <- rpart::rpart(sobreviveu ~ tipo_classe + sexo + idade + irmaos_conjuges + pais_criancas + valor_pago + local_embarque,
#data=titanic_treino,
#method='class',
#xval=5,
#control = rpart.control(cp = 0, 
#minsplit = 1, 
#maxdepth = 30))


# Submissao ------------------------------------------

#Survived = predict(treino_rf, newdata = titanic_teste)
#PassengerId = titanic_teste$id_passageiro
#output.df <- as.data.frame(PassengerId)
#output.df$Survived <- Survived

#write.csv(output.df, file = "titanic_kaggle_submissao.csv", row.names = F)

#view(output.df)