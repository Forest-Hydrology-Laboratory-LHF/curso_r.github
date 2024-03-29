#----------------------------------------------------------------------------------------------------------
# Laboratorio de Hidrologia Florestal
#
# Raiza Salomao Precinoto - raizaprecinoto@usp.br
#
# Janeiro de 2024
#
# Curso Pratico de R: de conceitos iniciais a analise e visualizacao de dados
#
# Parte 1 - Introduzindo o R, importacao e exportacao de dados, testes estatisticos de comparacao de medias
#----------------------------------------------------------------------------------------------------------

# Instale cada pacote apenas 1 vez no computador, depois nao precisa mais
#install.packages("ggplot2") # o tidyverse contem o ggplot2, logo nao precisa rodar esse comando
install.packages("tidyverse") # Pacote mais recomendado para manipular dados --> + intuitivo
install.packages("pacman")
install.packages("writexl")
install.packages("car")
install.packages("FSA")


# Opcao 1 - Carregar o pacote pacman e depois chamar os outros pacotes
library(pacman)
p_load(tidyverse, car, writexl, FSA)

# Opcao 2 - Fazer tudo em apenas 1 comando:
pacman::p_load(tidyverse, car, writexl, FSA)

# Cite o R e os pacotes utilizados no seu trabalho!
citation()
citation("ggplot2")
citation ("tidyverse")

#-------------------------------- Introducao - R como calculadora -------------------------------

# Hashtag para iniciar comentarios

5*5 # multiplicando

10-2 # diminuindo

55/11 # dividindo

10^2 # exponencial

5+4;5*5;10-2;55/11  #ponto e virgula separa os comandos (todos rodam)

#-------------------------------- Criando nossas proprias tabelas de dados manualmente

# Quero saber o valor da soma dos algarismos de 1 a 5
1+2+3+4+5

# ----- Criar um algoritmo para calcular a soma de 1 a 5
a <- c (1:5)
sum(a) #ou:
resultado <- sum(a);resultado #ou em apenas 1 linha:
resultado <- sum(c(1:5));resultado

# Imagina se o objetivo fosse calcular a soma de 1 a 5000 (altere o objeto "a" definindo que é de 1 a 5000)

# ----- Criar uma tabela em etapas
x <- c(4:6) # um vetor
class(x) # numeros inteiros
str(x) #Structure: x --> um vetor de numeros inteiros
x

is.vector(x)
x<-as.data.frame(x) #Clicar no x la no Environment para visualizar o dataframe na interface do RStudio
x
class(x)
is.data.frame(x)
x
nrow(x) #numero de linhas do dataframe
ncol(x) #numero de colunas do dataframe

x[1,1] # o que esta na primeira linha e na primeira coluna?
x[3,1] # o que esta na linha 3 e na primeira coluna?
x[1,2] 

# ----- Criar uma nova coluna no dataframe x
x$classe <- c("par","impar","par")
x
x$classe[2]  # qual o segundo elemento da coluna "classe"?
x$x[1]
names(x)
class(x$classe)

names(x)<- c("valor","classe")
x

# ----- Agora que vi que a tabela ficou do jeito que eu gostaria, vou exporta-la
tabela_exportar <- x  # nomeando um objeto de outra forma
str(tabela_exportar)

# Alternativa 1 para exportar tabela como txt:
# Criar a pasta desejada no seu computador e definir o diretorio (work directory - wd)
setwd("H:/Meu Drive/_Doc/Curso_R/exports") # as barras do diretorio devem ser invertidas no R
write.table(tabela_exportar, "tabela_numeros.txt") # Ao exportar, se atente ao objeto exportado e ao nome do arquivo

# Alternativa 2: salvar diretamente no diretorio desejado
write.table(tabela_exportar,"H:/Meu Drive/_Doc/Curso_R/exports/tabela_numeros.txt", row.names = FALSE,sep = ";", dec = ",", quote = FALSE) 


#-------------------------------- TESTES ESTATISTICOS DE COMPARACAO DE MEDIAS

# Analisando bases de dados ja embutidas no R

iris  # dataset de largura e comprimento de sepalas e petalas de flores de tres especies do genero Iris 

iris = iris  # criando um objeto do referido dataset
str(iris)  #visualizando a estrutura do dataframe
unique(iris$Species) #valores unicos da coluna "Species" do dataframe iris

#-----------LARGURA DAS SEPALAS---------------

#Sepal width  - largura das sepalas difere entre especies?

# H0: as médias das larguras das sepalas são iguais para as 3 spp
# H1: pelo menos 1 das médias de larguras das spp não é igual

names(iris)

#Primeiro vamos fazer um boxplot e visualizar
box_sepal_largura <- ggplot(iris,aes(x = Species, y = Sepal.Width)) +
  geom_boxplot(width = 0.4,
               color = "black",
               fill = "magenta4",
               #fill = c("cyan3","orange","purple4"),
               # outlier.colour = "black",outlier.shape = 16,
               # outlier.size = 2
  )+
  ylab("Sepal Width (cm)")+
  theme_classic(base_size = 20);box_sepal_largura

# Exportar a figura do boxplot criado acima no formato para publicacao
ggsave("boxplot1.jpeg", box_sepal_largura, height = 7 , width = 9,dpi = 300)
# Atencao para o nome do arquivo (nome.jpeg): se fizer alteracoes no grafico, vai salvar por cima cada vez que rodar o ggsave

# Teste de normalidade (resultado p>0,05 --> dados normais)
shapiro.test(iris$Sepal.Width)

#visualizar histograma
ggplot(iris,aes(x=Sepal.Width))+
  geom_histogram()

#Criar modelo linear para ANOVA
lm_sepal_largura <- lm(Sepal.Width~Species,data=iris)

lm_sepal_largura <- aov(Sepal.Width~Species,data=iris) #funcao aov tambem cria modelo linear para ser usado em ANOVA

# Testar a homogeneidade das variancias (teste de Levene: p > 0,05 variancias iguais, pode usar ANOVA)
leveneTest(lm_sepal_largura)

# Rodando a ANOVA para a comparacao da largura das sepalas
anova(lm_sepal_largura)

# p < 2,2*(10^(-16)) --> p<0,05 --> pelo menos 1 grupo é significativamente diferente

summary(lm_sepal_largura)

TukeyHSD(lm_sepal_largura) # p adj < 0.05 --> os dois grupos sao diferentes


#-----------COMPRIMENTO DAS SEPALAS---------------

#Sepal lenght  - comprimento das sepalas difere entre especies?

# H0: as médias dos comprimentos das sepalas são iguais para as 3 spp
# H1: pelo menos 1 das médias das spp não é igual

box_sepal_comprimento <- ggplot(iris,aes(x = Species, y = Sepal.Length)) +
  geom_boxplot(width = 0.4,
               color = "black",
               fill = "magenta4",
               #fill = c("cyan3","orange","purple4"),
               outlier.colour = "black",outlier.shape = 16,
               outlier.size = 2
  )+
  ylab("Sepal Lenght (cm)")+
  theme_classic(base_size = 20);box_sepal_comprimento

ggsave("boxplot2.jpeg", box_sepal_comprimento, height = 7 , width = 9,dpi = 300)


# Teste de normalidade (resultado p>0,05 --> dados normais)
shapiro.test(iris$Sepal.Length)

#visualizar histograma
ggplot(iris,aes(x=Sepal.Length))+
  geom_histogram()

# Pressuposto da ANOVA não atendido, logo faremos o teste de Kruskal-Wallis

# Kruskal-Wallis (p>0,05 nao existe diferenca entre as medias)
kruskal.test(Sepal.Length~Species,data=iris)
#  p-value  < 2.2e-16 --> Existe diferenca entre as medias de comprimento das sepalas

#Agora queremos descobrir quais sao os grupos diferentes

#O teste de Dunn é o teste indicado como post hoc do teste de Kruskal-Wallis
dunnTest(Sepal.Length~Species,data=iris,method = "bonferroni")

#----------------------FIM DO DIA 1----------------
