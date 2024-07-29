# PRIMEIROS PASSOS ----

# COMENTÁRIOS VEM COM # ANTES.

# SEÇÕES DO CÓDIGO COMEÇAM COM # E ACABAM COM 4 TRACINHOS DE MENOS COMO A PRIMEIRA LINHA

# PARA RODAR A LINHA, SELEÇÃO, ETC... CTRL+ENTER

# Primeiro instalar o pacote "pacman" e "installr"

# install.packages("pacman")
# install.packages("installr")

# Ele tem uma função para pegar todos os pacotes utilizados

# p_lib()

# Esta função é importante para pegar todos os pacotes de uma versão do R
# anterior e então criar uma lista para a nova instalação

# CÓDIGO DE ATUALIZAÇÃO DO R - RODAR QUANDO PERTINENTE
# Pegando pacotes da versão velha

# require(pacman)
# mypks <- p_lib()
# saveRDS(mypks, "~/mypks.rds")

# AGORA ATUALIZAR O R

# require(installr)
# updateR()

# NA VERSÃO ATUALIZADA SE NÃO PEGAR OS PACOTES AUTOMATICAMENTE

# mypks <- readRDS("~/mypks.rds")
# install.packages(mypks)

# PRONTO A VERSÃO NOVA DO R TEM OS PACOTES DE NOVO, E AGORA ATUALIZADOS =]

# INSTALAR E CARREGAR BIBLIOTECAS USADAS NO CÓDIGO ----
mycodepks <- c("readxl" , "dplyr" , "Ternary")

# RODAR SE NECESSÁRIO
 install.packages(mycodepks)

lapply(mycodepks, require, character.only = TRUE)

# Carregando 1 a 1
# require(dplyr)
# require(readxl)
# require(Ternary)

# CARREGAR A BASE ----

# função para ler xlsx
# sheet = NOME OU NÚMERO,
# col_names = FALSE para não usar primeira linha não vazia como cabeçalho
# ver documentação
#?read_xlsx

# Nome da base de dados

base.dados <- "Data.xlsx"

# Quantas planilhas

save_plots <- function(base.dados,wid,hei){
  
  n.sheets <- length( excel_sheets( base.dados ) )
  
  n.sheets
  
  # Carregando
  
  for(a in 1:n.sheets){
    
    data.tibble <- read_xlsx(base.dados , sheet = a , col_names = FALSE)
    
    # Usar como data.frame. Veja qual é mais fácil.
    
    data <- data.tibble %>% as.data.frame()
    
    # Visualizando
    
    #View(data)
    
    # Componentes
    
    componentes <- data.frame(data[2:4,1:3])
    
    names(componentes) <- data[1,1:3]
    
    #View(componentes)
    
    temperatura.celsius <- data[2,5]
    
    temperatura.celsius.tbl <- data.tibble[2,5]
    
    # comparando saídas de DF e TBL
    
    temperatura.celsius
    
    temperatura.celsius.tbl
    
    # LOCALIZAÇÃO DE CERTAS PALAVRAS DE INTERESSE ----
    # Vamos procurar espaço onde tem algumas palavras
    # Temos as coordenadas para Molar base (cuidado com a caixa das letras)
    # Mass Base, Left Phase e Right Fase
    
    place.molarbase <- which(data == "Molar base" , arr.ind = TRUE) 
    place.massbase <- which(data == "Mass Base" , arr.ind = TRUE)
    place.left <- which(data == "Left Phase" , arr.ind = TRUE)
    place.right <- which(data == "Right Phase" , arr.ind = TRUE)
    
    place.massbase
    
    place.right
    
    # A importância da localização se dá na posição para formatação dos dados. 
    # Uma checagemmeio boba
    data[place.molarbase[1] , place.molarbase[2]]
    data[place.left[1,1] , place.left[1,2]]
    data[place.left[2,1] , place.left[2,2]]
    data[place.left[,1] , place.left[,2]]
    data[place.left[1,] , place.left[2,]] # NÃO É COMO QUEREMOS.
    
    # DIMENSÃO DA BASE ----
    
    dim.data <- dim(data)
    
    dim.data
    
    # GRUPOS ----
    
    molar.left <- data.frame(data [9:dim.data[1] , 1:3 ] )
    
    names(molar.left) <- componentes$Componente
    
    molar.right <- data.frame(data [9:dim.data[1] , 4:6 ] )
    
    names(molar.right) <- componentes$Componente
    
    mass.left <- data.frame(data [9:dim.data[1] , 8:10 ] )
    
    names(mass.left) <- componentes$Componente
    
    mass.right <- data.frame(data [9:dim.data[1] , 11:13 ] )
    
    names(mass.right) <- componentes$Componente
    
    str(molar.left)
    for(i in 1:ncol(molar.left)){
      molar.left[,i] <- as.numeric(molar.left[,i])
    }
    str(molar.right)
    for(j in 1:ncol(molar.right)){
      molar.right[,j] <- as.numeric(molar.right[,j])
    }
    str(mass.left)
    for(k in 1:ncol(mass.left)){
      mass.left[,k] <- as.numeric(mass.left[,k])
    }
    str(mass.right)
    for(l in 1:ncol(mass.right)){
      mass.right[,l] <- as.numeric(mass.right[,l])
    }
    
    # PRIMEIRO TESTE ----
    #?TernaryPlot
    
    # Gráfico limpo
    TernaryPlot()
    
    # Gráfico com nomes
    TernaryPlot(alab = componentes$Componente[1] , 
                blab = componentes$Componente[2] ,
                clab = componentes$Componente[3] )
    
    # Gráfico com pontos
    png(file=paste0(componentes$Componente[1],"_",
                    componentes$Componente[2],"_",
                    componentes$Componente[3],"_",
                    "mass_temperature_",
                    temperatura.celsius,".png"),
        width=wid, height=hei)
    
    TernaryPlot(alab = componentes$Componente[2] , 
                blab = componentes$Componente[3] ,
                clab = componentes$Componente[1] ,
                clockwise = F,
                atip = "MASSA")
    TernaryPoints(mass.left [ , c(2,3,1) ])
    TernaryPoints(mass.right[ , c(2,3,1) ])
    for(i in 1:dim(mass.left)[1]){
      coord <- list(A = as.vector(unlist(mass.left[ i , c(2,3,1) ])),
                    B = as.vector(unlist(mass.right[ i , c(2,3,1) ])))
      coord
      TernaryLines(coord)
    }
    for(i in 1:(dim(mass.left)[1]-1)){
      coord <- list(A = as.vector(unlist(mass.left[ i , c(2,3,1) ])),
                    B = as.vector(unlist(mass.left[ i+1 , c(2,3,1) ])))
      coord
      TernaryLines(coord , col="red" , lwd = 2)
    }
    for(i in 1:(dim(mass.right)[1]-1)){
      coord <- list(A = as.vector(unlist(mass.right[ i , c(2,3,1) ])),
                    B = as.vector(unlist(mass.right[ i+1 , c(2,3,1) ])))
      coord
      TernaryLines(coord , col="red" , lwd = 2)
    }
    
    dev.off()
    
    png(file=paste0(componentes$Componente[1],"_",
                    componentes$Componente[2],"_",
                    componentes$Componente[3],"_",
                    "molar_temperature_",
                    temperatura.celsius,".png"),
        width=wid, height=hei)
    
    TernaryPlot(alab = componentes$Componente[2] , 
                blab = componentes$Componente[3] ,
                clab = componentes$Componente[1] ,
                clockwise = F,
                atip = "MOLAR")
    TernaryPoints(molar.left [ , c(2,3,1) ])
    TernaryPoints(molar.right[ , c(2,3,1) ])
    for(i in 1:dim(molar.left)[1]){
      coord <- list(A = as.vector(unlist(molar.left[ i , c(2,3,1) ])),
                    B = as.vector(unlist(molar.right[ i , c(2,3,1) ])))
      coord
      TernaryLines(coord)
    }
    for(i in 1:(dim(molar.left)[1]-1)){
      coord <- list(A = as.vector(unlist(molar.left[ i , c(2,3,1) ])),
                    B = as.vector(unlist(molar.left[ i+1 , c(2,3,1) ])))
      coord
      TernaryLines(coord , col="red" , lwd = 2)
    }
    for(i in 1:(dim(molar.right)[1]-1)){
      coord <- list(A = as.vector(unlist(molar.right[ i , c(2,3,1) ])),
                    B = as.vector(unlist(molar.right[ i+1 , c(2,3,1) ])))
      coord
      TernaryLines(coord , col="red" , lwd = 2)
    }
    
    dev.off()
    
    print(a)
    
  }
  
}

save_plots(base.dados,800,800)






