# Instalar pct
#install.packages("cepR")

# Carregar pcts
library(tidyverse)
library(cepR)
library(pracma)

token <- "0f088d16c04b648813fa754a77514d2b" # Meu token
#sp <- cepR::busca_cidades(estado = "BA", token = token)

# Exemplo
cepR::busca_cep(cep = '46400000', token)

# Carregar base de lojas
lojas <- read_excel("data.frame/enderecos_hortfruti.xlsx", sheet = 1) %>% janitor::clean_names()

## Remover caracteres do CEP
lojas <- lojas %>%
  mutate(cep = str_remove_all(cep,"-|\\.") %>% str_trim
         ,qtd_caracters = nchar(cep)
  )
lojas <- lojas[,-6]
lojas %>% janitor::tabyl(qtd_caracters)

clientes <- read_csv("date/userid.csv")
glimpse(clientes)

# Cep das lojas
cep_t = lojas$CEP

# Buscar as coordenadas
lojas_ll = as.data.frame(NULL)
for(i in 1:length(cep_t)){
  Sys.sleep(1.2)
  a <-  cepR::busca_cep(cep = cep_t[i], token)
  lojas_ll <- rbind(lojas_ll,a)
}

lojas_ll <- na.omit(b)
lojas_ll

# Cruzar com codigo da loja
#b <- inner_join(b, lojas, by = c())


# Calculo da distancia

# Separar a longitude da cidade da loja
lojas_ll$lat.cabo_frio = lojas_ll$latitude[b$cidade == 'Cabo Frio']
lojas_ll$lon.cabo_frio = lojas_ll$longitude[b$cidade == 'Cabo Frio']


longlat1 = purrr::map2(lojas_ll$lat.cabo_frio, lojas_ll$lon.cabo_frio, function(x,y) c(x,y))
longlat2 = purrr::map2(lojas_ll$latitude, lojas_ll$longitude, function(x,y) c(x,y))
dist_caboFrio = purrr::map2_dbl(longlat1, longlat2, function(x,y) haversine(x, y))
b <- cbind(lojas_ll, dist_caboFrio)



