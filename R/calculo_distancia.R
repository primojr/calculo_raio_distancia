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
lojas <- read_excel("date/Lojas - Endereços completos.xlsx", sheet = 1) %>% janitor::clean_names()

## Remover caracteres do CEP
lojas <- lojas %>%
  mutate(cep = str_remove_all(cep,"-|\\.") %>% str_trim
         ,qtd_caracters = nchar(cep)
  )
lojas <- lojas[,-6]
lojas %>% janitor::tabyl(qtd_caracters)

# Carregar UserID
clientes <- read_csv("date/userid.csv") %>%
  mutate(latitude  = round(latitude , 5), longitude = round(longitude,5)) %>%
  distinct(.keep_all = TRUE)

glimpse(clientes)

# Cep das lojas
cep_t = lojas$cep

# Buscar as coordenadas das lojas
lojas_ll = as.data.frame(NULL)
for(i in 1:length(cep_t)){
  Sys.sleep(1.2)
  a <-  cepR::busca_cep(cep = cep_t[i], token)
  lojas_ll <- rbind(lojas_ll,a)
}

lojas_ll <- na.omit(lojas_ll)

# Codigo da loja com as coordenadas
lojas_ll <- inner_join(lojas, select(lojas_ll,cep, latitude,longitude), by = c("cep"="cep"))

lojas_ll <-  distinct(lojas_ll, .keep_all = TRUE)

# Loja proxima
# r = 1.5

calcular_distancia = function(num_da_loja){
# Codigo da loja
  n_loja = num_da_loja
# Latitude e longitude da loja
  lat = rep(lojas_ll$latitude[lojas_ll$loja == n_loja],nrow(clientes))
  lon = rep(lojas_ll$longitude[lojas_ll$loja == n_loja],nrow(clientes))
# Calculo da distancia
  ll_loja = purrr::map2(lat, lon, function(x,y) c(x,y))
  ll_cliente = purrr::map2(clientes$latitude, clientes$longitude, function(x,y) c(x,y))
  dist_loja = purrr::map2_dbl(ll_loja, ll_cliente, function(x,y) haversine(x, y))
  dist_loja
}


col <- unique(lojas_ll$loja)
dist = purrr::map_dfc(.x = col, .f = calcular_distancia)
colnames(dist) <- col

clientes <- cbind(clientes, dist)


## Contagem
clientes <- clientes %>%
  mutate(across(H006:T814, ~ if_else(.x <= 2,1,0)))

clientes %>%
summarise(across(H006:T814, ~ sum(.x)))
