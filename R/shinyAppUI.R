#' Shiny app server object
#'
#' @importFrom graphics hist
#' @import shiny

# create the shiny application user interface
# Carregando Pacotes ----
require(tabulizer)
require(dplyr)
require(biogeo)
require(tidyverse)
require(measurements)
require(leaflet)
require(dashboardthemes)
require(shiny)
require(highcharter)
require(tidyverse)
require(shinyjs)
require(shinyBS)
require(zoo)
require(lubridate)
require(shinydashboardPlus)
require(shinydashboard)
require(DT)

# Carregando e Ajustando base de dados ----
# Location of WARN notice pdf file
arquivo <- 'data/barragens.pdf'

# Extraindo a tabela do arquivo
tabela <- extract_tables(arquivo,encoding = "UTF-8")
final <- do.call(rbind, tabela[-length(tabela)])
headers <- final[1,]
final <- as.data.frame(final[2:nrow(final), ])

# Ajustando nome das colunas
names(final) <- headers

# Retirando os cabeçalhos das páginas
final <- final %>% filter(MUNICÍPIO!="MUNICÍPIO")

# Ajustando coluna de latitude e longitude
final$LATITUDE <- gsub(" ", "", final$LATITUDE, fixed = TRUE)
final$LATITUDE <- str_replace(final$LATITUDE,pattern = "o",replacement="°")
final$LATITUDE <- str_replace(final$LATITUDE,pattern = ",",replacement=".")
final$LONGITUDE <- gsub(" ", "", final$LONGITUDE, fixed = TRUE)
final$LONGITUDE <- str_replace(final$LONGITUDE,pattern = "o",replacement="°")
final$LONGITUDE <- str_replace(final$LONGITUDE,pattern = ",",replacement=".")

dd<-ifelse(substr(final$LATITUDE,1,1)=="-",substr(final$LATITUDE,2,3),substr(final$LATITUDE,1,2))
mm<-ifelse(substr(final$LATITUDE,1,1)=="-",substr(final$LATITUDE,5,6),substr(final$LATITUDE,4,5))
ss<-ifelse(substr(final$LATITUDE,1,1)=="-",substr(final$LATITUDE,8,13),substr(final$LATITUDE,7,12))
ns<-ifelse(substr(final$LATITUDE,1,1)=="-","S","N")
final$lat <- biogeo::dms2dd(as.numeric(dd),as.numeric(mm),as.numeric(ss),ns)

dd<-ifelse(substr(final$LONGITUDE,1,1)=="-",substr(final$LONGITUDE,2,3),substr(final$LONGITUDE,1,2))
mm<-ifelse(substr(final$LONGITUDE,1,1)=="-",substr(final$LONGITUDE,5,6),substr(final$LONGITUDE,4,5))
ss<-ifelse(substr(final$LONGITUDE,1,1)=="-",substr(final$LONGITUDE,8,13),substr(final$LONGITUDE,7,12))
ns<-ifelse(substr(final$LONGITUDE,1,1)=="-","W","E")
final$long <- biogeo::dms2dd(as.numeric(dd),as.numeric(mm),as.numeric(ss),ns)

# Criando Cores para as categorias de risco e dano associado
final<- final %>%
  mutate(Color = ifelse(`CATEGORIA DE RISCO`=="Alta","red",ifelse(`CATEGORIA DE RISCO`=="Média","yellow","green")),
         ColorDano = ifelse(`DANO POTENCIAL ASSOCIADO`=="Alta","red",ifelse(`DANO POTENCIAL ASSOCIADO`=="Média","yellow","green")))

# Ajustando a variável Volume Atual
final$VolumeAtual <- as.numeric(gsub(",", ".", gsub(".","",gsub(" ", "", final$`VOLUME ATUAL (m3)`, fixed = TRUE),fixed = TRUE), fixed = TRUE))

# Ajustando a variável Altura Atual
final$AlturaAtual <- as.numeric(gsub(",", ".", gsub(".","",gsub(" ", "", final$`ALTURA ATUAL (m)`, fixed = TRUE),fixed = TRUE), fixed = TRUE))

# Design ######################################################################

brbg <- hsv(0.5, .35, seq(.25, .95, length.out = 12))

logo_blue <- shinyDashboardLogoDIY(
  boldText = ""
  , mainText = ""
  , textSize = 16
  , badgeText = ""
  , badgeTextColor = "white"
  , badgeTextSize = 0
  , badgeBackColor = ""
  , badgeBorderRadius = 4
)

header <- dashboardHeader(title = logo_blue)

sidebar <- dashboardSidebar(

  sidebarMenu(
    id = "tabs",
    menuItem("Apresentação", tabName = "capa", icon = icon("file")),
    menuItem("Dados Completos", tabName = "dados", icon = icon("file")),
    menuItem("Mapa das Barragens", tabName = "exploratoria", icon = icon("file")),
    br(),br(),br(),br(),br(), br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
    br(),br(),br(),br(),br(),br(),br(),br(),
    h6("Desenvolvido por",align="center"),h6("ABG Consultoria Estatística",align="center"),
    h6("contato@abgconsultoria.com.br",align="center"),
    h6("Tel: (31) 2516-0068",align="center")
  ),
  tags$footer(label = "Desenvolvido por", src = "https://www.abgconsultoria.com.br/assets/images/logo-footer1.png",
              style = "text-align:center; align: center; padding: 0px; margin-bottom: 0px;")
)

body <- dashboardBody(
  tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
  tags$head(tags$link(rel = "shortcut icon", href = "https://scontent.fplu3-1.fna.fbcdn.net/v/t1.0-1/c0.1.200.200a/p200x200/556325_370425363046864_1647666579_n.png?_nc_cat=101&_nc_ht=scontent.fplu3-1.fna&oh=407b166f07b413f7d5669492b9301ba7&oe=5CC1C664")),
  # shinyDashboardThemes(theme = "blue_gradient"),
  theme_blue_gradient,
  tabItems(
    tabItem(
      tabName = "capa",
      h1("APRESENTAÇÃO DO PROJETO", align = "center"),
      h4("Após pouco mais de três anos, na tarde do dia 25 de janeiro de 2019 recebemos mais uma notícia de um rompimento de barragem de contenção de minério. Decidimos então verificar quais dados se tem disponíveis para conhecer sobre a distribuição das barragens de mineradoras no pais e entender o tamanho do problema que temos sobre este tema."),
      h4("As visualizações a seguir se referem a base de dados de classificação das barragens de mineração - database 01/2019, que consta na Cadastro Nacional de Barragens de Mineração no site da Agência Nacional de Mineração."),
      img(src="FotoCapa.jpg",class="img-responsive")
    ),
    tabItem(
      tabName = "dados",
      h1("TABELA COMPLETA", align = "center"),
      DT::dataTableOutput("TabelaCompleta")
    ),
    tabItem(
      tabName = "exploratoria",
      h1("ANÁLISE EXPLORATÓRIA DOS DADOS", align = "center"),
      br(),
      setShadow("box"),
      fluidRow(
        column(12, box(
          height = "600px",
          column(12, splitLayout(cellWidths = c("49%", "2%","49%"),
                                 leafletOutput("MapaBarragens", height = "550px"),
                                 "",
                                 leafletOutput("MapaBarragensDano", height = "550px")
          )),
          title = "MAPAS DE CATEGORIA DO RISCO E POTENCIAL DE DANO ASSOCIADO", width = 12
        ))),
      fluidRow(
        column(12, box(
          height = "800px",
          column(12, splitLayout(cellWidths = c("49%", "2%","49%"),
                                 DT::dataTableOutput("TabCategoriaRisco"),
                                 "",
                                 DT::dataTableOutput("TabCategoriaRiscoPotencial"))),
          title = "VISÃO DAS BARRAGENS POR CATEGORIA DO RISCO E POTENCIAL DE DANO ASSOCIADO", width = 12
        )))
    )
  )
)
shinyAppUI <- dashboardPage(
  title = "BARRAGENS BRASIL",
  header,
  sidebar,
  body
)
