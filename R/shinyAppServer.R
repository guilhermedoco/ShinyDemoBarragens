#' Shiny app server function
#'
#' @param input provided by shiny
#' @param output provided by shiny
#'


# Define server logic required to draw a histogram
shinyAppServer <- function(input, output, session) {

  output$TabelaCompleta <- DT::renderDataTable({
    Tab <- final %>%
      select(`NOME DO EMPREENDEDOR`,BARRAGEM = `NOME DA BARRAGEM DE MINERAÇÃO`,UF,MUNICÍPIO,`MINÉRIO PRINCIPAL`,`ALTURA ATUAL (m)`=AlturaAtual,
             `VOLUME ATUAL (m3)`=VolumeAtual,UF,`CATEGORIA DE RISCO`,`CATEGORIA DE RISCO`, `DANO POTENCIAL ASSOCIADO`,CLASSE)


    DT::datatable(Tab,rownames = FALSE, escape = FALSE, extensions = 'Scroller', options =
                    list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'),
                         pageLength = 20,
                         columnDefs = list(list(className = 'dt-center', targets = c(0:9))),
                         initComplete = JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#22115e', 'color': '#fff'});",
                           "}")
                    )) %>%formatCurrency(c(6:7),digits = 1,currency = "",mark=".",dec.mark = ",")
  })

  output$TabCategoriaRisco <- DT::renderDataTable({

    Tab <- final %>%
      group_by(UF,`CATEGORIA DE RISCO`) %>%
      summarise(Qtd = n()) %>%
      spread(`CATEGORIA DE RISCO`,Qtd) %>%
      select(UF,`NÃO CATEGORIZADO`=V1,BAIXA=Baixa,MÉDIA=Média,ALTA=Alta) %>%
      mutate(`TOTAL DE BARRAGENS` = sum(`NÃO CATEGORIZADO`,BAIXA,MÉDIA,ALTA,na.rm=TRUE))

    DT::datatable(Tab,rownames = FALSE, escape = FALSE, extensions = 'Scroller', options =
                    list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'),
                         pageLength = 20,
                         columnDefs = list(list(className = 'dt-center', targets = c(0:5))),
                         initComplete = JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#22115e', 'color': '#fff'});",
                           "}")
                    )) %>%formatCurrency(c(2:6),digits = 0,currency = "",mark=".",dec.mark = ",")
  })

  output$TabCategoriaRiscoPotencial <- DT::renderDataTable({

    Tab <- final %>%
      group_by(UF,`DANO POTENCIAL ASSOCIADO`) %>%
      summarise(Qtd = n()) %>%
      spread(`DANO POTENCIAL ASSOCIADO`,Qtd) %>%
      select(UF,`NÃO CATEGORIZADO`=V1,BAIXA=Baixa,MÉDIA=Média,ALTA=Alta) %>%
      mutate(`TOTAL DE BARRAGENS` = sum(`NÃO CATEGORIZADO`,BAIXA,MÉDIA,ALTA,na.rm=TRUE))

    DT::datatable(Tab,rownames = FALSE, escape = FALSE, extensions = 'Scroller', options =
                    list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'),
                         pageLength = 20,
                         columnDefs = list(list(className = 'dt-center', targets = c(0:5))),
                         initComplete = JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#22115e', 'color': '#fff'});",
                           "}")
                    )) %>%formatCurrency(c(2:6),digits = 0,currency = "",mark=".",dec.mark = ",")
  })
  output$MapaBarragens <- renderHighchart({
    leaflet(data = final) %>% addTiles() %>%
      addCircleMarkers(~long, ~lat, radius = 5,
                       #clusterOptions = markerClusterOptions(),
                       popup = ~as.character(`NOME DA BARRAGEM DE MINERAÇÃO`), label = ~as.character(`NOME DA BARRAGEM DE MINERAÇÃO`),
                       color=final$Color)
  })

  output$MapaBarragensDano <- renderHighchart({
    leaflet(data = final) %>%
      addTiles() %>%
      addCircleMarkers(~long, ~lat, radius = 5,
                       #clusterOptions = markerClusterOptions(),
                       popup = ~as.character(`NOME DA BARRAGEM DE MINERAÇÃO`), label = ~as.character(`NOME DA BARRAGEM DE MINERAÇÃO`),
                       color=final$ColorDano)
  })


}
