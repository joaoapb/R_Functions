library(tidyverse)
library(httr)
library(rvest)
library(lubridate)

pegaInflacao <- function(indice="IPCA", dtIni="01/2010", dtFim="01/2018",
                         inflator = TRUE) {
  indice_list <- case_when(
    indice == "IGPM" ~ list(cod = "00189IGP-M", label = "IGP-M (FGV)",
                            dtMin = dmy("01/06/1989")),
    indice == "IGP-DI" ~ list(cod = "00190IGP-DI", label = "IGP-DI (FGV)",
                              dtMin = dmy("01/02/1944")),
    indice == "INPC" ~ list(cod = "00188INPC", label = "INPC (IBGE)",
                            dtMin = dmy("01/04/1979")),
    indice == "IPCA" ~ list(cod = "00433IPC-A", label = "IPC-A (IBGE)",
                            dtMin = dmy("01/01/1980")),
    indice == "IPCA-E" ~ list(cod = "10764IPC-E", label = "IPCA-E (IBGE)",
                              dtMin = dmy("01/04/1979")),
    indice == "IPC-BR" ~ list(cod = "00191IPC-BRASIL", label = "IPC-BRASIL (FGV)",
                              dtMin = dmy("01/01/1990")),
    indice == "IPC-SP" ~ list(cod = "00193IPC-SP", label = "IPC-SP (FIPE)",
                              dtMin = dmy("01/11/1942")))
  indice_list <- setNames(indice_list, c("cod", "label", "dtMin"))

  # prepara a query
  query <- list('aba' = "1",
                'selIndice' = indice_list$cod,
                'dataInicial' = dtIni,
                'dataFinal' = dtFim,
                'valorCorrecao' = "1,00",
                'idIndice' = "",
                'nomeIndicePeriodo' = "")

  r <- POST(
    url = paste0("https://www3.bcb.gov.br/CALCIDADAO/publico/",
                 "corrigirPorIndice.do?method=corrigirPorIndice"),
    body = query)

  if (r$status_code != 200) {
    return("erro")
  }
  html <- read_html(rawToChar(r$content))

  trs <- html_nodes(html, css = ".tabela tr")

  ind = FALSE
  for (tr in trs) {
    tds  = html_nodes(tr, css = "td")

    if (length(tds) == 2) {
      if (gregexpr("ndice de", html_text(tds[1]),
                   useBytes = FALSE)[[1]][1] > 0) {
        ind = as.numeric(gsub(",", ".", html_text(tds[2]), fixed = TRUE))
        break
      }
    }
  }

  if (inflator) {
    return(ind)
  }
  if (!inflator) {
    print("aqui")
    return(1 / ind)
  }
  return(ind)
}

atualizaDF <- function(df, indice = "IPCA", dtBase = "05/2011",
                       colValor = "Valor", colData = "Data",
                       colDataFormato = "my") {
  # guarda os nomes e cria os novos
  nomes_originais <- names(df)
  novos_nomes <- nomes_originais
  novos_nomes[which(novos_nomes == colData)] <- "colunaData"
  novos_nomes[which(novos_nomes == colValor)] <- "colunaValor"
  names(df) <- novos_nomes

  dtBase_lubri = dmy(paste("01/", dtBase))

  df <- df %>%
    rowwise() %>%
    mutate(
      colunaData_lubri = dmy(paste0("01/", colunaData)),
      multiplicador = ifelse(colunaData_lubri < dtBase_lubri,
                             pegaInflacao(indice = indice, dtIni = colunaData,
                                          dtFim = dtBase), NA),
      multiplicador = ifelse(colunaData_lubri == dtBase_lubri,
                             1, multiplicador),
      multiplicador = ifelse(colunaData_lubri > dtBase_lubri,
                             pegaInflacao(indice = indice, dtIni = dtBase,
                                          dtFim = colunaData, inflator = FALSE),
                             multiplicador),
      valor_atualizado = multiplicador * colunaValor) %>%
    select(-colunaData_lubri)

  names(df) <- c(nomes_originais, "multiplicador", "valor_atualizado")

  return(df)
}