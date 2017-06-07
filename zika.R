zika_old = read.csv("linkage.csv", header = TRUE, sep = ";", fileEncoding = 'utf-8')

################################################################################
# criando data.frame com os dados do csv selecionados para utiliza??o
################################################################################

zika_new <- data.frame(
  zika_old$NU_IDADE_GESTANTE,
  zika_old$CO_RACA_COR,
  zika_old$SG_UF_RESIDENCIA,
  zika_old$CO_MUN_RESIDENCIA,
  zika_old$SEXO,
  zika_old$DTNASC,
  zika_old$DTNASC,
  zika_old$PESO,
  zika_old$NU_COMPRIMENTO,
  zika_old$NU_PERIMETRO_CEFALICO,
  zika_old$NU_DIAMETRO_CEFALICO,
  zika_old$NU_PERIMETRO_TORAXICO,
  zika_old$TP_CLASSIFICACAO_FETO_RN,
  zika_old$TP_GRAVIDEZ,
  zika_old$TP_CLASSIFICACAO_FINAL,
  zika_old$ST_PRESENCA_FEBRE,
  zika_old$ST_PRESENCA_EXANTEMA,
  zika_old$ST_PRESENCA_EXANTEMA,
  zika_old$ST_REALIZACAO_EXAME_TORSCH,
  zika_old$ST_REALIZACAO_EXAME_DCZ,
  zika_old$ST_OBITO
)

################################################################################
# renomeando as colunas do novo data.frame
################################################################################

colnames(zika_new) = c(
  "idade_mae",
  "raca_mae",
  "UF_mae",
  "mun_mae",
  "sexo_filho",
  "ano_nasc_filho",
  "mes_nasc_filho",
  "peso_filho",
  "comprimento_filho",
  "perimetro_cefalico_filho",
  "diametro_cefalico_filho",
  "perimetro_toraxico_filho",
  "classificacao_feto_gestacao",
  "tipo_gravidez_gestacao",
  "classificacao_gestacao",
  "febre_dados_clinicos",
  "exantema_dados_clinicos",
  "trimestre_exantema_dados_clinicos",
  "torsch_dados_clinicos",
  "dcz_dados_clinicos",
  "obito_dados_clinicos"
)

################################################################################
# removendo valores inv?lidos
################################################################################

zika_new[zika_new == "FORMSUS_PE_NA"] <- NA
zika_new[zika_new == 0] <- NA
zika_new[zika_new == ""] <- NA
zika_new[zika_new == "Nao Sabe"] <- NA

################################################################################
# a coluna de municipio de resid?ncia da gestante est? com o tipo "int"
# para corrigir isso, deve-se alterar o tipo dessa coluna para "factor"
################################################################################

zika_new$mun_mae <- as.factor(zika_new$mun_mae)

################################################################################
# as colunas comprimento e perimetro cefalico do filho est?o com o tipo "factor"
# para corrigir isso, deve-se substituir "," por "."
# ap?s isso, deve-se alterar o tipo dessa coluna para "numeric"
################################################################################

zika_new$comprimento_filho <- as.numeric(sub(",", ".", zika_new$comprimento_filho))
zika_new$perimetro_cefalico_filho <- as.numeric(sub(",", ".", zika_new$perimetro_cefalico_filho))
zika_new$diametro_cefalico_filho <- as.numeric(sub(",", ".", zika_new$diametro_cefalico_filho))
zika_new$perimetro_toraxico_filho <- as.numeric(sub(",", ".", zika_new$perimetro_toraxico_filho))

################################################################################
# alguns registros das colunas que representam datas apresentavam inconsist?ncia
# a inconsist?ncia era notada por alguns registros possuirem apenas 7 d?gitos
# o motivo disso era por conta do primeiro caractere ser "0"
# para corrigir isso, deve-se adicionar um "0" para os registros com 7 d?gitos
# ap?s isso, deve-se alterar o tipo dessa coluna para "date"
################################################################################

zika_new$ano_nasc_filho[which(nchar(zika_new$ano_nasc_filho) == 7)] <- paste(
  "0", 
  zika_new$ano_nasc_filho[which(nchar(zika_new$ano_nasc_filho) == 7)], 
  sep = ""
)
zika_new$ano_nasc_filho <- as.Date(zika_new$ano_nasc_filho, "%d%m%Y")

zika_new$mes_nasc_filho[which(nchar(zika_new$mes_nasc_filho) == 7)] <- paste(
  "0", 
  zika_new$mes_nasc_filho[which(nchar(zika_new$mes_nasc_filho) == 7)], 
  sep = ""
)
zika_new$mes_nasc_filho <- as.Date(zika_new$mes_nasc_filho, "%d%m%Y")

################################################################################
# pegando a parte do m?s/ano para as respectivas colunas
################################################################################

zika_new$ano_nasc_filho <- as.integer(substr(zika_new$ano_nasc_filho, 1, 4))
zika_new$mes_nasc_filho <- as.integer(substr(zika_new$mes_nasc_filho, 6, 7))

################################################################################
# para cada coluna do data.frame
#   se a coluna for um valor num?rico
#     enquanto existir outliers na coluna
#       seta NA ao valor considerado outlier
################################################################################

for(i in 1:ncol(zika_new)) {
  if(!is.factor(zika_new[,i])) {
    while(length(boxplot(zika_new[,i])$out) != 0) {
      zika_new[,i][zika_new[,i] %in% boxplot(zika_new[,i])$out] <- NA
    }
  }
}

################################################################################
# concatenando valores do campo "obito_dados_clinicos"
################################################################################

zika_new$obito_dados_clinicos[zika_new$obito_dados_clinicos == "Obito"] <- "Sim"
zika_new$obito_dados_clinicos[zika_new$obito_dados_clinicos == "Vivo"] <- "Nao"
zika_new$obito_dados_clinicos[zika_new$obito_dados_clinicos == "Ignorado"] <- NA
zika_new$obito_dados_clinicos <- droplevels(zika_new$obito_dados_clinicos)

################################################################################
# mostrar somente o texto da coluna "classificacao_gestacao"
################################################################################

zika_new$classificacao_gestacao <- substr(
  zika_new$classificacao_gestacao, 
  4, 
  nchar(as.character(zika_new$classificacao_gestacao))
)

zika_new$classificacao_gestacao <- as.factor(zika_new$classificacao_gestacao)

################################################################################
# discretizando colunas
################################################################################

zika_new$idade_mae <- 
  ordered(
    cut(
      zika_new$idade_mae, 
      c(0, 12, 18, 24, 65)
    ), 
    labels = c(
      "Crian?a", 
      "Adolescente", 
      "Jovem-Adulto", 
      "Adulto"
    )
  )

zika_new$ano_nasc_filho <- as.factor(zika_new$ano_nasc_filho)
zika_new$mes_nasc_filho <- as.factor(zika_new$mes_nasc_filho)

zika_new$peso_filho <- 
  ordered(
    cut(
      zika_new$peso_filho, 
      c(0, 2500, 3500, 5000)
    ), 
    labels = c(
      "Abaixo do Normal", 
      "Normal", 
      "Acima do Normal"
    )
  )

zika_new$comprimento_filho <- cut(
  zika_new$comprimento_filho, 
  hist(zika_new$comprimento_filho)$breaks,
  dig.lab = 10
)

zika_new$perimetro_cefalico_filho <- cut(
  zika_new$perimetro_cefalico_filho, 
  hist(zika_new$perimetro_cefalico_filho)$breaks,
  dig.lab = 10
)

zika_new$diametro_cefalico_filho <- cut(
  zika_new$diametro_cefalico_filho, 
  hist(zika_new$diametro_cefalico_filho)$breaks,
  dig.lab = 10 
)

zika_new$perimetro_toraxico_filho <- cut(
  zika_new$perimetro_toraxico_filho, 
  hist(zika_new$perimetro_toraxico_filho)$breaks,
  dig.lab = 10 
)

################################################################################
# alterando exantema_dados_clinicos
################################################################################

zika_new$exantema_dados_clinicos <- gsub("Nao.*", "Nao", zika_new$exantema_dados_clinicos)
zika_new$exantema_dados_clinicos <- gsub("Sim.*", "Sim", zika_new$exantema_dados_clinicos)
zika_new$exantema_dados_clinicos <- as.factor(zika_new$exantema_dados_clinicos)

################################################################################
# criando coluna para o trimestre de presença da exantema
################################################################################

zika_new$trimestre_exantema_dados_clinicos <- gsub(".*nao.*", NA, zika_new$trimestre_exantema_dados_clinicos)
zika_new$trimestre_exantema_dados_clinicos <- gsub(".*1.*", 1, zika_new$trimestre_exantema_dados_clinicos)
zika_new$trimestre_exantema_dados_clinicos <- gsub(".*2.*", 2, zika_new$trimestre_exantema_dados_clinicos)
zika_new$trimestre_exantema_dados_clinicos <- gsub(".*3.*", 3, zika_new$trimestre_exantema_dados_clinicos)
zika_new$trimestre_exantema_dados_clinicos <- gsub(".*3.*", 3, zika_new$trimestre_exantema_dados_clinicos)
zika_new$trimestre_exantema_dados_clinicos <- as.factor(zika_new$trimestre_exantema_dados_clinicos)

################################################################################
# removendo conte?do tempor?rio
################################################################################

zika_new <- droplevels(zika_new)
str(zika_new)