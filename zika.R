################################################################################
# criando DF com os dados do .csv selecionados para utilização
################################################################################

zika_old = read.csv("linkage.csv", header = T, sep = ";", fileEncoding = 'utf-8')
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
  zika_old$TP_GRAVIDEZ,
  zika_old$QT_TEMPO_GEST_DIAG_MICROCEFA,
  zika_old$TP_CLASSIFICACAO_FETO_RN,
  zika_old$TP_CLASSIFICACAO_FINAL,
  zika_old$ST_OBITO,
  zika_old$ST_PRESENCA_FEBRE,
  zika_old$ST_PRESENCA_EXANTEMA,
  zika_old$ST_PRESENCA_EXANTEMA,
  zika_old$ST_REALIZACAO_EXAME_TORSCH,
  zika_old$ST_REALIZACAO_EXAME_DCZ
)

################################################################################
# renomeando as colunas do novo DF
################################################################################

colnames(zika_new) = c(
  "mae.idade",
  "mae.raca",
  "mae.uf",
  "mae.municipio",
  "filho.sexo",
  "filho.nasc.ano",
  "filho.nasc.mes",
  "filho.peso",
  "filho.comprimento",
  "filho.cranio.perimetro",
  "filho.cranio.diametro",
  "filho.torax.perimetro",
  "gestacao.tipo",
  "gestacao.qtd_semanas",
  "gestacao.filho.classificacao",
  "gestacao.filho.microcefalia",
  "gestacao.filho.obito",
  "gestacao.doenca.febre",
  "gestacao.doenca.exantema",
  "gestacao.doenca.exantema.trimestre",
  "gestacao.exame.torsch",
  "gestacao.exame.dcz"
)

################################################################################
# as colunas comprimento e perimetro cefalico do filho estão com o tipo "factor"
# para corrigir isso, deve-se substituir "," por "."
# após isso, deve-se alterar o tipo dessa coluna para "numeric"
################################################################################

x <- function(x) {
  as.numeric(sub(",", ".", x))
}

zika_new$filho.comprimento <- x(zika_new$filho.comprimento)
zika_new$filho.cranio.perimetro <- x(zika_new$filho.cranio.perimetro)
zika_new$filho.cranio.diametro <- x(zika_new$filho.cranio.diametro)
zika_new$filho.torax.perimetro <- x(zika_new$filho.torax.perimetro)
zika_new$gestacao.qtd_semanas <- x(zika_new$gestacao.qtd_semanas)

################################################################################
# pegando a parte do mês/ano para as respectivas colunas
################################################################################

x <- function(x, start, end = 0) {
  as.integer(substr(x, nchar(x)-start+1, nchar(x)-end))
}

zika_new$filho.nasc.ano <- x(zika_new$filho.nasc.ano, 4)
zika_new$filho.nasc.mes <- x(zika_new$filho.nasc.mes, 6, 4)

x <- function(x, start) {
  substr(x, start, nchar(as.character(x)))
}

zika_new$gestacao.filho.microcefalia <- x(zika_new$gestacao.filho.microcefalia, 4)

################################################################################
# concatenando valores do campo "gestacao.filho.obito"
################################################################################

zika_new$gestacao.filho.obito[zika_new$gestacao.filho.obito == "Obito"] <- "Sim"
zika_new$gestacao.filho.obito[zika_new$gestacao.filho.obito == "Vivo"] <- "Nao"

zika_new$gestacao.doenca.exantema.trimestre <- gsub(".*nao.*", NA, zika_new$gestacao.doenca.exantema.trimestre)
zika_new$gestacao.doenca.exantema.trimestre <- gsub("Nao.*", NA, zika_new$gestacao.doenca.exantema.trimestre)
zika_new$gestacao.doenca.exantema.trimestre <- gsub(".*1.*", 1, zika_new$gestacao.doenca.exantema.trimestre)
zika_new$gestacao.doenca.exantema.trimestre <- gsub(".*2.*", 2, zika_new$gestacao.doenca.exantema.trimestre)
zika_new$gestacao.doenca.exantema.trimestre <- gsub(".*3.*", 3, zika_new$gestacao.doenca.exantema.trimestre)
zika_new$gestacao.doenca.exantema.trimestre <- gsub(".*3.*", 3, zika_new$gestacao.doenca.exantema.trimestre)

zika_new$gestacao.doenca.exantema <- gsub("Nao.*", "Nao", zika_new$gestacao.doenca.exantema)
zika_new$gestacao.doenca.exantema <- gsub("Sim.*", "Sim", zika_new$gestacao.doenca.exantema)

zika_new$gestacao.filho.classificacao <- gsub("t", "T", zika_new$gestacao.filho.classificacao)

################################################################################
# removendo valores inválidos
################################################################################

zika_new[zika_new == "FORMSUS_PE_NA"] <- NA
zika_new[zika_new == 0] <- NA
zika_new[zika_new == ""] <- NA
zika_new[zika_new == "Nao Sabe"] <- NA
zika_new[zika_new == "Nao se aplica"] <- NA
zika_new[zika_new == "Ignorado"] <- NA
zika_new[zika_new == "Sem classificacao"] <- NA
zika_new[zika_new == "Em investigacao"] <- NA
zika_new[zika_new == "SEM INFORMACAO"] <- NA

################################################################################
# para cada coluna do data.frame
#   se a coluna for um valor numérico
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
# discretizando colunas
################################################################################

zika_new$mae.municipio <- as.factor(zika_new$mae.municipio)
zika_new$gestacao.filho.microcefalia <- as.factor(zika_new$gestacao.filho.microcefalia)
zika_new$gestacao.doenca.exantema <- as.factor(zika_new$gestacao.doenca.exantema)
zika_new$gestacao.doenca.exantema.trimestre <- as.factor(zika_new$gestacao.doenca.exantema.trimestre)
zika_new$filho.nasc.ano <- as.factor(zika_new$filho.nasc.ano)
zika_new$filho.nasc.mes <- as.factor(zika_new$filho.nasc.mes)
zika_new$gestacao.filho.classificacao <- as.factor(zika_new$gestacao.filho.classificacao)
levels(zika_new$gestacao.filho.classificacao)[4] <- "A Termo"

x <- function(x) {
  ordered(
    cut(x, c(boxplot(x)$stats[-3])), 
    labels = c("Abaixo da Média", "Na Média", "Acima da Média")
  )
}

zika_new$mae.idade <- x(zika_new$mae.idade)
zika_new$filho.peso <- x(zika_new$filho.peso)
zika_new$filho.comprimento <- x(zika_new$filho.comprimento)
zika_new$filho.cranio.perimetro <- x(zika_new$filho.cranio.perimetro)
zika_new$filho.cranio.diametro <- x(zika_new$filho.cranio.diametro)
zika_new$filho.torax.perimetro <- x(zika_new$filho.torax.perimetro)
zika_new$gestacao.qtd_semanas <- x(zika_new$gestacao.qtd_semanas)

zika_new <- droplevels(zika_new)

################################################################################
# rodando padrões frequentes
################################################################################

#install.packages("arules")
library(arules)

ZikaTrans <- as(zika_new, "transactions")
rules <- apriori(
  ZikaTrans, 
  parameter = list(
    supp = 0.001,
    maxlen = 22,
    maxtime = 20,
    target = "rules"
  ), 
  appearance = list(
    rhs = c("gestacao.filho.microcefalia=Confirmado"),
    default ="lhs"
  ),
  control = NULL
)

rules_a <- as(rules, "data.frame")
rm(zika_old, i, x)
