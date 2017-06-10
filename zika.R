################################################################################
# criando DF com os dados do .csv selecionados para utilização
################################################################################

zika_old = read.csv("linkage.csv", header = TRUE, sep = ";", fileEncoding = 'utf-8')
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
  "filho.ano_nasc",
  "filho.mes_nasc",
  "filho.peso",
  "filho.comprimento",
  "filho.cranio.perimetro",
  "filho.cranio.diametro",
  "filho.torax.perimetro",
  "gestacao.tipo",
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

zika_new$filho.comprimento <- as.numeric(sub(",", ".", zika_new$filho.comprimento))
zika_new$filho.cranio.perimetro <- as.numeric(sub(",", ".", zika_new$filho.cranio.perimetro))
zika_new$filho.cranio.diametro <- as.numeric(sub(",", ".", zika_new$filho.cranio.diametro))
zika_new$filho.torax.perimetro <- as.numeric(sub(",", ".", zika_new$filho.torax.perimetro))

################################################################################
# alguns registros das colunas que representam datas apresentavam inconsistência
# a inconsistência era notada por alguns registros possuirem apenas 7 dígitos
# o motivo disso era por conta do primeiro caractere ser "0"
# para corrigir isso, deve-se adicionar um "0" para os registros com 7 dígitos
# após isso, deve-se alterar o tipo dessa coluna para "date"
################################################################################

zika_new$filho.ano_nasc[which(nchar(zika_new$filho.ano_nasc) == 7)] <- paste(
  "0", 
  zika_new$filho.ano_nasc[which(nchar(zika_new$filho.ano_nasc) == 7)], 
  sep = ""
)
zika_new$filho.ano_nasc <- as.Date(zika_new$filho.ano_nasc, "%d%m%Y")

zika_new$filho.mes_nasc[which(nchar(zika_new$filho.mes_nasc) == 7)] <- paste(
  "0", 
  zika_new$filho.mes_nasc[which(nchar(zika_new$filho.mes_nasc) == 7)], 
  sep = ""
)
zika_new$filho.mes_nasc <- as.Date(zika_new$filho.mes_nasc, "%d%m%Y")

################################################################################
# pegando a parte do m?s/ano para as respectivas colunas
################################################################################

zika_new$filho.ano_nasc <- as.integer(substr(zika_new$filho.ano_nasc, 1, 4))
zika_new$filho.mes_nasc <- as.integer(substr(zika_new$filho.mes_nasc, 6, 7))

zika_new$gestacao.filho.microcefalia <- substr(
  zika_new$gestacao.filho.microcefalia, 
  4, 
  nchar(as.character(zika_new$gestacao.filho.microcefalia))
)

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
# discretizando colunas
################################################################################

zika_new$mae.municipio <- as.factor(zika_new$mae.municipio)
zika_new$gestacao.filho.microcefalia <- as.factor(zika_new$gestacao.filho.microcefalia)
zika_new$gestacao.doenca.exantema <- as.factor(zika_new$gestacao.doenca.exantema)
zika_new$gestacao.doenca.exantema.trimestre <- as.factor(zika_new$gestacao.doenca.exantema.trimestre)
zika_new$filho.ano_nasc <- as.factor(zika_new$filho.ano_nasc)
zika_new$filho.mes_nasc <- as.factor(zika_new$filho.mes_nasc)
zika_new$gestacao.filho.classificacao <- as.factor(zika_new$gestacao.filho.classificacao)

levels(zika_new$gestacao.filho.classificacao)[4] <- "A Termo"

zika_new$mae.idade <- 
  ordered(
    cut(
      zika_new$mae.idade, 
      c(0, 12, 18, 24, 65)
    ), 
    labels = c(
      "Crian?a", 
      "Adolescente", 
      "Jovem-Adulto", 
      "Adulto"
    )
  )

zika_new$filho.peso <- cut(
  zika_new$filho.peso, 
  hist(zika_new$filho.peso)$breaks,
  dig.lab = 10
)

zika_new$filho.comprimento <- cut(
  zika_new$filho.comprimento, 
  hist(zika_new$filho.comprimento)$breaks,
  dig.lab = 10
)

zika_new$filho.cranio.perimetro <- cut(
  zika_new$filho.cranio.perimetro, 
  hist(zika_new$filho.cranio.perimetro)$breaks,
  dig.lab = 10
)

zika_new$filho.cranio.diametro <- cut(
  zika_new$filho.cranio.diametro, 
  hist(zika_new$filho.cranio.diametro)$breaks,
  dig.lab = 10 
)

zika_new$filho.torax.perimetro <- cut(
  zika_new$filho.torax.perimetro, 
  hist(zika_new$filho.torax.perimetro)$breaks,
  dig.lab = 10 
)

################################################################################
# removendo conte?do tempor?rio
################################################################################

zika_new <- droplevels(zika_new)
str(zika_new)
table(zika_new$gestacao.filho.classificacao)
