
# Script para leitura de bancos de dados diversos para geração de um data frame de uma única linha referente as informações do estado do aluno

# Ao receber este script esqueleto colocá-lo no repositório LOCAL Extensao, que deve ter sido clonado do GitHub
# Enviar o script esqueleto para o repositório REMOTO com o nome extensao-esqueleto.R

# Para realizar as tarefas da ETAPA 1, ABRIR ANTES uma branch de nome SINASC no main de Extensao e ir para ela
# Após os alunos concluírem a ETAPA 1 a professora orientará fazer o merge into main e depois abrir outro branch. Aguarde...


####################################
# ETAPA 1: BANCO DE DADOS DO SINASC
####################################

# A ALTERAÇÃO DO SCRIPT ESQUELETO - ETAPA 1 - DEVERÁ SER FEITA DENTRO DA BRANCH SINASC

# Tarefa 1. Leitura do banco de dados do SINASC 2015  com 3017668 linhas e 61 colunas
# nomeie o banco de dados como dados_sinasc
dados_sinasc=read.csv("SINASC_2015.csv",header= T ,sep=";")

# verificar se a leitura foi feita corretamente e a estrutura dos dados
head(dados_sinasc)
str(dados_sinasc)
names(dados_sinasc)



# Tarefa 2. Reduzir dados_sinasc apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sinasc_1
# as colunas serão 1, 4, 5, 6, 7, 12, 13, 14, 15, 19, 21, 22, 23, 24, 35, 38, 44, 46, 48, 59, 60, 61
# nomes das respectivas variáveis: CONTADOR, CODMUNNASC, LOCNASC, IDADEMAE, ESTCIVMAE, CODMUNRES, GESTACAO, GRAVIDEZ, PARTO,
# SEXO, APGAR5, RACACOR, PESO, IDANOMAL, ESCMAE2010, RACACORMAE, SEMAGESTAC, CONSPRENAT, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK
dados_sinasc_1 <- dados_sinasc [,c(1, 4, 5, 6, 7, 12, 13, 14, 15, 19, 21, 22, 23, 24, 35, 38, 44, 46, 48, 59, 60, 61)]

# Tarefa 3. Reduzir dados_sinasc_1 apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de dados como dados_sinasc_2
# Códigos das UF: 11: RO, 12: AC, 13: AM, 14: RR, 15: PA, 16: AP, 17: TO, 21: MA, 22: PI, 23: CE, 24: RN
# 25: PB, 26: PE, 27: AL, 28: SE, 29: BA, 31: MG, 32: ES, 33: RJ, 35: SP, 41: PR, 42: SC, 43: RS
# 50: MS, 51: MT, 52: GO, 53: DF 
dados_sinasc_2<- subset(
  dados_sinasc_1 ,
  substr(as.character(CODMUNRES), 1, 2)== "29")
head(dados_sinasc_2)

# observar abaixo o número de nascimentos por UF de residência para certificar-se que seu banco de dados está correto
# 11: 27918     12: 16980     13: 80097     14: 11409     15: 143657    16: 15750      17: 25110
# 21: 117564    22: 49253     23: 132516    24: 49099     25: 59089     26: 145024     27: 52257     28: 34917     29: 206655
# 31: 268305    32: 56941     33: 236960    35: 634026     
# 41: 160947    42: 97223     43: 148359
# 50: 44142     51: 56673     52: 100672    53: 46122 
table(substr(as.character(dados_sinasc_2$CODMUNRES), 1,2))


# Exportar o arquivo com o nome dados_sinasc_2.csv
write.csv(dados_sinasc_2, "dados_sinasc_2.csv")

# Ao concluir a Tarefa 3 da Etapa 1 commite e envie para o repositório REMOTO o script e dados_sinasc_2.csv com o comentário "Dados do estado UF (coloque o nome da UF) e script de sua obtenção"


# Tarefa 4. Verificar em dados_sinasc_2 a frequência das categorias das seguintes variáveis: LOCNASC, ESTCIVMAE, GESTACAO, GRAVIDEZ, PARTO,
# SEXO, APGAR5, RACACOR, IDANOMAL, ESCMAE2010, RACACORMAE, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK
str(dados_sinasc_2)
table(dados_sinasc_2$LOCNASC)
table(dados_sinasc_2$ESTCIVMAE)
table(dados_sinasc_2$GESTACAO)
table(dados_sinasc_2$GRAVIDEZ)
table(dados_sinasc_2$PARTO)
table(dados_sinasc_2$SEXO)
table(dados_sinasc_2$RACACOR)
table(dados_sinasc_2$IDANOMAL)
table(dados_sinasc_2$ESCMAE2010)
table(dados_sinasc_2$RACACORMAE)
table(dados_sinasc_2$TPAPRESENT)
table(dados_sinasc_2$TPROBSON)
table(dados_sinasc_2$PARIDADE)
table(dados_sinasc_2$KOTELCHUCK)

# Aproveitando para ver os valores das variáveis quantitativas
unique(dados_sinasc_2$IDADEMAE)
unique(dados_sinasc_2$CONSPRENAT)
unique(dados_sinasc_2$SEMAGESTAC)
unique(dados_sinasc_2$APGAR5)
unique(dados_sinasc_2$PESO)
summary(dados_sinasc_2$PESO)

# Tarefa 5. Atribuir para cada variável de dados_sinasc_2 como sendo NA a categoria de "Não informado ou Ignorado", geralmente com código 9
# KOTELCHUCK = 9 significa "não informado"   TPROBSON = 11 significa "não classificado por falta de informação"
# veja o dicionário do SINASC para identificar qual o código das categorias de cada variável

dados_sinasc_2$LOCNASC[dados_sinasc_2$LOCNASC == 9] = NA
dados_sinasc_2$IDADEMAE[dados_sinasc_2$IDADEMAE == 99] = NA
dados_sinasc_2$ESTCIVMAE[dados_sinasc_2$ESTCIVMAE == 9] = NA
dados_sinasc_2$GESTACAO[dados_sinasc_2$GESTACAO == 9] = NA
dados_sinasc_2$GRAVIDEZ[dados_sinasc_2$GRAVIDEZ == 9] = NA
dados_sinasc_2$PARTO[dados_sinasc_2$PARTO == 9] = NA
dados_sinasc_2$SEXO[dados_sinasc_2$SEXO == 0] = NA
dados_sinasc_2$APGAR5[dados_sinasc_2$APGAR5 == 99] = NA
dados_sinasc_2$PESO[dados_sinasc_2$PESO == 9999] = NA
dados_sinasc_2$IDANOMAL[dados_sinasc_2$IDANOMAL == 9] = NA
dados_sinasc_2$ESCMAE2010[dados_sinasc_2$ESCMAE2010 == 9] = NA
dados_sinasc_2$CONSPRENAT[dados_sinasc_2$CONSPRENAT == 99] = NA
dados_sinasc_2$TPAPRESENT[dados_sinasc_2$TPAPRESENT == 9] = NA
dados_sinasc_2$TPROBSON[dados_sinasc_2$TPROBSON == 11] = NA
dados_sinasc_2$KOTELCHUCK[dados_sinasc_2$KOTELCHUCK == 9] = NA
summary(dados_sinasc_2)

# Por curiosidade, verificando o tamanho dos banco de dados referente ao estado e aos municípios com e sem NAs
n_total_nasc_UF = nrow(dados_sinasc_2)
n_total_nasc_UF_sem_missing = sum(complete.cases(dados_sinasc_2))
n_total_nasc_MUN = tapply(rep(1, nrow(dados_sinasc_2)), dados_sinasc_2$CODMUNRES, sum)
n_total_nasc_MUN_sem_missing = tapply(complete.cases(dados_sinasc_2), dados_sinasc_2$CODMUNRES, sum)


# Tarefa 6. Atribuir legendas para as categorias das variáveis investigadas na etapa 4.
# Exemplo: dados_sinasc_2$KOTELCHUCK = factor(dados_sinasc_2$KOTELCHUCK, levels = c(1,2,3,4,5), 
# labels = c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado",  
# "Mais que adequado")

# ATENçÃO: 1. Na hora de escrever os labels, somente a primeira letra da palavra é maiúscula. Exemplo para SEXO: Feminino e Masculino
#          2. Nesta Tarefa 6 não crie novas variáveis no banco de dados

dados_sinasc_2$LOCNASC = factor(dados_sinasc_2$LOCNASC, levels = c(1,2,3,4), labels = c("Hospital", "Outros estabelecimentos de saúde", "Domicílio", "Outros"))
dados_sinasc_2$ESTCIVMAE = factor(dados_sinasc_2$ESTCIVMAE, levels = c(1,2,3,4,5), labels = c("Solteira", "Casada", "Viúva", "Separada judicialmente/divorciada", "União estável"))
dados_sinasc_2$GESTACAO = factor(dados_sinasc_2$GESTACAO, levels = c(1,2,3,4,5,6), labels = c("Menos de 22 semanas", "22 a 27 semanas", "28 a 31 semanas", "32 a 36 semanas", "37 a 41 semanas", "42 semanas e mais"))
dados_sinasc_2$GRAVIDEZ = factor(dados_sinasc_2$GRAVIDEZ, levels = c(1,2,3), labels = c("Única", "Dupla", "Tripla ou mais"))
dados_sinasc_2$PARTO = factor(dados_sinasc_2$PARTO, levels = c(1,2), labels = c("Vaginal", "Cesário"))
dados_sinasc_2$SEXO = factor(dados_sinasc_2$SEXO, levels = c(1,2), labels = c("Masculino", "Feminino"))
dados_sinasc_2$RACACOR = factor(dados_sinasc_2$RACACOR, levels = c(1,2,3,4,5), labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))
dados_sinasc_2$IDANOMAL = factor(dados_sinasc_2$IDANOMAL, levels = c(1,2), labels = c("Sim", "Não"))
dados_sinasc_2$ESCMAE2010 = factor(dados_sinasc_2$ESCMAE2010, levels = c(0,1,2,3,4,5), labels = c("Sem escolaridade", "Fundamental I (1ª a 4ª série)", "Fundamental II (5ª a 8ª série)", "Médio (antigo 2º grau)", "Superior incompleto", "Superior completo"))
dados_sinasc_2$RACACORMAE = factor(dados_sinasc_2$RACACORMAE, levels = c(1,2,3,4,5), labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))
dados_sinasc_2$TPAPRESENT = factor(dados_sinasc_2$TPAPRESENT, levels = c(1,2,3), labels = c("Cefálico", "Pélvica ou podálica", "Transversa"))
dados_sinasc_2$TPROBSON = factor(dados_sinasc_2$TPROBSON, levels = c(1,2,3,4,5,6,7,8,9,10), labels = c("Grupo 1", "Grupo 2", "Grupo 3", "Grupo 4", "Grupo 5", "Grupo 6", "Grupo 7", "Grupo 8", "Grupo 9", "Grupo 10"))
dados_sinasc_2$PARIDADE = factor(dados_sinasc_2$PARIDADE, levels = c(0,1), labels = c("Nulípara", "Multípara"))
dados_sinasc_2$KOTELCHUCK = factor(dados_sinasc_2$KOTELCHUCK, levels = c(1,2,3,4,5), labels = c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado", "Mais que adequado"))


# Tarefa 7. Categorizar as variáveis IDADEMAE, PESO e APGAR5
# nova variável: dados_sinasc_2$F_PESO com PESO: < 2500: Baixo peso, >=2500 e < 4000: Peso normal, >= 4000: Macrossomia
# nova variável dados_sinasc_2$F_IDADE com IDADEMAE: <15, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50+
# nova variável dados_sinasc_2$F_APGAR5 com APGAR5: < 7: Baixo, >= 7: Normal
# Atenção para casos de NA em IDADEMAE, PESO e APGAR5
# Ao categorizar as variáveis, garantir que sejam transformadas em tipo fator
dados_sinasc_2$F_IDADE = ifelse(dados_sinasc_2$IDADEMAE < 15, "<15",
                                ifelse(dados_sinasc_2$IDADEMAE <= 19, "15-19",
                                       ifelse(dados_sinasc_2$IDADEMAE <= 24, "20-24",
                                              ifelse(dados_sinasc_2$IDADEMAE <= 29, "25-29",
                                                     ifelse(dados_sinasc_2$IDADEMAE <= 34, "30-34",
                                                            ifelse(dados_sinasc_2$IDADEMAE <= 39, "35-39",
                                                                   ifelse(dados_sinasc_2$IDADEMAE <= 44, "40-44",
                                                                          ifelse(dados_sinasc_2$IDADEMAE <= 49, "45-49",
                                                                                 "50+"))))))))
dados_sinasc_2$F_IDADE = factor(dados_sinasc_2$F_IDADE,
                                levels = c("<15","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"), ordered = TRUE)

dados_sinasc_2$F_PESO = ifelse(dados_sinasc_2$PESO < 2500, "Baixo peso",
                               ifelse(dados_sinasc_2$PESO < 4000, "Peso normal",
                                      "Macrossomia"))
dados_sinasc_2$F_PESO = factor(dados_sinasc_2$F_PESO, levels = c("Baixo peso","Peso normal","Macrossomia"))

dados_sinasc_2$F_APGAR5 = ifelse(dados_sinasc_2$APGAR5 < 7, "Baixo", "Normal")
dados_sinasc_2$F_APGAR5 = factor(dados_sinasc_2$F_APGAR5,levels = c("Baixo","Normal"))

dados_sinasc_2$PERIG = ifelse(is.na(dados_sinasc_2$CODMUNNASC) | is.na(dados_sinasc_2$CODMUNRES), NA,
                              ifelse(dados_sinasc_2$CODMUNNASC == dados_sinasc_2$CODMUNRES, "Não", "Sim"))
dados_sinasc_2$PERIG = factor(dados_sinasc_2$PERIG, levels = c("Sim", "Não"))

dados_sinasc_2$ESTCIV = ifelse(dados_sinasc_2$ESTCIVMAE %in% c("Solteira", "Viúva", "Separada judicialmente/divorciada"), "Sem companheiro",
                               ifelse(dados_sinasc_2$ESTCIVMAE %in% c("Casada", "União estável"), "Com companheiro", NA))
dados_sinasc_2$ESTCIV = factor(dados_sinasc_2$ESTCIV, levels = c("Sem companheiro","Com companheiro"))


# Tarefa 8. Agregar ao banco de dados_sinasc_2 as informações PESO_P10 e PESO_P90 a partir de Tabela_PIG_Brasil.csv
# a Tabela PIG informa P10 e P90 dos pesos, de acordo com a idade gestacional
# criar nova variável referente ao peso, de acordo com a idade gestacional, conforme indicado abaixo
# nova variável apenas para casos de GRAVIDEZ única: dados_sinasc_2$F_PIG: PIG: PESO < PESO_P10, AIG: PESO_P10 <= PESO <= PESO_P90, GIG: PESO > PESO_P90
# Atenção para casos de NA em SEMAGESTAC, PESO ou SEXO. Lembre-se também que em dados_sinasc_2 SEXO está como fator com as categorias Feminino e Masculino.

# criar nova variável referente ao deslocamento materno para realizar o parto, chamado de peregrinação
# nova variável: dados_sinasc_2$PERIG: Não: CODMUNNASC igual a CODMUNRES, Sim: CODMUNNASC diferente de CODMUNRES
tabela_pig = read.csv("Tabela_PIG_Brasil.csv", header = TRUE, sep=";", stringsAsFactors = FALSE)
dados_sinasc_2$SEXO = as.character(dados_sinasc_2$SEXO)
tabela_pig$SEXO = as.character(tabela_pig$SEXO)
dados_sinasc_2$SEMAGESTAC = as.numeric(as.character(dados_sinasc_2$SEMAGESTAC))
tabela_pig$SEMAGESTAC = as.numeric(as.character(tabela_pig$SEMAGESTAC))
dados_sinasc_2$PESO_P10 = NULL
dados_sinasc_2$PESO_P90 = NULL
dados_sinasc_2 = merge(
  dados_sinasc_2,
  tabela_pig,
  by = c("SEMAGESTAC","SEXO"),
  all.x = TRUE
)

dados_sinasc_2$F_PIG = ifelse(
  dados_sinasc_2$GRAVIDEZ != "Única",
  NA,
  ifelse(
    is.na(dados_sinasc_2$PESO) |
      is.na(dados_sinasc_2$PESO_P10) |
      is.na(dados_sinasc_2$PESO_P90),
    NA,
    ifelse(
      dados_sinasc_2$PESO < dados_sinasc_2$PESO_P10, "PIG",
      ifelse(
        dados_sinasc_2$PESO <= dados_sinasc_2$PESO_P90, "AIG",
        "GIG"
      )
    )
  )
)
dados_sinasc_2$F_PIG = factor(dados_sinasc_2$F_PIG, levels = c("PIG","AIG","GIG"))

table(dados_sinasc_2$F_PIG, useNA = "always")

#Tarefas 9 e 10 (reformulada) do script esqueleto:
#Crie um banco de dados, de nome SINASC_UF.csv (Exemplo: SINASC_RJ.csv), contendo as 103 variáveis listadas no arquivo “Variáveis - Projeto - Tarefas 9 e 10 da Etapa 1.pdf”

#O banco final deverá possuir: 103 colunas, correspondentes às variáveis especificadas; n + 1 linhas, onde:
#n corresponde ao número de municípios distintos da UF em análise
#a primeira linha corresponde aos valores agregados para a UF como um todo;
#as demais linhas correspondem aos municípios da UF.
#As variáveis devem ser construídas a partir dos microdados do SINASC (dados_sinasc, dados_sinasc_1 e dados_sinasc_2, respeitando os nomes e a ordem especificados.
                                                                      
# 1. Base inicial 
base = data.frame(CODMUNRES = sort(unique(dados_sinasc_2$CODMUNRES)))

# TN - total de nascimentos
TN = as.data.frame(table(factor(dados_sinasc_2$CODMUNRES, levels = base$CODMUNRES)))
names(TN) = c("CODMUNRES","TN")
base = merge(base, TN, by = "CODMUNRES", all.x = TRUE)

# TNRC - completos nas 61 variáveis (Adaptado para Bahia: 29)
dados_UF = dados_sinasc[substr(as.character(dados_sinasc$CODMUNRES), 1, 2) == "29",]
dados_UF_comp = dados_UF[complete.cases(dados_UF), ]
TNRC = as.data.frame(table(factor(dados_UF_comp$CODMUNRES, levels = base$CODMUNRES)))
names(TNRC) = c("CODMUNRES","TNRC")
base = merge(base, TNRC, by = "CODMUNRES", all.x = TRUE)

# TNRCR - completos nas 22 variáveis (Adaptado para Bahia: 29)
dados_UF_1 = dados_sinasc_1[substr(as.character(dados_sinasc_1$CODMUNRES), 1, 2) == "29",]
dados_UF_1_comp = dados_UF_1[complete.cases(dados_UF_1), ]
TNRCR = as.data.frame(table(factor(dados_UF_1_comp$CODMUNRES, levels = base$CODMUNRES)))
names(TNRCR) = c("CODMUNRES","TNRCR")
base = merge(base, TNRCR, by = "CODMUNRES", all.x = TRUE)

# BLOCOS DE FREQUÊNCIAS CATEGÓRICAS 

# Idade categorizada (7 a 15)
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$F_IDADE, levels = c("<15","15-19","20-24","25-29", "30-34","35-39","40-44","45-49","50+")))
df = as.data.frame.matrix(tab)
names(df) = c("TGI_15","TGI_15_19","TGI_20_24","TGI_25_29", "TGI_30_34","TGI_35_39","TGI_40_44","TGI_45_49","TGI_50")
df$CODMUNRES = rownames(df)
base = merge(base, df, by = "CODMUNRES", all.x = TRUE)

# TGIF (Idade fértil 15-49) (16)
base$TGIF = base$TGI_15_19 + base$TGI_20_24 + base$TGI_25_29 + base$TGI_30_34 + base$TGI_35_39 + base$TGI_40_44 + base$TGI_45_49

# Escolaridade Mãe (22 a 27)
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$ESCMAE2010, levels = c("Sem escolaridade", "Fundamental I (1ª a 4ª série)", "Fundamental II (5ª a 8ª série)", "Médio (antigo 2º grau)", "Superior incompleto", "Superior completo")))
df = as.data.frame.matrix(tab)
names(df) = c("EM_S", "EM_FI", "EM_FII", "EM_M", "EM_SI", "EM_SC")
df$CODMUNRES = rownames(df)
base = merge(base, df, by = "CODMUNRES", all.x = TRUE)

# Raça/Cor Mãe (28 a 32)
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$RACACORMAE, levels = c("Branca", "Preta", "Amarela", "Parda", "Indígena")))
df = as.data.frame.matrix(tab)
names(df) = c("TGRC_B", "TGRC_PT", "TGRC_A", "TGRC_PD", "TGRC_I")
df$CODMUNRES = rownames(df)
base = merge(base, df, by = "CODMUNRES", all.x = TRUE)

# Companheiro (33 a 34)
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$ESTCIV, levels = c("Sem companheiro", "Com companheiro")))
df = as.data.frame.matrix(tab)
names(df) = c("TGSC", "TGCC")
df$CODMUNRES = rownames(df)
base = merge(base, df, by = "CODMUNRES", all.x = TRUE)

# Paridade (35 a 36)
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$PARIDADE, levels = c("Nulípara", "Multípara")))
df = as.data.frame.matrix(tab)
names(df) = c("TGPRI", "TGNPRI")
df$CODMUNRES = rownames(df)
base = merge(base, df, by = "CODMUNRES", all.x = TRUE)

# Gravidez Única e Gemelar (37 a 38)
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$GRAVIDEZ, levels = c("Única", "Dupla", "Tripla ou mais")))
df = as.data.frame.matrix(tab)
names(df) = c("TGU", "TGG1", "TGG2")
df$CODMUNRES = rownames(df)
base = merge(base, df, by = "CODMUNRES", all.x = TRUE)
base$TGG = base$TGG1 + base$TGG2
base$TGG1 = NULL; base$TGG2 = NULL

# Duração da Gestação (39 a 44)
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$GESTACAO, levels = c("Menos de 22 semanas", "22 a 27 semanas", "28 a 31 semanas", "32 a 36 semanas", "37 a 41 semanas", "42 semanas e mais")))
df = as.data.frame.matrix(tab)
names(df) = c("TGD_22", "TGD_22_27", "TGD_28_31", "TGD_32_36", "TGD_37_41", "TGD_42")
df$CODMUNRES = rownames(df)
base = merge(base, df, by = "CODMUNRES", all.x = TRUE)

# Pré-termo, Termo, Pós-termo (45 a 47)
base$TGD_PRT = base$TGD_22 + base$TGD_22_27 + base$TGD_28_31 + base$TGD_32_36
base$TGD_AT = base$TGD_37_41
base$TGD_PST = base$TGD_42

# Kotelchuck (53 a 57)
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$KOTELCHUCK, levels = c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado", "Mais que adequado")))
df = as.data.frame.matrix(tab)
names(df) = c("TKC_NR", "TKC_ID", "TKC_IT", "TKC_AD", "TKC_MAD")
df$CODMUNRES = rownames(df)
base = merge(base, df, by = "CODMUNRES", all.x = TRUE)

# Peregrinação (58 a 59)
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$PERIG, levels = c("Sim", "Não")))
df = as.data.frame.matrix(tab)
names(df) = c("TGPRG_S", "TGPRG_N")
df$CODMUNRES = rownames(df)
base = merge(base, df, by = "CODMUNRES", all.x = TRUE)

# Tipo de Parto (60 a 61)
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$PARTO, levels = c("Vaginal", "Cesário")))
df = as.data.frame.matrix(tab)
names(df) = c("TPV", "TPC")
df$CODMUNRES = rownames(df)
base = merge(base, df, by = "CODMUNRES", all.x = TRUE)

# Apresentação (62 a 64)
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$TPAPRESENT, levels = c("Cefálico", "Pélvica ou podálica", "Transversa")))
df = as.data.frame.matrix(tab)
names(df) = c("TRAP_C", "TRAP_P", "TRAP_T")
df$CODMUNRES = rownames(df)
base = merge(base, df, by = "CODMUNRES", all.x = TRUE)

# Grupos de Robson (65 a 74)
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$TPROBSON, levels = paste0("Grupo ", 1:10)))
df = as.data.frame.matrix(tab)
names(df) = paste0("TGROB_", 1:10)
df$CODMUNRES = rownames(df)
base = merge(base, df, by = "CODMUNRES", all.x = TRUE)

# Local Nascimento (75 a 79)
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$LOCNASC, levels = c("Hospital", "Outros estabelecimentos de saúde", "Domicílio", "Outros")))
df = as.data.frame.matrix(tab)
names(df) = c("TNLOC_H", "TNLOC_ES", "TNLOC_D", "TNLOC_O")
df$CODMUNRES = rownames(df)
base = merge(base, df, by = "CODMUNRES", all.x = TRUE)
base$TNLOC_AI = 0 # Adicionado como 0 (Aldeias Indígenas)

# Sexo RN (80 a 81)
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$SEXO, levels = c("Masculino", "Feminino")))
df = as.data.frame.matrix(tab)
names(df) = c("TRS_M", "TRS_F")
df$CODMUNRES = rownames(df)
base = merge(base, df, by = "CODMUNRES", all.x = TRUE)

# Raça/Cor RN (82 a 86)
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$RACACOR, levels = c("Branca", "Preta", "Amarela", "Parda", "Indígena")))
df = as.data.frame.matrix(tab)
names(df) = c("TRRC_B", "TRRC_PT", "TRRC_A", "TRRC_PD", "TRRC_I")
df$CODMUNRES = rownames(df)
base = merge(base, df, by = "CODMUNRES", all.x = TRUE)

# Peso Categorias (87 a 89)
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$F_PESO, levels = c("Baixo peso", "Peso normal", "Macrossomia")))
df = as.data.frame.matrix(tab)
names(df) = c("TRP_BP", "TRP_N", "TRP_M")
df$CODMUNRES = rownames(df)
base = merge(base, df, by = "CODMUNRES", all.x = TRUE)

# PIG/AIG/GIG (95 a 97)
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$F_PIG, levels = c("PIG", "AIG", "GIG")))
df = as.data.frame.matrix(tab)
names(df) = c("TRPIG_P", "TRPIG_A", "TRPIG_G")
df$CODMUNRES = rownames(df)
base = merge(base, df, by = "CODMUNRES", all.x = TRUE)

# Apgar 5 (98 a 99)
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$F_APGAR5, levels = c("Baixo", "Normal")))
df = as.data.frame.matrix(tab)
names(df) = c("TRAPG5_B", "TRAPG5_N")
df$CODMUNRES = rownames(df)
base = merge(base, df, by = "CODMUNRES", all.x = TRUE)

# Anomalias Congênitas (102 a 103)
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$IDANOMAL, levels = c("Sim", "Não")))
df = as.data.frame.matrix(tab)
names(df) = c("TRAC", "TRSAC")
df$CODMUNRES = rownames(df)
base = merge(base, df, by = "CODMUNRES", all.x = TRUE)


# BLOCOS DE MEDIDAS (aggregate, quantile -> merge)

# Idade da mãe (17 a 21)
p_idade = aggregate(IDADEMAE ~ CODMUNRES, dados_sinasc_2, function(x) quantile(x, probs = c(0.25,0.5,0.75), na.rm = TRUE))
p_idade = do.call(data.frame, p_idade)
names(p_idade) = c("CODMUNRES","IM_P25","IM_P50", "IM_P75")
p_idade[, c("IM_P25","IM_P50","IM_P75")] = round(p_idade[, c("IM_P25","IM_P50","IM_P75")], 2)
base = merge(base, p_idade, by="CODMUNRES", all.x=TRUE)

media_idade = aggregate(IDADEMAE ~ CODMUNRES, dados_sinasc_2, mean, na.rm = TRUE)
media_idade$IDADEMAE = round(media_idade$IDADEMAE, 2)
names(media_idade)[2] = "IM_MD"
dp_idade = aggregate(IDADEMAE ~ CODMUNRES, dados_sinasc_2, sd, na.rm = TRUE)
dp_idade$IDADEMAE = round(dp_idade$IDADEMAE, 2)
names(dp_idade)[2] = "IM_DP"
temp = merge(media_idade, dp_idade, by = "CODMUNRES")
base = merge(base, temp, by = "CODMUNRES", all.x = TRUE)

# Duração da Gestação em semanas (48 a 52)
p_dg = aggregate(SEMAGESTAC ~ CODMUNRES, dados_sinasc_2, function(x) quantile(x, probs = c(0.25,0.5,0.75), na.rm = TRUE))
p_dg = do.call(data.frame, p_dg)
names(p_dg) = c("CODMUNRES","DG_P25","DG_P50", "DG_P75")
p_dg[, c("DG_P25","DG_P50","DG_P75")] = round(p_dg[, c("DG_P25","DG_P50","DG_P75")], 2)
base = merge(base, p_dg, by="CODMUNRES", all.x=TRUE)

media_dg = aggregate(SEMAGESTAC ~ CODMUNRES, dados_sinasc_2, mean, na.rm = TRUE)
media_dg$SEMAGESTAC = round(media_dg$SEMAGESTAC, 2)
names(media_dg)[2] = "DG_MD"
dp_dg = aggregate(SEMAGESTAC ~ CODMUNRES, dados_sinasc_2, sd, na.rm = TRUE)
dp_dg$SEMAGESTAC = round(dp_dg$SEMAGESTAC, 2)
names(dp_dg)[2] = "DG_DP"
temp = merge(media_dg, dp_dg, by = "CODMUNRES")
base = merge(base, temp, by = "CODMUNRES", all.x = TRUE)

# Peso (90 a 94)
p_peso = aggregate(PESO ~ CODMUNRES, dados_sinasc_2, function(x) quantile(x, probs = c(0.25,0.5,0.75), na.rm = TRUE))
p_peso = do.call(data.frame, p_peso)
names(p_peso) = c("CODMUNRES","PESO_P25","PESO_P50", "PESO_P75")
p_peso[, c("PESO_P25","PESO_P50","PESO_P75")] = round(p_peso[, c("PESO_P25","PESO_P50","PESO_P75")], 2)
base = merge(base, p_peso, by="CODMUNRES", all.x=TRUE)

media_peso = aggregate(PESO ~ CODMUNRES, dados_sinasc_2, mean, na.rm = TRUE)
media_peso$PESO = round(media_peso$PESO, 2)
names(media_peso)[2] = "PESO_MD"
dp_peso = aggregate(PESO ~ CODMUNRES, dados_sinasc_2, sd, na.rm = TRUE)
dp_peso$PESO = round(dp_peso$PESO, 2)
names(dp_peso)[2] = "PESO_DP"
temp = merge(media_peso, dp_peso, by = "CODMUNRES")
base = merge(base, temp, by = "CODMUNRES", all.x = TRUE)

# Apgar 5 Medidas (100 a 101)
media_apg = aggregate(APGAR5 ~ CODMUNRES, dados_sinasc_2, mean, na.rm = TRUE)
media_apg$APGAR5 = round(media_apg$APGAR5, 2)
names(media_apg)[2] = "APG5_MD"
dp_apg = aggregate(APGAR5 ~ CODMUNRES, dados_sinasc_2, sd, na.rm = TRUE)
dp_apg$APGAR5 = round(dp_apg$APGAR5, 2)
names(dp_apg)[2] = "APG5_DP"
temp = merge(media_apg, dp_apg, by = "CODMUNRES")
base = merge(base, temp, by = "CODMUNRES", all.x = TRUE)

# FINALIZAÇÃO: LINHA DO ESTADO E EXPORTAÇÃO


# Colunas contínuas (que não somam)
cols_continuas = c("IM_MD","IM_DP","IM_P25","IM_P50","IM_P75",
                   "DG_MD","DG_DP","DG_P25","DG_P50","DG_P75",
                   "PESO_MD","PESO_DP","PESO_P25","PESO_P50","PESO_P75",
                   "APG5_MD","APG5_DP")

cols_contagem = setdiff(names(base), c("CODMUNRES", cols_continuas))

# Substituir NA por 0
base[cols_contagem][is.na(base[cols_contagem])] = 0

# Linha da UF
linha_estado = base[1, ]
linha_estado[,] = NA
linha_estado[cols_contagem] = colSums(base[cols_contagem], na.rm = TRUE)

# Recalculo manual das médias para o estado todo
linha_estado$IM_MD = round(mean(dados_sinasc_2$IDADEMAE, na.rm = TRUE), 2)
linha_estado$IM_DP = round(sd(dados_sinasc_2$IDADEMAE, na.rm = TRUE), 2)
q = round(quantile(dados_sinasc_2$IDADEMAE, probs = c(0.25,0.5,0.75), na.rm = TRUE), 2)
linha_estado$IM_P25 = q[1]; linha_estado$IM_P50 = q[2]; linha_estado$IM_P75 = q[3]

linha_estado$DG_MD = round(mean(dados_sinasc_2$SEMAGESTAC, na.rm = TRUE), 2)
linha_estado$DG_DP = round(sd(dados_sinasc_2$SEMAGESTAC, na.rm = TRUE), 2)
q = round(quantile(dados_sinasc_2$SEMAGESTAC, probs = c(0.25,0.5,0.75), na.rm = TRUE), 2)
linha_estado$DG_P25 = q[1]; linha_estado$DG_P50 = q[2]; linha_estado$DG_P75 = q[3]

linha_estado$PESO_MD = round(mean(dados_sinasc_2$PESO, na.rm = TRUE), 2)
linha_estado$PESO_DP = round(sd(dados_sinasc_2$PESO, na.rm = TRUE), 2)
q = round(quantile(dados_sinasc_2$PESO, probs = c(0.25,0.5,0.75), na.rm = TRUE), 2)
linha_estado$PESO_P25 = q[1]; linha_estado$PESO_P50 = q[2]; linha_estado$PESO_P75 = q[3]

linha_estado$APG5_MD = round(mean(dados_sinasc_2$APGAR5, na.rm = TRUE), 2)
linha_estado$APG5_DP = round(sd(dados_sinasc_2$APGAR5, na.rm = TRUE), 2)

linha_estado$CODMUNRES = 29 # Bahia

# Juntar tudo
SINASC_BA = rbind(linha_estado, base)
SINASC_BA$NIVEL = c("UF", rep("MUNICIPIO", nrow(SINASC_BA)-1))
SINASC_BA$ANO = 2015

# ORDEM FINAL EXATA DO PDF
ordem_pdf = c("ANO","NIVEL","CODMUNRES","TN","TNRC","TNRCR",
              "TGI_15","TGI_15_19","TGI_20_24","TGI_25_29","TGI_30_34","TGI_35_39","TGI_40_44","TGI_45_49","TGI_50","TGIF",
              "IM_P25","IM_P50","IM_P75","IM_MD","IM_DP","EM_S","EM_FI","EM_FII","EM_M","EM_SI","EM_SC",
              "TGRC_B","TGRC_PT","TGRC_A","TGRC_PD","TGRC_I","TGSC","TGCC","TGPRI","TGNPRI",
              "TGU","TGG","TGD_22","TGD_22_27","TGD_28_31","TGD_32_36","TGD_37_41","TGD_42","TGD_PRT","TGD_AT","TGD_PST",
              "DG_P25","DG_P50","DG_P75","DG_MD","DG_DP","TKC_NR","TKC_ID","TKC_IT","TKC_AD","TKC_MAD",
              "TGPRG_S","TGPRG_N","TPV","TPC","TRAP_C","TRAP_P","TRAP_T",
              "TGROB_1","TGROB_2","TGROB_3","TGROB_4","TGROB_5","TGROB_6","TGROB_7","TGROB_8","TGROB_9","TGROB_10",
              "TNLOC_H","TNLOC_ES","TNLOC_D","TNLOC_O","TNLOC_AI","TRS_M","TRS_F",
              "TRRC_B","TRRC_PT","TRRC_A","TRRC_PD","TRRC_I","TRP_BP","TRP_N","TRP_M",
              "PESO_P25","PESO_P50","PESO_P75","PESO_MD","PESO_DP","TRPIG_P","TRPIG_A","TRPIG_G",
              "TRAPG5_B","TRAPG5_N","APG5_MD","APG5_DP","TRAC","TRSAC")

SINASC_BA = SINASC_BA[, ordem_pdf]

#Verificando dados
dim(SINASC_BA)
head(SINASC_BA)
names(SINASC_BA)

# Tarefa 11: Exporte o banco de dados com o nome SINASC_UF.csv

# Exportar
write.csv(SINASC_BA, "SINASC_BA.csv", row.names = FALSE)

# Ao terminar a ETAPA 1 commite e envie para o repositório REMOTO com o comentário "Dados da UF e Script Etapa 1"



##################################
# ETAPA 2: BANCO DE DADOS DO SIM
##################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 2

# Tarefa 1. Leitura do banco de dados Mortalidade_Geral_2015 do SIM 2015 com 1216475 linhas e 87 colunas
# verificar se a leitura foi feita corretamente e a estrutura dos dados
# nomeie o banco de dados como dados_sim


# Tarefa 2. Reduzir dados_sim apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sim_1
# as colunas serão (a informar)
# nomes das respectivas variáveis: CONTADOR, TIPOBITO, CODMUNNATU, IDADE,  SEXO,  RACACOR,  ESTCIV, ESC2010, 
# CODMUNRES,  LOCOCOR, CODMUNOCOR, TPMORTEOCO,  OBITOGRAV, OBITOPUERP, CAUSABAS, CAUSABAS_O, TPOBITOCOR, MORTEPARTO



#####################################################
# ETAPA 3: OUTROS BANCOS DE DADOS: IBGE, SNIS, ...
#####################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 3

# Tarefa 1. Acesso aos bancos de dados e obtenção da informação



#####################################################################################################
# ETAPA 4: GERAR BANCO DE DADOS FINAL DO ESTADO, BASEADO NAS ANÁLISES DE SINASC, SIM, IBGE, SNIS,...
######################################################################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 4

# Cada aluno gerar um dataframe de uma única linha (referente ao seu estado) com as variáveis na ordem indicada pela professora



############################################################################################
# ETAPA 5: EMPILHAMENTO DOS DATAFRAMES DE CADA ESTADO, GERANDO UM DATAFRAME DE 27 LINHAS
############################################################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 5

# 1. Enviar arquivos para as pastas do repositório da Professora no GitHUb
# 2. A professora fará o empilhamentos dos dataframes

