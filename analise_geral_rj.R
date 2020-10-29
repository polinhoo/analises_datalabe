options(scipen=999)
##trazendo as infos dos municipios
rj <- read.csv("População Residente total, em relação ao Sexo, Raça ou Cor, Faixa Etária e Escolaridade - RJ - Página1.csv")

##como organizar os nomes
library(stringr)
rj$Município <- toupper(rj$Município)
rj$Município <-  sub(" \\(.*\\)", "", rj$Município)
table(rj$Município)
#retirando ultima linha
qt <- nrow(rj)-1
rj <- rj[(1:qt),]

##fazendo a de sexo
sexo_rj <- rj[,c(1,6,7)]

sexo_rj$total <- sexo_rj$Homens + sexo_rj$Mulheres

sexo_rj$perc_mulher <- sexo_rj$Mulheres/sexo_rj$total

##trazendo dados da candidatura
cand_sexo <- read.csv("territorios_genero.csv")
table(cand_sexo$NM_UE)
cand_sexo$perc_cand_mulher <- cand_sexo$FEMININO/cand_sexo$TOTAL

##juntando as duas bases
anal_sexo <- merge(cand_sexo, sexo_rj, by.x="NM_UE", by.y = "Município")

anal_sexo$repr_mul <- anal_sexo$perc_cand_mulher/anal_sexo$perc_mulher

##plotando um grafico da representatividade DE MULHERES
library(ggplot2)
##todos os municípios com repr  feminina
ggplot(anal_sexo, aes(x = reorder(NM_UE, -repr_mul), y = repr_mul)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6)+
  geom_hline(aes(yintercept=1), colour="red")+
  theme(text = element_text(size=5))+
  labs(x="Municípios do RJ", y="Índice de representatividade")+
  coord_flip()

##ordenando
sexo_outra <- anal_sexo[order(anal_sexo$repr_mul),]
pior30 <- sexo_outra[1:29,]
med30 <- sexo_outra[30:59,]
mel30 <- sexo_outra[60:92,]

##os 30 piores
ggplot(pior30, aes(x = reorder(NM_UE, -repr_mul), y = repr_mul)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6)+
    geom_hline(aes(yintercept=1), colour="#990000")+
  theme(text = element_text(size=10))+
  labs(x="30 piores Municípios do RJ", y="Índice de representatividade")+
  coord_flip()

##os 30 medios
ggplot(med30, aes(x = reorder(NM_UE, -repr_mul), y = repr_mul)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6)+
  geom_hline(aes(yintercept=1), colour="#990000")+
  theme(text = element_text(size=10))+
  labs(x="30 medianos Municípios do RJ", y="Índice de representatividade")+
  coord_flip()

##os 30 melhores
ggplot(mel30, aes(x = reorder(NM_UE, -repr_mul), y = repr_mul)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6)+
  geom_hline(aes(yintercept=1), colour="#990000")+
  theme(text = element_text(size=10))+
  labs(x="30 melhores Municípios do RJ", y="Índice de representatividade")+
  coord_flip()


##raca
raca_cand <- read.csv("territorios_raca.csv")
names(rj)
rj_raca <- rj[,c(1,8,10,12)]

anal_raca <- merge(rj_raca, raca_cand, by.x="Município", by.y = "NM_UE")

anal_raca$tt_cand <- rowSums(anal_raca[,c(5:9)])

anal_raca$perc_cand_preta <- anal_raca$PRETA/anal_raca$tt_cand

anal_raca$perc_pop_preta <- anal_raca$Preta/anal_raca$Total.de.Raça.ou.Cor

anal_raca$repr_preta <- anal_raca$perc_cand_preta/anal_raca$perc_pop_preta

##criando as mesmas cat da preta para a negra
#somando preto e parto pop
anal_raca$pop_negra <- anal_raca$Preta+anal_raca$Parda

anal_raca$cand_negra <- anal_raca$PRETA+anal_raca$PARDA

anal_raca$perc_pop_negra <- anal_raca$pop_negra/anal_raca$Total.de.Raça.ou.Cor

anal_raca$perc_cand_negra <- anal_raca$cand_negra/anal_raca$tt_cand

anal_raca$repr_negra <- anal_raca$perc_cand_negra/anal_raca$perc_pop_negra

summary(anal_raca$repr_negra)

##GRAFICO DE REPR_NEGRA
ggplot(anal_raca, aes(x = reorder(Município, -repr_negra), y = repr_negra)) +
  geom_bar(stat="identity", fill="bisque4", alpha=1)+
  geom_hline(aes(yintercept=1), colour="red")+
  ylim(0,3)+
  theme(text = element_text(size=5))+
  labs(x="Municípios do RJ", y="Índice de representatividade de raça")+
  coord_flip()
##GRAFICO DE REPR_PRETA
ggplot(anal_raca, aes(x = reorder(Município, -repr_negra), y = repr_preta)) +
  geom_bar(stat="identity", fill="black", alpha=1)+
  geom_hline(aes(yintercept=1), colour="red")+
  ylim(0,3)+
  theme(text = element_text(size=5))+
  labs(x="Municípios do RJ", y="Índice de representatividade de raça")+
  coord_flip()

##plotando a repr_preta pela repr_negra
ggplot(anal_raca, aes(repr_preta, repr_negra))+
  geom_line()+
  geom_smooth()
##resumo da pop negra e preta
summary(anal_raca$perc_pop_negra)
summary(anal_raca$perc_pop_preta)
##resumo da cand negra e preta
summary(anal_raca$perc_cand_negra)
summary(anal_raca$perc_cand_preta)
##organizando por ordem de rep preta
raca_outra <- anal_raca[order(anal_raca$repr_negra),]
pior30 <- raca_outra[1:30,]
med30 <- raca_outra[31:60,]
mel30 <- raca_outra[61:92,]

##grafico dos 30 piores
ggplot(pior30, aes(x = reorder(Município, -repr_negra), y = repr_negra)) +
  geom_bar(stat="identity", fill="black", alpha=.6)+
  geom_hline(aes(yintercept=1), colour="red")+
  theme(text = element_text(size=10))+
  labs(x="30 piores Municípios do RJ", y="Índice de representatividade de cand.negras")+
  coord_flip()

##grafico dos 30 medianos
ggplot(med30, aes(x = reorder(Município, -repr_negra), y = repr_negra)) +
  geom_bar(stat="identity", fill="black", alpha=.6)+
  geom_hline(aes(yintercept=1), colour="red")+
  theme(text = element_text(size=10))+
  labs(x="30 medianos Municípios do RJ", y="Índice de representatividade de cand.negras")+
  coord_flip()

##grafico dos 30 melhores
ggplot(mel30, aes(x = reorder(Município, -repr_negra), y = repr_negra)) +
  geom_bar(stat="identity", fill="bisque4", alpha=1)+
  geom_hline(aes(yintercept=1), colour="red")+
  ylim(0,3)+
  theme(text = element_text(size=10))+
  labs(x="32 melhores Municípios do RJ", y="Índice de representatividade de cand.negras")+
  coord_flip()
##grafico dos 30 melhores pretos
ggplot(mel30, aes(x = reorder(Município, -repr_negra), y = repr_preta)) +
  geom_bar(stat="identity", fill="black", alpha=1)+
  geom_hline(aes(yintercept=1), colour="red")+
  ylim(0,3)+
  theme(text = element_text(size=10))+
  labs(x="32 melhores Municípios do RJ", y="Índice de representatividade de cand.pretas")+
  coord_flip()
##
##base original 
total <- read.csv("consulta_cand_2016_RJ.csv", sep=";", fileEncoding = "latin1")

table(total$NM_TIPO_ELEICAO) #22.297 eleicao ordinaria, 108 suplementar
##ficando apenas com as eleicoes ordinarias
total <- subset(total, NM_TIPO_ELEICAO=="ELEIÇÃO ORDINÁRIA")
#ficou com apenas 22.297

table(total$DS_CARGO)
#499 prefeito, 21395 vereador, 511 vice prefeito

table(total$DS_DETALHE_SITUACAO_CAND)
#20609 deferidos 
##ficando apenas com as candidaturas deferidas
total <- subset(total, DS_DETALHE_SITUACAO_CAND=="DEFERIDO")
##FICOU COM APENAS 20609

library(epiDisplay)
##análise de gênero
tab1(total$DS_GENERO, graph=F)
#6359 feminino

tab1(total$DS_GRAU_INSTRUCAO, graph=F)
#4542 superior completo

##racacor
tab1(total$DS_COR_RACA, graph=F)
#2937 preta


#unificando mulher, preta de ens superior
raro <- subset(total, DS_COR_RACA=="PRETA" & DS_GRAU_INSTRUCAO== "SUPERIOR COMPLETO" &
                 DS_GENERO=="FEMININO")
176/20609
#0,8% dessas candidaturas
raro$NM_CANDIDATO <- as.character(raro$NM_CANDIDATO)
names(raro)
table(raro$SG_UE)

raro_redu <- raro[,c(13,17,18,15,21,22,26,30)]

library(epiDisplay)
raro_redu$NM_PARTIDO <- as.character(raro_redu$NM_PARTIDO)
##partidos do evento raro
tab1(raro_redu$NM_PARTIDO, graph=F, sort.group = "decreasing")
##quais os cargos desse evento raro
tab1(raro_redu$DS_CARGO, graph=F)

##quais os municipios que aconteceram
raro_redu$NM_UE <- as.character(raro_redu$NM_UE)
tab1(raro_redu$NM_UE, graph=F, sort.group = "decreasing")

write.table(raro_redu, "evento_raro.csv", sep="|", quote = F, row.names = F)

##base votacao
votacao <- read.csv("votacao_candidato_munzona_2016_RJ.csv", sep=";", fileEncoding = "latin1")
names(votacao)
#vendo os tipos de eleicao 
tab1(votacao$NM_TIPO_ELEICAO, graph=F, sort.group = "decreasing")
##214752 eleicao ordinaria
#ficando apenas com as eleicoes ordinarias
votacao <- subset(votacao, NM_TIPO_ELEICAO=="Eleição Ordinária")

#situacao candidatura
table(votacao$DS_SITUACAO_CANDIDATURA)
#204078 candidatos aptos
votacao <- subset(votacao, DS_SITUACAO_CANDIDATURA=="APTO")

##DS_DETALHE SITU
table(votacao$DS_DETALHE_SITUACAO_CAND)
##203579 deferidas
#ficando so com as deferidas
votacao <- subset(votacao, DS_DETALHE_SITUACAO_CAND=="DEFERIDO")

##SITUACAO DO CANDIDATO NAQUELE TURNO
table(votacao$DS_SIT_TOT_TURNO)

##TABELA CRUZANDO A RACA EM GERAL E A SITUACAO
tab1(votacao$NR_CANDIDATO, graph = F, sort.group = "decreasing")
##o numero do candidato nao serve para dar merge
#existe o sq_candidato que é um número interno gerado
library(dplyr)
##fazendo numero de votos para cada candidato
votacao %>% 
  group_by(SQ_CANDIDATO) %>% 
  summarize(máximo=max(QT_VOTOS_NOMINAIS, na.rm = T))

##vendo quais candidatos foram eleitos 
votacao$status <- ""

votacao$status[which(votacao$DS_SIT_TOT_TURNO=="ELEITO POR MÉDIA")] <- "ELEITO"
votacao$status[which(votacao$DS_SIT_TOT_TURNO=="ELEITO POR QP")] <- "ELEITO"
votacao$status[which(votacao$DS_SIT_TOT_TURNO=="ELEITO")] <- "ELEITO"
votacao$status[which(votacao$DS_SIT_TOT_TURNO=="SUPLENTE")] <- "NÃO ELEITO"
votacao$status[which(votacao$DS_SIT_TOT_TURNO=="NÃO ELEITO")] <- "NÃO ELEITO"
table(votacao$status)
table(votacao$DS_SIT_TOT_TURNO)


base2 <- merge(votacao, total, by="SQ_CANDIDATO", all.y =  T, all.x=F)
names(votacao)
base2 <- base2[!duplicated(base2),]

##analisando apenas as candidaturas de vereadores, e portanto apenas 1º turno
#cuidado nao reproduzir, não cheguei aqui ainda!
#names(base2)
#table(base2$NR_TURNO.x)
#primturno <- subset(base2, NR_TURNO.x=="1")
#names(primturno)
#resultado_raca <- primturno %>% group_by(NM_UE.x, DS_COR_RACA, status) %>%
# count()

##pensando em outra forma de fazer
tab1(base2$NM_MUNICIPIO, graph=F, sort.group = "decreasing")
tabpct(base2$NM_MUNICIPIO ,base2$DS_COR_RACA, graph = F, sort.group="decreasing")
##vou separar as bases municipais em eleito e nao eleito e ver a distribuição proporcional pelas raças
table(base2$status)

eleitos <- subset(base2, status=="ELEITO")
naoeleitos <- subset(base2, status=="NÃO ELEITO")
##proporção de eleitos por raça
a <- table(eleitos$NM_MUNICIPIO ,eleitos$DS_COR_RACA)
a <- cbind(municipios=row.names(a),a)
a <- as.data.frame.matrix(a)
#transformando os valores em vetores numéricos
a$AMARELA <- as.numeric(a$AMARELA)
a$BRANCA <- as.numeric(a$BRANCA)
a$INDÍGENA <- as.numeric(a$INDÍGENA)
a$PARDA <- as.numeric(a$PARDA)
a$PRETA <- as.numeric(a$PRETA)

#criando o total
a$tt <- rowSums(a[,c(2:6)])
##agregando os negros
a$NEGRA <- a$PARDA+a$PRETA
#criando o percentual de pretos eleitos
a$perc_pret_eleito <- round((a$PRETA/a$tt)*100,2)
#criando o percentual de pardos eleitos
a$perc_pard_eleito <- round((a$PARDA/a$tt)*100,2)
#criando o percentual de brancos eleitos
a$perc_branc_eleito <- round((a$BRANCA/a$tt)*100,2)
#criando o percentual de amarela eleitos
a$perc_amar_eleito <- round((a$AMARELA/a$tt)*100,2)
#criando o percentual de indigena eleitos
a$perc_indi_eleito <- round((a$INDÍGENA/a$tt)*100,2)
#criando o percentual de negros eleitos
a$perc_negr_eleito <- round((a$NEGRA/a$tt)*100,2)

##CRIANDO A TABELA DE REPRE ELEITA
repr_eleita <- merge(a, anal_raca, by.x="municipios", by.y = "Município")

##criando um grafico que ordenado pela representatividade, mas com a porcentagem de cand.pretas eleitas
repr_eleita$perc_cand_preta <- round((repr_eleita$perc_cand_preta*100),2)
repr_eleita$perc_cand_negra <- round((repr_eleita$perc_cand_negra*100),2)

ggplot(repr_eleita, aes(x = reorder(municipios, -perc_negr_eleito), y = perc_negr_eleito)) +
  geom_bar(stat="identity", fill="darkgoldenrod1", alpha=.9)+
  ylim(0,100)+
  theme(text = element_text(size=5))+
  labs(x="Municípios do RJ", y="Percentual de cand.negres eleites")+
  coord_flip()
##tentando criarar outro alternativa
repr_eleita$repr_preta100 <- round(repr_eleita$repr_preta*100,2) 
repr_eleita$repr_negra100 <- round(repr_eleita$repr_negra*100,2) 

ggplot(repr_eleita, aes(x = reorder(municipios, -repr_preta), y = repr_preta100)) +
  geom_bar(stat="identity", fill="black", alpha=1)+
  ylim(0,250)+
  theme(text = element_text(size=5))+
  labs(x="Municípios do RJ", y="Percentual de cand.pretes eleites")+
  coord_flip()
##fazendo uma regressao
ggplot(repr_eleita, aes(repr_preta100, perc_pret_eleito)) +
  geom_point()+
 geom_smooth(method = lm)

# loess method: local regression fitting
ggplot(repr_eleita, aes(repr_preta100, perc_pret_eleito)) +
  geom_point()+
  geom_smooth(method = "loess")+
  labs(x="Representatividade preta", y="Porcentagem de cand.pret.eleit")

# loess method: local regression fitting
ggplot(repr_eleita, aes(repr_negra100, perc_negr_eleito)) +
  geom_point()+
  geom_smooth(method = "loess")+
  labs(x="Representatividade negra", y="Porcentagem de cand.negr.eleit")
##regressao linear
perc_elet = lm(perc_negr_eleito~repr_negra100, data = repr_eleita) #Create the linear regression
summary(perc_elet)

perc_elet = lm(perc_pret_eleito~repr_preta100, data = repr_eleita) #Create the linear regression
summary(perc_elet)

##plotando por piores representatividades as porc. de pretes eleites
repr_eleita <- repr_eleita[order(repr_eleita$repr_negra100),]
pior30 <- repr_eleita[1:30,]
med30 <- repr_eleita[31:60,]
mel30 <- repr_eleita[61:92,]
##grafico dos municipios organizados pela representatividade de eleitos
ggplot(repr_eleita, aes(x = reorder(municipios, -perc_negr_eleito), y = perc_negr_eleito)) +
  geom_bar(stat="identity", fill="darkgoldenrod1", alpha=.9)+
  ylim(0,100)+
  theme(text = element_text(size=5))+
  labs(x="Municípios do RJ", y="Percentual de cand.negres eleites")+
  coord_flip()

ggplot(repr_eleita, aes(x = reorder(municipios, -perc_negr_eleito), y = repr_negra100)) +
  geom_bar(stat="identity", fill="bisque4", alpha=.9)+
  theme(text = element_text(size=5))+
  labs(x="Municípios do RJ", y="")+
  coord_flip()
#resumindo perc de eleitos negros e pretos
summary(repr_eleita$perc_negr_eleito)
summary(repr_eleita$perc_pret_eleito)

##30 piores de representatividade pelos seus perc. pretes eleites
ggplot(pior30, aes(x = reorder(municipios, -repr_preta), y = perc_pret_eleito)) +
  geom_bar(stat="identity", fill="darkgoldenrod1", alpha=.9)+
  ylim(0,250)+
  theme(text = element_text(size=10))+
  labs(x="30 piores Municípios do RJ", y="Percentual de cand.pretes eleites")+
  coord_flip()
####30 piores de representatividade pelos seus indices
ggplot(pior30, aes(x = reorder(municipios, -repr_preta), y = repr_preta100)) +
  geom_bar(stat="identity", fill="black", alpha=1)+
  ylim(0,250)+
  geom_hline(yintercept = 100, colour="red")+
  theme(text = element_text(size=10))+
  labs(x="30 piores Municípios do RJ", y="Índice de repr.preta")+
  coord_flip()
##fazendo uma regressao
ggplot(pior30, aes(repr_preta100, perc_pret_eleito)) +
  geom_point()+
  geom_smooth(method = lm)

# loess method: local regression fitting
ggplot(pior30, aes(repr_preta100, perc_pret_eleito)) +
  geom_point()+
  geom_smooth(method = "loess")

##regressao linear
perc_elet = lm(perc_pret_eleito~repr_preta100, data = pior30) #Create the linear regression
summary(perc_elet)

##30 medianos de representatividade pelos seus perc. pretes eleites
ggplot(med30, aes(x = reorder(municipios, -repr_preta), y = perc_pret_eleito)) +
  geom_bar(stat="identity", fill="darkgoldenrod1", alpha=.9)+
  ylim(0,250)+
  theme(text = element_text(size=10))+
  labs(x="30 medianos Municípios do RJ", y="Percentual de cand.pretes eleites")+
  coord_flip()
####30 medianos de representatividade pelos seus indices
ggplot(med30, aes(x = reorder(municipios, -repr_preta), y = repr_preta100)) +
  geom_bar(stat="identity", fill="black", alpha=1)+
  ylim(0,250)+
  geom_hline(yintercept = 100, colour="red")+
  theme(text = element_text(size=10))+
  labs(x="30 medianos Municípios do RJ", y="Índice de repr.preta")+
  coord_flip()
##fazendo uma regressao
ggplot(med30, aes(repr_preta100, perc_pret_eleito)) +
  geom_point()+
  geom_smooth(method = lm)

# loess method: local regression fitting
ggplot(med30, aes(repr_preta100, perc_pret_eleito)) +
  geom_point()+
  geom_smooth(method = "loess")

##regressao linear
perc_elet = lm(perc_pret_eleito~repr_preta100, data = med30) #Create the linear regression
summary(perc_elet)

##30 melhores de representatividade pelos seus perc. pretes eleites
ggplot(mel30, aes(x = reorder(municipios, -repr_preta), y = perc_pret_eleito)) +
  geom_bar(stat="identity", fill="darkgoldenrod1", alpha=.9)+
  ylim(0,250)+
  theme(text = element_text(size=10))+
  labs(x="30 melhores Municípios do RJ", y="Percentual de cand.pretes eleites")+
  coord_flip()
####30 medianos de representatividade pelos seus indices
ggplot(mel30, aes(x = reorder(municipios, -repr_preta), y = repr_preta100)) +
  geom_bar(stat="identity", fill="black", alpha=1)+
  ylim(0,250)+
  geom_hline(yintercept = 100, colour="red")+
  theme(text = element_text(size=10))+
  labs(x="30 melhores Municípios do RJ", y="Índice de repr.preta")+
  coord_flip()
##fazendo uma regressao
ggplot(mel30, aes(repr_preta100, perc_pret_eleito)) +
  geom_point()+
  geom_smooth(method = lm)

# loess method: local regression fitting
ggplot(mel30, aes(repr_preta100, perc_pret_eleito)) +
  geom_point()+
  geom_smooth(method = "loess")

##regressao linear
perc_elet = lm(perc_pret_eleito~repr_preta100, data = mel30) #Create the linear regression
summary(perc_elet)

write.table(repr_eleita,"cand_eleitos_raca.csv", sep="|", row.names = F, quote = F)

###trazendo a base da grana
options(scipen=999)
receitas <- read.table("receitas_candidatos_prestacao_contas_final_2016_RJ.txt",
                       sep=";", fileEncoding = "latin1", header = T)
##vendo o que a base tem
names(receitas)
##vendo o valor por municipio
table(receitas$Nome.da.UE)
##vendo o numero sequencial
summary(receitas$Sequencial.Candidato)

#valor.receita com #sequencial.candidato #tipo.receita
receitasredu <- receitas[,c(5,8,27,26)]
library(stringr)
#transformando em character
receitasredu$Valor.receita <- as.character(receitasredu$Valor.receita)
#trocando virgula por ponto
receitasredu$Valor.receita <- str_replace_all(receitasredu$Valor.receita, ",",".")
#transformando em numeric
receitasredu$Valor.receita <- as.numeric(receitasredu$Valor.receita)
#receita media de campanho
summary(receitasredu$Valor.receita)
##75%dos candidatos tiverem receita de ate 800, e o valor maximo de campanha foi de 40.000.000

##dando merge com os candidatos
library(dplyr)
#fazendo uma tabela com apenas o valor maximo 
b <- receitasredu%>% group_by(Sequencial.Candidato)%>%
  summarise(soma=sum(Valor.receita), max=max(Valor.receita))
#merge base com base de receiras 'b'
##trazendo de volta 

d <- table(base2$SQ_CANDIDATO, base2$DS_COR_RACA)
d <- cbind(sq_cand=row.names(d),d)
d <- as.data.frame.matrix(d)
##pegando só o que eu quero da base de votos, que é a descrição dos caras e a qtde de votos
names(base2)
base2redu <- base2[,c(1,14,31,38,39,81,83,87)]

votos <- base2redu%>%group_by(SQ_CANDIDATO, NM_UE.x,NM_PARTIDO.x,status,
                     DS_GENERO, DS_GRAU_INSTRUCAO, DS_COR_RACA)%>%summarise(maxvoto=max(QT_VOTOS_NOMINAIS))

##vendo o valor medio e max das campanhas 
rm(c)
valores <- receitasredu%>% group_by(Sequencial.Candidato)%>%summarise(medreceita=mean(Valor.receita), maxreceita=max(Valor.receita))

base3 <- merge(votos, b, by.x = "SQ_CANDIDATO", by.y = "Sequencial.Candidato",
               all = T)
##base3 tem registros com missing
base4 <- na.omit(base3)
##base 4 tem apenas registros sem linhas missing
mun_valor <- base4%>% group_by(NM_UE.x)%>%summarise(sum(soma))
names(mun_valor) <- c("Municipio", "Valortt")

pretos <- subset(base4, DS_COR_RACA=="PRETA")
#fazendo a categoria negro para raca
negros <- subset(base4, DS_COR_RACA=="PRETA" | DS_COR_RACA=="PARDA")

mun_valor_pret <- pretos%>% group_by(NM_UE.x)%>%summarise(sum(soma))
mun_valor_negr <- negros%>% group_by(NM_UE.x)%>%summarise(sum(soma))
names(mun_valor_pret) <- c("Municipio", "Valorpret")
names(mun_valor_negr) <- c("Municipio", "Valornegr")

base5 <- merge(mun_valor, mun_valor_pret, by="Municipio")
base5 <- merge(base5, mun_valor_negr, by="Municipio")

base5$perc_din_pret <- round((base5$Valorpret/base5$Valortt)*100,1)
base5$perc_din_negr <- round((base5$Valornegr/base5$Valortt)*100,1)

##fazendo a base com a repr_pret,perc_pret_eleita E FINALMENTE A GRANA PRETA
base6 <- merge(repr_eleita, base5, by.x="municipios", by.y = "Municipio")

#categorizando o perc.pret.eleito
summary(base6$perc_pret_eleito)
summary(base6$perc_pop_preta)
base6$perc_pop_preta100 <- round(base6$perc_pop_preta*100,2)
base6$perc_pop_negra100 <- round(base6$perc_pop_negra*100,2)

#criando um indice de repr pretos e negros eleitos
base6$repr_pret_eleitos <- base6$perc_pret_eleito/base6$perc_pop_preta
base6$repr_negr_eleitos <- base6$perc_negr_eleito/base6$perc_pop_negra

#categorizando 
base6$repr_pret_eleit_cat <- cut(base6$repr_pret_eleitos, breaks = c(0,99.9,Inf))
base6$repr_negr_eleit_cat <- cut(base6$repr_negr_eleitos, breaks = c(0,99.9,Inf))
#olhando pra ver se deu certo
table(base6$repr_pret_eleit_cat)
table(base6$repr_negr_eleit_cat)
#categorizando os levels
levels(base6$repr_pret_eleit_cat) <- c("pret_eleit_sub", "pret_eleit_repr")
levels(base6$repr_negr_eleit_cat) <- c("negr_eleit_sub", "negr_eleit_repr")

table(base6$repr_pret_eleit_cat)
table(base6$repr_negr_eleit_cat)

table(base6$repr_negr_eleit_cat, base6$repr_pret_eleit_cat)
##olha os quartis 
##1QT DOS MUN TIVERAM SÓ 2,84 ELEITOS
##2QT DOS MUN TIVERAM 6,670 ELEITOS
##3QT DOS MUN TIVERAM 14,750
##4QT DOS MUN TIVERAM 37,50
#faz os breaks com uma unidade decimal a menos que o valor quartis
#base6$pret_eleit_cat <- cut(base6$perc_pret_eleito, breaks = c(
  #0,2.83, 6.69,14.74, Inf))

#os nomes tem que corresponder a uma unidade decimal a menos, e começar com o valor do quartil
#levels(base6$pret_eleit_cat) <- c("0% - 2,83%", "2,84% - 6,69%", "6,70% - 14,7%","14,8% a 37,5%")
#olha se deu certo
#table(base6$pret_eleit_cat)

##categoriazando a representatividade preta
summary(base6$repr_preta100)
summary(base6$repr_negra100)
#dicotomizado ate 99 ou maior
base6$rep_pret_cat <- cut(base6$repr_preta100, breaks = c(0,99,Inf))
base6$rep_negr_cat <- cut(base6$repr_negra100, breaks = c(0,99,Inf))
#vê se deu certo
table(base6$rep_pret_cat)
table(base6$rep_negr_cat)
#renomeia os labels
levels(base6$rep_pret_cat) <- c("subrepresentado","representado")
levels(base6$rep_negr_cat) <- c("subrepresentado","representado")

table(base6$rep_pret_cat)
table(base6$rep_negr_cat)

##categorizando a renda investida em cand.pretes
summary(base6$perc_din_pret)
summary(base6$perc_din_negr)

summary(base6$perc_cand_preta)

base6$inves_pret <- base6$perc_din_pret/base6$perc_cand_preta
base6$inves_negr <- base6$perc_din_negr/base6$perc_cand_negra

base6$inves_pret_cat <- cut(base6$inves_pret, breaks = c(0,0.99, Inf))
base6$inves_negr_cat <- cut(base6$inves_negr, breaks = c(0,0.99, Inf))

table(base6$inves_pret_cat)
table(base6$inves_negr_cat)

levels(base6$inves_pret_cat) <- c("din_pret_abaixo", "din_pret_justo")
levels(base6$inves_negr_cat) <- c("din_negr_abaixo", "din_negr_justo")

#1qt=2,75
#2qt=4,0
#3qt=6,425
#4qt=26,10
 #base6$din_pret_cat <- cut(base6$perc_din_pret, breaks = c(
  # 0,2.74,3.9,6.424,Inf))
#vendo se deu certo
# table(base6$din_pret_cat)
#renomeando
#levels(base6$din_pret_cat) <- c("0% - 2,74%", "2,75% - 3,9%", "4% - 6,42%", "6,43% - 26,1%")
#table(base6$din_pret_cat)

##rodando uma one-way anova com a porcentagem (?)
#res.aov <- aov(perc_pret_eleito ~ rep_pret_cat, data = base6)
# Summary of the analysis
#summary(res.aov)
##verificando as categorias de riscos
contrasts(base6$repr_pret_eleit_cat)
contrasts(base6$rep_pret_cat)
contrasts(base6$inves_pret_cat)
##vendo tabelas
tab1(base6$repr_pret_eleit_cat)
#grafico de barras para representatividade ou nao
ggplot(base6, aes(repr_pret_eleit_cat))+
  geom_bar()
#grafico de barras para representatividade negra
ggplot(base6, aes(repr_negr_eleit_cat))+
  geom_bar()
##tabela de duplamente representado
table(base6$repr_negr_eleit_cat, base6$repr_pret_eleit_cat)

##lista de municipios com representatividade boa
repr_negr_pret <- subset(base6, repr_negr_eleit_cat=="negr_eleit_repr" & repr_pret_eleit_cat=="pret_eleit_repr" )
repr_negr_pret$municipios <- as.character(repr_negr_pret$municipios)
table(repr_negr_pret$municipios)
write.csv(repr_negr_pret, "muni_repr_pret_negr.csv")
#tabela dos municipios pela categ de repr.pret.eleitos
table(base6$repr_pret_eleit_cat)
mun_repr_ele <- subset(base6, repr_pret_eleit_cat=="pret_eleit_repr")
mun_repr_ele$municipios <- as.character(mun_repr_ele$municipios)
table(mun_repr_ele$municipios)
names(mun_repr_ele)
mun_repr_ele <- mun_repr_ele[,c(1,8,33)]
write.csv(mun_repr_ele,"muni_repr_elei_pret.csv", row.names = F, quote = F)
##a representatividade de candidaturas
tabpct(base6$repr_pret_eleit_cat,base6$rep_pret_cat)


######dinheiro para candidatura preta
summary(base6$Valortt)
summary(base6$Valornegr)
summary(base6$Valorpret)


tabpct( base6$repr_pret_eleit_cat ,base6$inves_pret_cat)
##municipios com investimento justo
mun_justo_pret <- subset(base6, inves_pret_cat=="din_pret_justo")
mun_justo_negr <- subset(base6, inves_negr_cat=="din_negr_justo")
#names(mun_justo)
#write.csv(mun_justo, "muni_justo_din_pret.csv", row.names = F, quote = F)

#acredito que anova é enviesado por não são númros continuos para darem medioa
#rodando uma logistica
##rodando uma logistica
#mylogit <- glm(repr_pret_eleit_cat ~ rep_pret_cat + inves_pret_cat, data = base6, family = "binomial")

#summary(mylogit)

#confint(mylogit)

##odds ratio apenas
#exp(coef(mylogit))

##odds ratio e intervalos de confianca
#exp(cbind(OR = coef(mylogit), confint(mylogit)))

##plotando os municipios 
#ggplot(base6, aes())

##ordenando a base6 toda pela prop de pretos eleitos
base6_outra <- base6[order(base6$repr_negr_eleitos),]

pior30 <- base6_outra[1:30,]
med30 <- base6_outra[31:60,]
mel30 <- base6_outra[61:92,]
##plotando o grafico da repre negra eleita
ggplot(base6, aes(x = reorder(municipios, -repr_negr_eleitos), y = repr_negr_eleitos)) +
  geom_bar(stat="identity", fill="bisque4", alpha=1)+
  ylim(0,300)+
  theme(text = element_text(size=5))+
  labs(x="Municípios do RJ", y="Representavidade de pessoas pretas eleitas")+
  coord_flip()
##plotando o gráfico da repre preta eleita
ggplot(base6, aes(x = reorder(municipios, -repr_negr_eleitos), y = repr_pret_eleitos)) +
  geom_bar(stat="identity", fill="chocolate4", alpha=1)+
  ylim(0,300)+
  theme(text = element_text(size=5))+
  labs(x="Municípios do RJ", y="Representavidade de pessoas pretas eleitas")+
  coord_flip()

##plotando os 30 piores de repre eleito
ggplot(pior30, aes(x = reorder(municipios, -repr_pret_eleitos), y = repr_pret_eleitos)) +
  geom_bar(stat="identity", fill="chocolate4", alpha=1)+
  ylim(0,300)+
  theme(text = element_text(size=10))+
  labs(x="30 piores Municípios do RJ", y="Representavidade de pessoas pretas eleitas")+
  coord_flip()

##plotando os 30 MEDIANOS de repre eleito
ggplot(med30, aes(x = reorder(municipios, -repr_pret_eleitos), y = repr_pret_eleitos)) +
  geom_bar(stat="identity", fill="chocolate4", alpha=1)+
  ylim(0,300)+
  theme(text = element_text(size=10))+
  labs(x="30 medianos Municípios do RJ", y="Representavidade de pessoas pretas eleitas")+
  coord_flip()

##plotando os 30 MEDIANOS de repre eleito
ggplot(mel30, aes(x = reorder(municipios, -repr_pret_eleitos), y = repr_pret_eleitos)) +
  geom_bar(stat="identity", fill="chocolate4", alpha=1)+
  ylim(0,300)+
  theme(text = element_text(size=10))+
  labs(x="30 melhores Municípios do RJ", y="Representavidade de pessoas pretas eleitas")+
  coord_flip()

##plotando o grafico da REPR DE CANDIDATURA ORDENADO PELO REPR ELEITO
ggplot(base6, aes(x = reorder(municipios, -repr_pret_eleitos), y = repr_preta100)) +
  geom_bar(stat="identity", fill="goldenrod2", alpha=1)+
  ylim(0,300)+
  theme(text = element_text(size=5))+
  labs(x="Municípios do RJ", y="Representavidade de candidaturas pretas")+
  coord_flip()

##plotando os 30 piores de REPR DE CANDIDATURA ORDENADO PELO REPR ELEITO
ggplot(pior30, aes(x = reorder(municipios, -repr_pret_eleitos), y = repr_preta100)) +
  geom_bar(stat="identity", fill="goldenrod2", alpha=1)+
  ylim(0,300)+
  theme(text = element_text(size=10))+
  labs(x="30 piores Municípios do RJ", y="Representavidade de candidaturas pretas")+
  coord_flip()

##plotando os 30 MEDIANOS REPR DE CANDIDATURA ORDENADO PELO REPR ELEITO
ggplot(med30, aes(x = reorder(municipios, -repr_pret_eleitos), y = repr_preta100)) +
  geom_bar(stat="identity", fill="goldenrod2", alpha=1)+
  ylim(0,300)+
  theme(text = element_text(size=10))+
  labs(x="30 medianos Municípios do RJ", y="Representavidade de candidaturas pretas")+
  coord_flip()

##plotando os 32 MELHORES REPR DE CANDIDATURA ORDENADO PELO REPR ELEITO
ggplot(mel30, aes(x = reorder(municipios, -repr_pret_eleitos), y = repr_preta100)) +
  geom_bar(stat="identity", fill="goldenrod2", alpha=1)+
  ylim(0,300)+
  theme(text = element_text(size=10))+
  labs(x="30 melhores Municípios do RJ", y="Representavidade de candidaturas pretas")+
  coord_flip()

##plotando o grafico da INVESTIMENTO EM PRETES ORDENADO PELO REPR ELEITO
base6_outra$inves_pret100 <- base6$inves_pret*100
base6_outra$inves_negr100 <- base6$inves_negr*100

ggplot(base6_outra, aes(x = reorder(municipios, -repr_negr_eleitos), y = inves_negr100)) +
  geom_bar(stat="identity", fill="forestgreen", alpha=1)+
  ylim(0,300)+
  theme(text = element_text(size=5))+
  labs(x="Municípios do RJ", y="Investimento de candidaturas negras")+
  coord_flip()

#barras entre grana e repr preta eleita
ggplot(base6_outra, aes(x = reorder(municipios, -repr_negr_eleitos), y = inves_pret100)) +
  geom_bar(stat="identity", fill="forestgreen", alpha=1)+
  ylim(0,300)+
  theme(text = element_text(size=5))+
  labs(x="Municípios do RJ", y="Investimento de candidaturas pretas")+
  coord_flip()
##grafico de linha e smooth entre a representividade de eleitos e invest negras
ggplot(base6_outra, aes(inves_negr100, repr_negr_eleitos))+
  geom_smooth(method=loess)+
  geom_point()

#relação entre grana e representatividade preta
ggplot(base6_outra, aes(inves_pret100, repr_pret_eleitos))+
  geom_smooth(method=loess)+
  geom_point()
##tabelas do repre e invest pret
table(base6_outra$repr_negr_eleit_cat, base6_outra$inves_negr_cat)

##tabelas do repre e invest pret
levels(base6_outra$inves_pret_cat) <- c("invest_abaixo","invest_justo")
table(base6_outra$repr_pret_eleit_cat, base6_outra$inves_pret_cat)

##plotando os 30 piores de INVESTIMENTO EM CANDIDATURA ORDENADO PELO REPR ELEITO
ggplot(pior30, aes(x = reorder(municipios, -repr_pret_eleitos), y = inves_pret100)) +
  geom_bar(stat="identity", fill="forestgreen", alpha=1)+
  ylim(0,300)+
  theme(text = element_text(size=10))+
  labs(x="30 piores Municípios do RJ", y="Investimento de candidaturas pretas")+
  coord_flip()


##plotando os 30 MEDIANOS INVESTIMENTO CANDIDATURA ORDENADO PELO REPR ELEITO
ggplot(med30, aes(x = reorder(municipios, -repr_pret_eleitos), y = inves_pret100)) +
  geom_bar(stat="identity", fill="forestgreen", alpha=1)+
  ylim(0,300)+
  theme(text = element_text(size=10))+
  labs(x="30 medianos Municípios do RJ", y="Investimento de candidaturas pretas")+
  coord_flip()

##plotando os 32 MELHORES INVESTIMENTO DE CANDIDATURA ORDENADO PELO REPR ELEITO
ggplot(mel30, aes(x = reorder(municipios, -repr_pret_eleitos), y = inves_pret100)) +
  geom_bar(stat="identity", fill="forestgreen", alpha=1)+
  ylim(0,300)+
  theme(text = element_text(size=10))+
  labs(x="30 melhores Municípios do RJ", y="Investimento de candidaturas pretas")+
  coord_flip()
######################
##análise dos partidos
#trazendo a class de partido 
class <- read.csv("PART_POSI.csv", sep="|")
names(class)

#retirando quem nao tinha status de eleito
base7 <- subset(base4, status!="")
base7$NM_PARTIDO.x <- as.character(base7$NM_PARTIDO.x)
#trazendo a sigla que está na base 2
names(base2)
siglas <- base2[,c(30,31)]
siglas%>%group_by(SG_PARTIDO.x)%>%count()
#deixando tudo maiusculo
siglas$SG_PARTIDO.x <- toupper(siglas$SG_PARTIDO.x)
#retirando espaços
siglas$SG_PARTIDO.x <- str_replace_all(siglas$SG_PARTIDO.x, " ", "")
#RETIRANDO ESPACOS DO CLASS
class$Partido <- as.character(class$Partido)
class$Posição <- as.character(class$Posição)
class$Partido <- str_replace_all(class$Partido, " ", "")

#vendo se deu certo
e <- siglas%>%group_by(SG_PARTIDO.x, NM_PARTIDO.x)%>%count()
e <- e[,-3]
#FAZENDO MERGE ENTRE A BASE 7 E O E
base7 <- merge(base7, e, by.x ="NM_PARTIDO.x", by.y = "NM_PARTIDO.x",
               all.x = T)

##fazendo merge entre base7 e class
base7 <- merge(base7, class, by.x = "SG_PARTIDO.x", by.y = "Partido", all.x = T, all.y = F)
#vendo se deu certo
table(base7$Posição) #sim
##########
library(epiDisplay)
tab1(base7$NM_PARTIDO.x, graph=F, sort.group = "decreasing")
##candidaturas entre as posições
tab1(base7$Posição, graph=F, sort.group = "decreasing")
##fazendo a mesma classificacao com a orientacao 
direita <- subset(base7, ALMA_LABE=="DIREITA")
esquerda <- subset(base7, ALMA_LABE=="ESQUERDA")
f <- table(base7$NM_PARTIDO.x, base7$ALMA_LABE)
f <- cbind(partidos=row.names(f),f)
f <- as.data.frame.matrix(f)
f$DIREITA <- as.character(f$DIREITA)
f$DIREITA <- as.numeric(f$DIREITA)

f$ESQUERDA <- as.character(f$ESQUERDA)
f$ESQUERDA <- as.numeric(f$ESQUERDA)

f$tt <- f$ESQUERDA+f$DIREITA

##gênero total
tab1(base7$DS_GENERO)

#genero por partido e posição
library(ggplot2)
##reposicionando a posicao
base7$Posição <- as.factor(base7$Posição)
levels(base7$Posição)
base7$Posição <- factor(base7$Posição, levels = c("ESQUERDA", "CENTRO", "DIREITA"))
table(base7$Posição)

ggplot(base7, aes(reorder(NM_PARTIDO.x,desc(NM_PARTIDO.x)), fill=DS_GENERO)) +
  geom_bar(aes(fill=DS_GENERO), position='fill')+
  labs(x="Partidos", y="Distribuição entre gênero", fill="Gênero")+
  theme(axis.text.y = element_text(colour = cor ))+
  coord_flip()
#criar um vetor de cor por posicao
tab <- base7%>% group_by(SG_PARTIDO.x, NM_PARTIDO.x, Posição)%>%summarise()
write.csv(tab,"tab_part.csv")
cor <- c("blue4","green4", "green4", "blue4",
         "blue4","blue4","blue4","darkred",
         "darkred","darkred","blue4","green4",
         "blue4","blue4","blue4","blue4",
         "blue4","blue4","blue4","green4","green4",
         "blue4","green4","blue4","darkred","green4",
         "darkred","green4","blue4","blue4",
         "blue4","darkred","darkred","darkred","blue4")

#genero por portido entre eleitos e nao 
table(base7$status)
ggplot(subset(base7, status!=""), aes(reorder(NM_PARTIDO.x,desc(NM_PARTIDO.x)), fill=DS_GENERO)) +
  geom_bar(aes(fill=DS_GENERO), position='fill')+
  labs(x="Partidos", y="Distribuição entre gênero", fill="Gênero")+
  theme(axis.text.y = element_text(colour = cor ))+
  coord_flip()+
  facet_wrap(~status)
##tabela de eleitos e nao eleitos por genero
base7%>%group_by(Posição, DS_GENERO, status)%>%summarise()
##grafico da escolaridade
levels(base7$DS_GRAU_INSTRUCAO) <- c("FUNDAMENTAL","FUNDAMENTAL", "MÉDIO", "MÉDIO","LÊ/ESCREVE", "SUPERIOR", "SUPERIOR")
base7$DS_GRAU_INSTRUCAO <- factor(base7$DS_GRAU_INSTRUCAO, levels=c("LÊ/ESCREVE", "FUNDAMENTAL", "MÉDIO", "SUPERIOR"))

ggplot(base7, aes(reorder(NM_PARTIDO.x,desc(NM_PARTIDO.x)), fill=DS_GRAU_INSTRUCAO)) +
  geom_bar(aes(fill=DS_GRAU_INSTRUCAO), position='fill')+
  labs(x="Partidos", y="Distribuição entre escolaridade", fill="Escolaridade")+
  theme(axis.text.y = element_text (colour=cor))+
  coord_flip()
##escolaridade entre os eleitos e nao eleitos
ggplot(subset(base7, status!=""), aes(reorder(NM_PARTIDO.x,desc(NM_PARTIDO.x)), fill=DS_GRAU_INSTRUCAO)) +
  geom_bar(aes(fill=DS_GRAU_INSTRUCAO), position='fill')+
  labs(x="Partidos", y="Distribuição entre escolaridade", fill="Escolaridade")+
  theme(axis.text.y = element_text(colour = cor))+
  coord_flip()+
  facet_wrap(~status)

#ESCOLARIDADE POR GENERO
ggplot(subset(base7, status!=""), aes(reorder(NM_PARTIDO.x,desc(NM_PARTIDO.x)), fill=DS_GRAU_INSTRUCAO)) +
  geom_bar(aes(fill=DS_GRAU_INSTRUCAO), position='fill')+
  labs(x="Partidos", y="Distribuição entre escolaridade", fill="Escolaridade")+
  theme(axis.text.y = element_text(colour = cor))+
  coord_flip()+
  facet_wrap(~DS_GENERO)

##partido por racacor
ggplot(subset(base7, status!=""), aes(reorder(NM_PARTIDO.x,desc(NM_PARTIDO.x)), fill=DS_COR_RACA)) +
  geom_bar(aes(fill=DS_COR_RACA), position='fill')+
  labs(x="Partidos", y="", fill="Raça cor")+
  theme(axis.text.y = element_text(colour = cor))+
  coord_flip()

##raca cor por escolaridade


##RACA COR ENTRE GENEROS
ggplot(subset(base7, status!=""), aes(reorder(NM_PARTIDO.x,desc(NM_PARTIDO.x)), fill=DS_COR_RACA)) +
  geom_bar(aes(fill=DS_COR_RACA), position='fill')+
  labs(x="Partidos", y="", fill="Raça cor")+
  theme(axis.text.y = element_text(colour = cor))+
  coord_flip()+
  facet_wrap(~DS_GENERO)

#racacor entre eleitos e nao 
ggplot(subset(base7, status!=""), aes(reorder(NM_PARTIDO.x,desc(NM_PARTIDO.x)), fill=DS_COR_RACA)) +
  geom_bar(aes(fill=DS_COR_RACA), position='fill')+
  labs(x="Partidos", y="", fill="Raça cor")+
  theme(axis.text.y = element_text(colour = cor))+
  coord_flip()+
  facet_wrap(~status)

##refazendo o raro
raro <- subset(base7, DS_GRAU_INSTRUCAO=="SUPERIOR" & DS_COR_RACA=="PRETA" & DS_GENERO=="FEMININO")
names(total)
ident <- total[,c(16,21,18,22)]
raro2 <- merge(raro, ident, by.x="SQ_CANDIDATO", by.y = "SQ_CANDIDATO",
               all.x=T, all.y = F)
write.table(raro2, "raro_exp.csv", sep="|", row.names = F, quote = F)
##pirando
#criando negros ou nao
base7$negrocat <- ""
base7$negrocat[which(base7$DS_COR_RACA=="PRETA" | 
                 base7$DS_COR_RACA=="PARDA")] <- "NEGRA"

base7$negrocat[which(base7$DS_COR_RACA!="PRETA" & 
                       base7$DS_COR_RACA!="PARDA")] <- "NÃO-NEGRA"
#vendo se deu certo
table(base7$negrocat)
#criando preto ou nao
base7$pretocat <- ""
base7$pretocat[which(base7$DS_COR_RACA=="PRETA")] <- "PRETA"

base7$pretocat[which(base7$DS_COR_RACA!="PRETA")] <- "NÃO-PRETA"
#VENDO SE DEU CERTO
table(base7$pretocat)

##escolaridade gênero e raça entre eleitos e nao eleitos
ggplot(subset(base7, status!=""), aes(DS_GRAU_INSTRUCAO, DS_GENERO))+
  geom_jitter(aes(colour=negrocat), alpha=.5)+
  labs(x="Escolaridade", y="Gênero", colour="Negro cat")
##escolaridade gênero e raça entre eleitos e nao eleitos para preto
ggplot(subset(base7, status!=""), aes(DS_GRAU_INSTRUCAO, DS_GENERO))+
  geom_jitter(aes(colour=pretocat), alpha=.5)+
  labs(x="Escolaridade", y="Gênero", colour="Preto cat")
#tudo entre eleitos e nao
##escolaridade gênero e raça entre eleitos e nao eleitos
ggplot(subset(base7, status!=""), aes(DS_GRAU_INSTRUCAO, DS_GENERO))+
  geom_jitter(aes(colour=negrocat), alpha=.5)+
  labs(x="Escolaridade", y="Gênero", colour="Negro cat")+
  facet_wrap(~status)
##escolaridade gênero e raça entre eleitos e nao eleitos para preto
ggplot(subset(base7, status!=""), aes(DS_GRAU_INSTRUCAO, DS_GENERO))+
  geom_jitter(aes(colour=pretocat), alpha=.5)+
  labs(x="Escolaridade", y="Gênero", colour="Preto cat")+
  facet_wrap(~status)

##########################
##tabelas do partido
#GENERO POR POSICIONAMENTO
tabpct(base7$Posição, base7$DS_GENERO)
##GENERO POR ELEITOS E NAO 
tabpct(base7$status, base7$DS_GENERO)
##SEPARANDO ENTRE ELEITOS E NAO 
eleitos <- subset(base7, status=="ELEITO")
naoeleitos <- subset(base7, status=="NÃO ELEITO")
##TABELA Das variaveis por posicionamento NO ELEITO
tabpct(eleitos$DS_GENERO, eleitos$Posição)
library(tableone)
vars <- c("DS_GENERO","DS_COR_RACA","DS_GRAU_INSTRUCAO")
tab1 <- CreateTableOne(vars, strata="Posição", data=eleitos)
tab11 <- print(tab1, showAllLevels = T)
write.csv(tab11, "eleitos_posicao.csv")

##tabela das variaveis por posicionamento no nao eleito
vars <- c("DS_GENERO","DS_COR_RACA","DS_GRAU_INSTRUCAO")
tab2 <- CreateTableOne(vars, strata="Posição", data=naoeleitos)
tab21 <- print(tab2, showAllLevels = T)
write.csv(tab21, "naoeleitos_posicao.csv")

#tabela da escolaridade
tab1(base7$DS_GRAU_INSTRUCAO)

#tabela da escolaridade por sexo
tabpct(base7$DS_GENERO ,base7$DS_GRAU_INSTRUCAO )

#tabela da racacor
tab1(base7$DS_COR_RACA)

#tabela da racacor entre generos
tabpct(base7$DS_GENERO, base7$DS_COR_RACA)

write.table(base6_outra, "base_cand_fim.csv", sep="|", quote = F, row.names = F)
write.table(base7, "base_cand_posicao.csv", sep="|", quote = F, row.names = F)
