********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
**********                      Trabalho Final                      ************
**********                        Grupo 05                          ************
**********                                                          ************
********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************


clear all
cd "C:\Users\laiza\OneDrive\Área de Trabalho\Insper Economia\5 Semestre\Micro IV\Trabalho Final"

log using "Micro IV - Trabalho Final - Grupo 05", replace


********** Bases de dados
********************************************************************************
use "dados.dta"

keep cod_upp upp ano mes hom_doloso roubo_veiculo apreensao_drogas pop_2010 registro_ocorrencias

********** A analise sera feita considerando apenas ate o ano de 2019 para que nao tenha contribuicoes dos efeitos da pandemia que atingiu os anos de 2020 e 2021

drop if ano==2020 | ano==2021

********** Em janeiro do ano de 2018 aconteceu uma operação de intervecao militar em todo estado do Rio de Janeiro, por isso iremos identificar por meio de dummy o efeito desse periodo inicial na regressao 
********************************************************************************
********** Dummy de janeiro de 2018

gen jan18=0
replace jan18=1 if mes==1 & ano==2018

gen fev18=0
replace fev18=1 if mes==2 & ano==2018

gen marc18=0
replace marc18=1 if mes==3 & ano==2018

gen abr18=0
replace abr18=1 if mes==4 & ano==2018

********** Identificando as UPPs abertas e fechadas 
********************************************************************************

gen upp_fechada=0
replace upp_fechada=1 if cod_upp== 2| cod_upp== 3 | cod_upp==14 | cod_upp==16 | cod_upp==29 | cod_upp==32 | cod_upp==33 | cod_upp==36 | cod_upp==37 | cod_upp==38

********** Sera utilizado o senso demografico respectivo de cada UPP do ano de 2010
********************************************************************************

global controls pop_2010

gen ocorrencias_pop=.
replace ocorrencias_pop= registro_ocorrencias/pop_2010

********** Diferenciando o periodo antes e apos o fechamento das UPPs 
********************************************************************************
gen antes=1
replace antes=0 if mes>=6 & ano>=2018

********** Balanceamento & PSM
********************************************************************************
********** O PSM sera realizado utilizando a populacao de 2010 de cada UPP e a média de crimes antes das UPPs serem instaladas 

bysort cod_upp: egen crimes_2008= mean (registro_ocorrencias)

psmatch2 upp_fechada $controls crimes_2008 , out(ocorrencias_pop)

pstest $controls crimes_2008

psgraph

********** Diff in Diff via comparação de médias
********************************************************************************

********** Comparação de medias antes e depois para UPP que permaneceram abertas 
********************************************************************************

ttest ocorrencias_pop if upp_fechada==0, by(antes)

********** Comparação de médias antes e depois para UPP que foram fechadas 
********************************************************************************

ttest ocorrencias_pop if upp_fechada==1, by(antes)


********** Dif-in-Dif via regressao
********************************************************************************

reg ocorrencias_pop upp_fechada i.ano 1.upp_fechada#c.ano jan18 $controls 

********** Diff-in-Diff para crimes especificos 
********************************************************************************

gen roubos_pop=.
replace roubos_pop= roubo_veiculo/pop_2010

********** Comparação de medias antes e depois para UPP que permaneceram abertas 
********************************************************************************

ttest roubos_pop if upp_fechada==0, by(antes)

********** Comparação de médias antes e depois para UPP que foram fechadas 
********************************************************************************

ttest roubos_pop if upp_fechada==1, by(antes)

gen hom_pop=.
replace hom_pop= hom_doloso/pop_2010

********** Comparação de medias antes e depois para UPP que permaneceram abertas 
********************************************************************************

ttest hom_pop if upp_fechada==0, by(antes)

********** Comparação de médias antes e depois para UPP que foram fechadas 
********************************************************************************

ttest hom_pop if upp_fechada==1, by(antes)



********** Teste de placebo 
********** Comparação de médias antes e depois para UPP que foram fechadas 
********************************************************************************
drop if ano==2019 | ano<2014

gen antes_fake=1
replace antes_fake=0 if ano>2016

********** Comparação de medias antes e depois para UPP que permaneceram abertas 
********************************************************************************

ttest ocorrencias_pop if upp_fechada==0, by(antes_fake)

********** Comparação de médias antes e depois para UPP que foram fechadas 
********************************************************************************

ttest ocorrencias_pop if upp_fechada==1, by(antes_fake)




log close


