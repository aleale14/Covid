

# Libraries ---------------------------------------------------------------

library(jsonlite)
library(ggplot2)
library(dplyr)
library(ggpubr)



# Import data -------------------------------------------------------------

json_file <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-json/dpc-covid19-ita-province.json"
data_province<-jsonlite::fromJSON(json_file)

json_file<-"https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-json/dpc-covid19-ita-regioni.json"
data_regioni<-jsonlite::fromJSON(json_file)

json_file<-"https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-json/dpc-covid19-ita-andamento-nazionale.json"
data_ita<-jsonlite::fromJSON(json_file)




# Variables ---------------------------------------------------------------

prov = "RM"

prov_nom = data_province%>%
  filter(sigla_provincia==prov)%>%
  select(denominazione_provincia)%>%
  filter(row_number()==1)
regio = data_province%>%
  filter(sigla_provincia==prov)%>%
  select(denominazione_regione)%>%
  filter(row_number()==1)



# data_wrangling ----------------------------------------------------------

data_province<-data_province%>%
  group_by(sigla_provincia)%>%
  arrange(data)%>%
  mutate(delta=(totale_casi-lag(totale_casi)))%>%
  mutate(perc_evo=round(100*(totale_casi-lag(totale_casi))/totale_casi),0)

tot_data_province<-data_province%>%
  group_by(data)%>%
  summarise(totale_casi=sum(totale_casi))%>%
  mutate(delta=totale_casi-lag(totale_casi))%>%
  mutate(perc_evo=round(100*(1-lag(totale_casi)/totale_casi)))




# Visualization -----------------------------------------------------------


g_nat<-ggplot(data_province,
       aes(x=as.Date(data),y=totale_casi,fill=denominazione_regione))+
  geom_col()+ labs(
    title = "Andamento nazionale",
    subtitle = "Contributi regionali", x = NULL, y = "Casi totali", colour = NULL
  )+theme_minimal()+
  theme(legend.title=element_blank())+
  theme(legend.position = "bottom")

g_reg<-ggplot(filter(data_province,denominazione_regione==as.character(regio)),
       aes(x=as.Date(data),y=totale_casi,fill=sigla_provincia))+
  scale_fill_brewer()+
  geom_col()+ labs(
    title = as.character(regio),
    #subtitle = "Timeline provinciale", 
    x = NULL, y = "Casi totali", 
    colour = NULL
  ) +
  theme_minimal()+
  theme(legend.title=element_blank())+ 
  theme(legend.position = c(0.2, 0.8))

g_prov<-ggplot(filter(data_province,sigla_provincia==prov),
       aes(x=as.Date(data),y=totale_casi,fill=sigla_provincia))+
  geom_col(fill="orange")+
  labs(
    title = paste0("Provincia di ",prov_nom),
    #subtitle = "Timeline", 
    x = NULL, y = "Casi totali", colour = NULL
  ) + 
  theme_minimal()+
  theme(legend.position = "none")+
  geom_text(aes(label=totale_casi))#,vjust=-0.5)



g_provnat<-ggplot()+
  geom_col(data=data_province,
           aes(x=as.Date(data),y=totale_casi),fill="lightgray")+
  geom_col(data=filter(data_province,denominazione_regione==as.character(regio)),
           aes(x=as.Date(data),y=totale_casi),
           fill="navyblue")+
  geom_col(data=filter(data_province,sigla_provincia==prov),
             aes(x=as.Date(data),y=totale_casi,colour=sigla_provincia),
             colour="orange")+
  labs(
    title = "Paragone andamento nazionale, regionale e provinciale",
    subtitle = paste0("Regione: ",regio," Provincia: ",as.character(prov_nom)),
    x = NULL, y = "Casi totali", colour = NULL
  )+
  theme_minimal()




# Stats -------------------------------------------------------------------

Stats<-data.frame(matrix(NA,2,5))%>%
  rename(Scopo=X1,Data=X2,"Casi totali"=X3,
         "Evoluzione casi"=X4,"Evoluzione casi in %"=X5)

Stats[1,]<-c("ITA",tot_data_province%>%filter(row_number()==n()))
Stats[2,]<-data_province%>%
  filter(sigla_provincia==prov)%>%
  filter(row_number()==n())%>%
  select(c(data,totale_casi,delta,perc_evo))

g_stats <- ggtexttable(Stats, rows = NULL, 
                        theme = ttheme("mOrange"))



# Output ------------------------------------------------------------------


figure <- ggarrange(g_stats, g_prov,g_reg,g_provnat,
                    #labels = c("A", "B", "C","D"),
                    ncol = 1, nrow = 4)
figure