## COPIAR (CTRL + C) TABELA DA WEB E CARREGÁ-LA COM O COMANDO ABAIXO:
a <- read.delim("clipboard")
a

library(ggplot2)

desmatamento <- function(x, y){-100*round(1-a$`AMZ.LEGAL`[a[,1]==y]/a$`AMZ.LEGAL`[a[,1]==x], digits = 3)}
segment_round <- function(x, y){list(
  annotate("segment", x = x, xend = y, y = a$`AMZ.LEGAL`[a[,1]==x], yend = a$`AMZ.LEGAL`[a[,1]==y]),
  geom_point(aes(x = x, y = a$AMZ.LEGAL[a[,1]==x]), shape = 21, size = 4, fill = "white", stroke = 1.1),
  geom_point(aes(x = y, y = a$AMZ.LEGAL[a[,1]==y]), shape = 21, size = 4, fill = "white", stroke = 1.1)
)}


## PLOTAGEM
ggplot(a, aes(x = Ano.Estados)) +
  
  ## Regiões coloridas
  geom_rect(aes(xmin=1995,xmax=2003,ymin=-Inf,ymax=Inf),alpha=0.01,fill="blue") +
  annotate("rect", xmin=2003, xmax=2011, ymin=-Inf, ymax=Inf, alpha=0.2, fill="red") +
  annotate("rect", xmin=2011, xmax=2016+(8/12), ymin=-Inf, ymax=Inf, alpha=0.2, fill="pink") +
  annotate("rect", xmin=2016+(8/12), xmax=2019, ymin=-Inf, ymax=Inf, alpha=0.2, fill="black") +
  annotate("rect", xmin=2019, xmax=2022, ymin=-Inf, ymax=Inf, alpha=0.2, fill="green") +
  
  ## Gráfico com os dados
  geom_step(aes(y = AMZ.LEGAL), stat = "identity") +
  
  ## FHC
  annotate("text", x = 1995+(2003-1995)/2, y = 2500, label = "FHC") +
  segment_round(1995, 2003) +
  geom_label(aes(x = 1995+(2003-1995)/2, y = 27500, label = paste0(desmatamento(1995, 2003), " %"))) +
  
  # Lula
  annotate("text", x = 2003+(2011-2003)/2, y = 2500, label = "Lula") +
  segment_round(2003, 2011) +
  geom_label(aes(x = 2003+(2011-2003)/2, y = 27500, label = paste0(desmatamento(2003, 2011), " %"))) +
  
  ## Dilma
  annotate("text", x = 2011+(2016+(8/12)-2011)/2, y = 2500, label = "Dilma") +
  segment_round(2011, 2016) +
  geom_label(aes(x = 2011+(2016+(8/12)-2011)/2, y = 10000, label = paste0(desmatamento(2011, 2016), " %"))) +
  
  ## Temer
  annotate("text", x = 2016+(8/12)+(2019-2016-(8/12))/2, y = 2500, label = "Temer") +
  segment_round(2016, 2019) +
  geom_label(aes(x = 2016+(2019-2016-(8/12))/2, y = 10000, label = paste0(desmatamento(2016, 2019), " %"))) +
  
  ## Jair
  annotate("text", x = 2019+(2021-2019)/2, y = 2500, label = "Jair") +
  segment_round(2019, 2021) +
  geom_label(aes(x = 2019+(2021-2019)/2, y = 15000, label = paste0(desmatamento(2019, 2021), " %"))) +
  

  ## Comemorativos finais
  theme_bw() +
  scale_x_continuous(breaks=c(1995, 2003, 2011, 2016, 2019, 2022)) +
  ylab("DESMATAMENTO ANUAL EM KM²") +
  xlab("ANO DE REGISTRO (PRODES - INPE)") +
  theme(text = element_text(size=rel(4)))

