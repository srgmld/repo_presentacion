library(tidyverse)
library(hrbrthemes)
library(janitor)
library(magrittr)
library(lubridate)
library(kableExtra)
library(highcharter)
library(ggrepel)
library(plotly)
library(scales)
library(zoo)
library(forecast)

margen <- rio::import("input/margen_gral.xlsx")
avg_mes <- rio::import("input/promedio_mes_margen.xlsx")



avg_mes %>% ggplot(aes(x = as.Date(as.yearmon(avg_mes$mes_year)), y = margen_bruto_bs )) + 
  geom_line(color="steelblue") + 
  geom_point() +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  scale_y_continuous(labels = comma, breaks = seq(0, 700000, by = 100000)) +
  # theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Fecha") + ylab("Bs") + 
  ggtitle("Margen Bruto Bs")


# valores totales

hchart(margen %>% group_by(fecha) %>% 
         summarise(`Precio total` = sum(precio_total_venta_bs),
                   `Costo total` = sum(costo_total_bs),
                   `Margen` = `Precio total`-`Costo total`) %>%
         mutate(`Costo total` = (-`Costo total`)) %>% 
         gather(tipo, valor, -fecha) %>% 
         arrange(fecha),
       "line", 
       hcaes(x = as.Date(fecha), y = valor, group = tipo)) %>% 
  hc_colors(c("#ee4035" ,"#0392cf", "#7bc043")) %>% 
  hc_yAxis(title = list(text = "Valor en Bs.")) %>% 
  hc_xAxis(title = list(text = ""))





# Valores por m3

hchart(margen %>% group_by(fecha) %>% 
         summarise(`Precio por m3` = mean(precio_bs_m3),
                   `Costo por m3` = mean(costo_total_bs_m3),
                   `Margen por m3` = mean(margen_bs_m3)) %>%
         mutate(`Costo por m3` = (-`Costo por m3`)) %>% 
         gather(tipo, valor, -fecha) %>% 
         arrange(fecha),
       "line", 
       hcaes(x = as.Date(fecha), y = valor, group = tipo)) %>% 
  hc_colors(c("#ee4035" ,"#0392cf", "#7bc043")) %>% 
  hc_yAxis(title = list(text = "Valor en Bs. por m3")) %>% 
  hc_xAxis(title = list(text = ""))





margen %>% group_by(mes = month(fecha), gestion =year(fecha)) %>% 
  summarise(`Precio por m3` = mean(precio_bs_m3),
            `Costo por m3` = mean(costo_total_bs_m3),
            `Margen por m3` = mean(margen_bs_m3)) %>%
  mutate(`Costo por m3` = (-`Costo por m3`)) 



# comparacion Precios/m3
hchart(margen %>% group_by(mes = month(fecha), gestion =year(fecha)) %>% 
         summarise(`Precio por m3` = mean(precio_bs_m3),
                   `Costo por m3` = mean(costo_total_bs_m3),
                   `Margen por m3` = mean(margen_bs_m3)) %>%
         mutate(`Costo por m3` = (-`Costo por m3`)),
       "line", 
       hcaes(x = month.name[mes], y = `Precio por m3`, group = gestion)) %>% 
  hc_yAxis(title = list(text = "Precio en Bs. por m3")) %>% 
  hc_xAxis(title = list(text = ""))


# comparacion costo/m3
hchart(margen %>% group_by(mes = month(fecha), gestion =year(fecha)) %>% 
         summarise(`Precio por m3` = mean(precio_bs_m3),
                   `Costo por m3` = mean(costo_total_bs_m3),
                   `Margen por m3` = mean(margen_bs_m3)),
       "line", 
       hcaes(x = month.name[mes], y = `Costo por m3`, group = gestion)) %>% 
  hc_yAxis(title = list(text = "Costo en Bs. por m3")) %>% 
  hc_xAxis(title = list(text = ""))

# comparacion margen/m3
hchart(margen %>% group_by(mes = month(fecha), gestion =year(fecha)) %>% 
         summarise(`Precio por m3` = mean(precio_bs_m3),
                   `Costo por m3` = mean(costo_total_bs_m3),
                   `Margen por m3` = mean(margen_bs_m3)),
       "line", 
       hcaes(x = month.name[mes], y = `Margen por m3`, group = gestion)) %>% 
  hc_yAxis(title = list(text = "Margen en Bs. por m3")) %>% 
  hc_xAxis(title = list(text = ""))



# Totales

# margen total
hchart(margen %>% group_by(mes = month(fecha), gestion =year(fecha)) %>% 
         summarise(`Precio total` = sum(precio_total_venta_bs),
                   `Costo total` = sum(costo_total_bs),
                   `Margen` = `Precio total`-`Costo total`),
       "line", 
       hcaes(x = month.name[mes], y = `Margen`, group = gestion)) %>% 
  hc_yAxis(title = list(text = "Margen total en Bs.")) %>% 
  hc_xAxis(title = list(text = ""))


# precio total
hchart(margen %>% group_by(mes = month(fecha), gestion =year(fecha)) %>% 
         summarise(`Precio total` = sum(precio_total_venta_bs),
                   `Costo total` = sum(costo_total_bs),
                   `Margen` = `Precio total`-`Costo total`),
       "line", 
       hcaes(x = month.name[mes], y = `Precio total`, group = gestion)) %>% 
  hc_yAxis(title = list(text = "Ingreso total en Bs.")) %>% 
  hc_xAxis(title = list(text = ""))

# costo total
hchart(margen %>% group_by(mes = month(fecha), gestion =year(fecha)) %>% 
         summarise(`Precio total` = sum(precio_total_venta_bs),
                   `Costo total` = sum(costo_total_bs),
                   `Margen` = `Precio total`-`Costo total`),
       "line", 
       hcaes(x = month.name[mes], y = `Costo total`, group = gestion)) %>% 
  hc_yAxis(title = list(text = "Costo total en Bs.")) %>% 
  hc_xAxis(title = list(text = ""))





margen %>% group_by(mes = month(fecha), gestion =year(fecha)) %>% 
  summarise(`Precio total` = sum(precio_total_venta_bs),
            `Costo total` = sum(costo_total_bs),
            `Margen` = `Precio total`-`Costo total`) 


avg_mes %>% select(mes_year, precio_total_venta_bs, costo_total_bs, margen_bruto_bs)