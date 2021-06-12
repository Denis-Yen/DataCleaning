
# DATA CLEANING: FALLECIDOS POR COVID-19 EN EL PERU
# Autor: Denis Rodriguez
# Fecha: 05/06/2021


# CARGA DE LIBRERIAS A USAR -----------------------------------------------
library(lubridate)
library(tidyverse)
library(ggdark)
library(extrafont)


# IMPORTACIÓN DE LOS DATOS ------------------------------------------------

data <- read.csv("https://cloud.minsa.gob.pe/s/Md37cjXmjT9qYSa/download",
                 sep = ";")
head(data)
tail(data)
str(data)


# LIMPIEZA DE LOS DATOS ---------------------------------------------------
# Primera Forma
data2 <- data %>%
  mutate(
    FECHA_CORTE = NULL,
    UUID = NULL,
    Year  = substring(FECHA_FALLECIMIENTO, 1,4),
    Month = substring(FECHA_FALLECIMIENTO, 5,6),
    Day   = substring(FECHA_FALLECIMIENTO, 7,8),
    Departamento = as.factor(str_to_title(DEPARTAMENTO)),
    Provincia    = as.factor(PROVINCIA),
    Distrito     = as.factor(DISTRITO),
    Sexo = as.factor(SEXO),
    CriterioMuerte = CLASIFICACION_DEF,
    Ubigeo = UBIGEO,
    Edad = as.integer(EDAD_DECLARADA)) %>%
  select(Year, Month, Day, Ubigeo, Departamento, Provincia, Distrito, Sexo, Edad, CriterioMuerte)
class(data2)

# Guardamos la data
write.csv(data2, "D:/Ciencia de datos/FallecidosCovid/Data/data2.csv",
          fileEncoding = "UTF-8")

# Segunda forma
data3  <- data %>%
  separate(FECHA_FALLECIMIENTO, sep = 4, into = c("Anio", "Mes"), convert = T) %>%
  separate(Mes, sep=2, into=c("Mes", "Dia"), convert = T) %>%
  mutate(FECHA_CORTE = NULL,
         UUID = NULL,
         Departamento = as.factor(DEPARTAMENTO),
         Provincia    = as.factor(PROVINCIA),
         Distrito     = as.factor(DISTRITO),
         Sexo = as.factor(SEXO),
         CriterioMuerte = CLASIFICACION_DEF,
         Ubigeo = UBIGEO,
         Edad = as.integer(EDAD_DECLARADA)
         ) %>%
  select(Anio, Mes, Dia, Ubigeo, Departamento, Provincia, Distrito, Sexo, Edad, CriterioMuerte)
 

# Unimos el campo año, mes y dia al campo fechaMuerte y lo convertimos a tipo date.
data21 <- data2 %>% unite(fecha, Year,Month, sep = "-") %>%
  unite(FechaMuerte, fecha, Day, sep = "-") %>%
  mutate(FechaMuerte2 = as.Date(FechaMuerte))

# guardamos data21
write.csv(data21, "D:/Ciencia de datos/FallecidosCovid/Data/data21.csv",
          fileEncoding = "UTF-8")

# FALLECIDOS POR COVID - 19 (Acumulación diaria) -------------------------
data21 %>%
  #filter(FechaMuerte2 > "2020-12-31") %>%
  count(day = floor_date(FechaMuerte2, "day")) %>%
  ggplot(aes(day, n)) +
  geom_line(color = "#00BB92", size = 0.8) +
  labs(title = "Fallecidos por COVID-19 (acumulación diaria)",
       subtitle = "07/04/2019 - 09/06/2021",
       caption = "Fuente: Centro Nacional de Epidemiologia, Prevención y Control de Enfermedades – MINSA \nAutor: Denis Rodríguez",
       x = "Semana",
       y = "Catidad de fallecidos") +
  theme_grey(base_size = 12, base_line_size = 0.6, base_rect_size = 0.6) -> p1

  p1 + dark_theme_dark(base_family = "Arial Black", base_size = 10) +
  theme(plot.title = element_text(family = "Arial Black", hjust = 0.5),
        plot.subtitle = element_text(family = "Arial Black", hjust = 0.5),
        plot.caption = element_text(family = "Times New Roman", hjust = 0),
        plot.background = element_rect(fill = "#121E1E"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray30", size = 0.2),
        panel.grid.minor = element_line(color = "gray30", size = 0.2)) -> graph1


# FALLECIDOS POR COVID - 19 POR DEPARTAMENTO --------------------------

  data21 %>%
    #filter(FechaMuerte2 > "2020-12-31") %>%
    ggplot(aes(y = Departamento)) +
    geom_bar(color = "#121E1E", fill = "#00BB92") +
    labs(title = "Cantidad de fallecidos por COVID-19 por departamento",
         subtitle = "07/04/2019 - 09/06/2021",
         caption = "Fuente: Centro Nacional de Epidemiologia, Prevención y Control de Enfermedades – MINSA \nAutor: Denis Rodríguez",
         x = "Cantidad de fallecidos",
         y = "Departamento") + 
    
    dark_theme_dark(base_family = "Calibri", base_size = 11) +
    theme(plot.title = element_text(family = "Calibri", hjust = 0.5),
          plot.subtitle = element_text(family = "Calibri", hjust = 0.5),
          plot.caption = element_text(family = "Calibri", hjust = 0),
          plot.background = element_rect(fill = "#121E1E"),
          panel.background = element_blank(),
          panel.grid.major = element_line(color = "gray30", size = 0.2),
          panel.grid.minor = element_line(color = "gray30", size = 0.2)) -> graph2

  
# ALGUNOS INSIGHTS --------------------------------------------------------

# Calcular la edad promedio de fallecidos por departamento.
data21 %>%
  group_by(Departamento) %>%
  summarise(EdadPromedio = mean(Edad, na.rm = T))


# Edad promedio de muerte por criterio de fallecimiento
data21 %>%
  group_by(CriterioMuerte) %>%
  summarise(EdadPromedio = mean(Edad, na.rm = T)) %>% 
  arrange(desc(EdadPromedio))

# Cantidad de fallecidos por departamento y edad promedio de muerte
data21 %>%
  group_by(Departamento) %>%
  summarise(Cantidad = n(),
            Edadpromedio = mean(Edad, na.rm = T)) %>% view()

# Cantidad de fallecidos para los distritos de Lima y edad promedio de muerte
data21$Distrito <- str_to_title(data21$Distrito)


# Cantidad de fallecidos por covid-19 para los distritos de Lima Metropolitana
data21 %>%
  filter(Departamento == "Lima Metropolitana") %>%
  group_by(Distrito) %>%
  summarise(Cantidad = n(),
            Edadpromedio = round(mean(Edad, na.rm = T),2)) %>%
  arrange(desc(Cantidad)) -> fallecidosLima

# Exportamos los datos a formato excel.
xlsx::write.xlsx(fallecidosLima, "D:/Ciencia de datos/FallecidosCovid/Recursos/FallecidoLima.xlsx",)  

# End



