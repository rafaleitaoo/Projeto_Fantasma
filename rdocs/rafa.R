
install.packages("pacman")

cores_estat <- c("#A11D21", "#003366", "#CC9900", "#663333", "#FF6600", "#CC9966", "#999966", "#006606", "#008091", "#041835", "#666666")

theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  return(
    list(
      theme,
      scale_fill_manual(values = cores_estat),
      scale_colour_manual(values = cores_estat)
    )
  )
}




library(pacman)
p_load(readxl, knitr, dplyr, tidyverse, ggplot2)
p_load(lubridate)




# Baixando o banco e tirando as duplicatas

vendas <- "C:\\Users\\Lisandre\\Desktop\\ESTAT\\Projeto_Fantasma\\banco\\vendas.csv" %>%read.csv()
vendas_limpo <- vendas %>%
  distinct(Unique.ID, .keep_all = TRUE)
vendas_limpo <- vendas_limpo %>%
  rename(Categoria=Category)

devolucao<- "C:\\Users\\Lisandre\\Desktop\\ESTAT\\Projeto_Fantasma\\banco\\devolução_atualizado.csv" %>%read.csv()
dev_limpo <- devolucao %>%
  distinct(Unique.ID, .keep_all = TRUE)







# Análise 1 Faturamento Anual por Categoria

cpmlimpo <- vendas_limpo[!is.na(vendas_limpo$Price) & !is.na(vendas_limpo$Data.Venda) & !is.na(vendas_limpo$Categoria), ]


cpmlimpo<-cpmlimpo%>%
mutate(
  Categoria = fct_recode(
    Categoria,
    "Moda Infantil" = "Kids' Fashion",
    "Moda Masculina" = "Men's Fashion",
    "Moda Feminina" = "Women's Fashion"
  )
)

cpmlimpo$Data.Venda <- as.Date(cpmlimpo$Data.Venda, format = "%m/%d/%Y")
cpmlimpo$Mes <- month(cpmlimpo$Data.Venda)
vendas_agrupadas <- cpmlimpo %>%
  group_by(Mes, Categoria) %>%
  summarise(total_Preco = sum(Price))


ggplot(vendas_agrupadas) +
  aes(x = Mes, y = total_Preco, group = Categoria, colour = Categoria) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_colour_manual(name = "Categoria", labels = ) +
  labs(x = "Mês", y = "Preço") +
  scale_x_continuous(breaks = 1:12, labels = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")) +
  theme_estat()

ggsave("analise1_grupo.pdf", width = 158, height = 93, units = "mm")







  
# analise 2 Variação do preço por marca
  
  marcaslimpas <- vendas_limpo[!is.na(vendas_limpo$Brand), ]

ggplot(marcaslimpas) +
  aes(x = Brand, y = Price) +
  geom_boxplot(fill = "#A11D21", width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Marca", y = "Preço") +
  theme_estat()
ggsave("analise2.pdf", width = 158, height = 93, units = "mm")


quadro_resumo <- marcaslimpas %>%
  group_by(Brand) %>%
  summarize(Média = round(mean(Price, na.rm = TRUE), 2),
            `Desvio Padrão` = round(sd(Price, na.rm = TRUE), 2),
            `Variância` = round(var(Price, na.rm = TRUE), 2),
            `Mínimo` = round(min(Price, na.rm = TRUE), 2),
            `1º Quartil` = round(quantile(Price, probs = 0.25, na.rm = TRUE), 2),
            Mediana = round(quantile(Price, probs = 0.5,na.rm = TRUE), 2),
            `3º Quartil` = round(quantile(Price, probs = 0.75, na.rm = TRUE), 2),
            `Máximo` = round(max(Price, na.rm = TRUE), 2)
  ) %>%
  t() %>%
  as.data.frame() %>%
  mutate(V1 = str_replace(V1, "\\.", ","))

 xtable::xtable(quadro_resumo)
 
 


 
 



# Análise 3 relação entre categoria (fem e masc) e cor

color_category <- vendas_limpo %>%
  filter(!is.na(Categoria) & !is.na(Color) & !grepl("Kids", Categoria, ignore.case = TRUE)) %>%
  group_by(Color, Categoria) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = scales::percent(freq / sum(freq)),
    label = str_c(freq, " (", freq_relativa, ")")
  ) %>%
  mutate(
    Color = fct_recode(
      Color,
      "Branco" = "White",
      "Azul" = "Blue",
      "Verde" = "Green",
      "Preto" = "Black",
      "Vermelho" = "Red",
      "Amarelo" = "Yellow"
    ),
    Categoria = fct_recode(
      Categoria,
      "Masculino" = "Men's Fashion",
      "Feminino" = "Women's Fashion"
    )
  )

ggplot(color_category) +
  aes(
    x = fct_reorder(Color, freq, .desc = TRUE), y = freq,
    fill = Categoria, label = label
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = 0.9),
    vjust = -0.5, hjust = 0.5,
    size = 1.8
  ) +
  labs(x = "Cor", y = "Frequência") +
  theme_estat()

ggsave("analise3.pdf", width = 158, height = 93, units = "mm")








# Análise 4 Relação de Preço e Avaliação

ggplot(vendas_limpo) +
  aes(x = Price, y = Rating) +
  geom_jitter(colour = "#A11D21", size = 3, alpha = 0.3) +
  labs(
    x = "Preço",
    y = "Avaliação"
  ) +
  theme_estat()

ggsave("analise4.pdf", width = 158, height = 93, units = "mm")

# correlação de pearson
rplimpo <- vendas_limpo%>%
  filter(!is.na(Price) & !is.na(Rating))

correlacao <- cor(rplimpo$Price, rplimpo$Rating)
view(correlacao)

# quadro
novo_av <- vendas_limpo %>%
  mutate(Rating = round(Rating))
  
novo_av <- novo_av[!is.na(novo_av$Rating), ]

# quadro em relação a avaliação
quadro_r <- vendas_limpo %>%
  summarize(Média = round(mean(Rating, na.rm = TRUE), 2),
            `Desvio Padrão` = round(sd(Rating, na.rm = TRUE), 2),
            `Mínimo` = round(min(Rating, na.rm = TRUE), 2),
            `1º Quartil` = round(quantile(Rating, probs = 0.25, na.rm = TRUE), 2),
            Mediana = round(quantile(Rating, probs = 0.5,na.rm = TRUE), 2),
            `3º Quartil` = round(quantile(Rating, probs = 0.75, na.rm = TRUE), 2),
            `Máximo` = round(max(Rating, na.rm = TRUE), 2)
  ) %>%
  t() %>%
  as.data.frame() %>%
  mutate(V1 = str_replace(V1, "\\.", ","))

xtable::xtable(quadro_r)


 # quadro em relação ao preço
quadro_p <- vendas_limpo %>%
  summarize(Média = round(mean(Price, na.rm = TRUE), 2),
            `Desvio Padrão` = round(sd(Price, na.rm = TRUE), 2),
            `Mínimo` = round(min(Price, na.rm = TRUE), 2),
            `1º Quartil` = round(quantile(Price, probs = 0.25, na.rm = TRUE), 2),
            Mediana = round(quantile(Price, probs = 0.5,na.rm = TRUE), 2),
            `3º Quartil` = round(quantile(Price, probs = 0.75, na.rm = TRUE), 2),
            `Máximo` = round(max(Price, na.rm = TRUE), 2)
  ) %>%
  t() %>%
  as.data.frame() %>%
  mutate(V1 = str_replace(V1, "\\.", ","))
xtable::xtable(quadro_p)







# Análise 5 frequencia de cada tipo de devolução por marca



banco <- merge(vendas_limpo, dev_limpo, by = "Unique.ID", all = TRUE)

banco$Motivo_Devolucao <- ifelse(!is.na(banco$Motivo.devolução.y), banco$Motivo.devolução.y, banco$Motivo.devolução.x)

banco <- banco[, !(names(banco) %in% c("Motivo.devolução.x", "Motivo.devolução.y"))]

banco <- banco %>%
  rename(Devolução = Motivo_Devolucao)
grafico <- banco %>%
  filter(!is.na(Brand) & !is.na(Devolução)) %>%
  group_by(Brand, Devolução) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = scales::percent(freq / sum(freq)),
    label = str_c(freq, " (", freq_relativa, ")")
  )

 

ggplot(grafico) +
  aes(
    x = fct_reorder(Brand, freq, .desc = TRUE), y = freq,
    fill = Devolução, label = label
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = 0.9),
    vjust = 0.2, hjust = 0,
    size = 2.0
  ) +
  labs(x = "Marca", y = "Frequência") +
  coord_flip() +
  theme_estat()

ggsave("analise5.pdf", width = 158, height = 93, units = "mm")






# Análise 6 média de avaliação por marca

allmp <- vendas_limpo[!is.na(vendas_limpo$Rating) & !is.na(vendas_limpo$Brand), ]


medias <- allmp %>%
  group_by(Brand) %>%  
  summarise(Media_Avaliacao = mean(Rating, na.rm = TRUE)) 


ggplot(medias) +
  aes(x = fct_reorder(Brand, Media_Avaliacao, .desc = TRUE), y = Media_Avaliacao, label = round(Media_Avaliacao, 2)) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3
  ) +
  labs(x = "Marcas", y = "Avaliação Média") +
  theme_estat()

ggsave("analise6.pdf", width = 158, height = 93, units = "mm")











