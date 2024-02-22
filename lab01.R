library(tidyverse)

# leitura -----------------------------------------------------------------

olist <- arrow::read_parquet(
  "https://github.com/jtrecenti/202431-padsv/releases/download/dados/olist_items.parquet"
)

# exercício 1 ----------------------------------------------------------

contagem <- olist %>%
  count(types) %>%
  filter(n > 100)

df_plot1 <- data.frame(
  types = contagem$types, frequencia = contagem$n
)

df_plot1$types <- factor(df_plot1$types, levels = df_plot1$types[order(df_plot1$frequencia)])

df_plot1 <- df_plot1 |>
  mutate(
    types = fct_reorder(types, frequencia)
  )

plot1 <- df_plot1 |>
  ggplot(aes(x = frequencia / 1000, y = types, label = round(frequencia / 1000, 2))) +
  geom_col(fill = "#A8E1D7", width = 0.5) +
  labs(
    title = "Formas de pagamento mais comuns",
    subtitle = "Considerando tipos com mais de 100 observações",
    x = "Quantidade\n(milhares)",
    y = "Formas de pagamento",
    fill = "Formas de pagamento",
    caption = "Fonte: Olist"
  ) +
  geom_label(
    aes(x = frequencia / 1000 / 2)
  ) +
  theme(
    panel.background = element_rect(fill = "gray20"),
    plot.background = element_rect(fill = "gray10"),
    text = element_text(color = "white", family = "serif"),
    axis.text = element_text(color = "white", family = "serif"),
    panel.grid.minor = element_blank()
  )

print(plot1)


# exercício 2 ----------------------------------------------------------

# exercício 3 ----------------------------------------------------------

# exercício 4 ----------------------------------------------------------

