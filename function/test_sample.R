library(treemap)
data(GNI2014, package = "treemap")
subset(GNI2014, subset = grepl(x = country, pattern = 'China'))
treemap(GNI2014,
        index = c("continent", "iso3"),
        vSize = "population", 
        vColor = "GNI",
        type = "value",
        format.legend = list(scientific = FALSE, big.mark = " ")
)

treemap(df |> filter(DBDX == 1),
        index = c("DBDX","DBTYPE","DBRX"),
        vSize = "RUHP6Q", 
        vColor = "RUHP6Q", 
        type = "value",
        format.legend = list(scientific = FALSE, big.mark = " ")
)


writexl::write_xlsx(df,"./Data/rawdata_0316.xlsx")   
devtools::install_github("yogevherz/plotme")

library(plotme)
library(dplyr)
library(palmerpenguins)
View(penguins)
penguins %>% 
  count() %>% 
  count_to_sunburst()
df |>
  count(DBDX, DBTYPE, DBIN, DBRX#,wt = RUHP6Q
        ) |>
  count_to_sunburst(fill_by_n = TRUE)


boxplot(Ozone ~ Month, data = airquality)
wilcox.test(Ozone ~ Month, data = airquality,
            subset = Month %in% c(5, 8))
# 加载tidyverse包，主要用于数据处理
library(tidyverse)

# 创建一个包含有序变量的数据框
df <- tibble(
  ord_var = factor(c("Low", "Medium", "High", "Medium", "Low"), levels = c("Low", "Medium", "High"), ordered = TRUE)
)

# 查看数据框
print(df)
# 创建模型矩阵
mat <- model.matrix(~ ord_var, data = df)

# 查看结果
print(mat)
library(plotly)
library(data.table)
diamonds <- as.data.table(df_encode |> filter(DBDX == "Diagnosed"))
names(diamonds)
diamonds[, .(cnt = .N), by = .(DBMed)] %>%
  plot_ly(x = ~DBMed, y = ~cnt, type = "bar") %>%
  add_text(
    text = ~ scales::comma(cnt), y = ~cnt,
    textposition = "top middle",
    cliponaxis = FALSE, showlegend = FALSE
  )


plot_ly(diamonds,
        x = ~DBMed,  y = ~RUHP6Q, color = ~DBTYPE,
        colors = "Accent", type = "bar"
) 

df_encode |> 
  filter(DBDX == "Diagnosed") |>
  filter(RUHP6Q >0) |>
 # mutate(neighbourhood = fct_reorder(neighbourhood, price)) |> 
  plot_ly(y = ~RUHP6Q, color = ~DBMed, type = "box", colors = "viridis")
