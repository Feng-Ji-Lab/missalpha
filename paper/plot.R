library(devtools)
load_all()

set.seed(0)
score_max <- 2
scores_mat_bernoulli <- generate_scores_mat_bernoulli(
  n_person = 50,
  n_item = 10,
  n_missing = 20,
  score_max = score_max
)

result = cronbachs_alpha(
    scores_mat_bernoulli, score_max, enum_all = FALSE
)
summary(result)

library(ggplot2)
library(tidyr)
library(dplyr)
library(forcats)

plot_missing <- as_tibble(scores_mat_bernoulli) %>%
  mutate(Person = row_number()) %>%
  pivot_longer(
    cols = -Person,
    names_to = "Item",
    values_to = "Value"
  ) %>%
  mutate(
    Missing = is.na(Value),
    Person = fct_rev(factor(Person))  # 反转顺序
  )
plot_missing <- plot_missing %>%
  mutate(Item = factor(Item, levels = c(paste0("V", 1:9), "V10")))

# 绘图
p <- ggplot(plot_missing, aes(x = Item, y = Person)) +
  geom_tile(aes(fill = Missing), color = "white", linewidth = 0.3) +
  scale_fill_manual(values = c("TRUE" = "#e63946", "FALSE" = "#f1faee"),
                    labels = c("Present", "Missing")) +
  scale_y_discrete(breaks = levels(plot_missing$Person)[seq(1, 50, 5)]) +
  labs(
    title = "Missing Data Pattern",
    x = "Item",
    y = "Person",
    fill = "Data Status"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

# 显示并保存图
print(p)
ggsave("missing_data_map.png", plot = p, width = 8, height = 6, dpi = 300)
