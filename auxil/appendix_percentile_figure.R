library(CKutils)
dependencies(c("ggplot2", "cowplot", "fst"))


sp <- list.files("./inputs/synthpop/", full.names = T)[1]

sp <- read_fst(sp, as.data.table = TRUE)


sp <- sp[age %in% c(30, 50) & sex == "women" & year == 14]

theme_set(theme_cowplot())

ggplot(sp, aes(x = bmi_curr_xps, col = factor(age))) +
  stat_ecdf(linewidth = 1.3) +
  geom_segment(aes(x = 21.9, xend = 21.9, y = 0, yend = 0.5),
               col = "black", linewidth = 1, linetype = "dashed") +
  geom_segment(aes(x = 24.5, xend = 24.5, y = 0, yend = 0.5),
               col = "black", linewidth = 1, linetype = "dashed") +
  geom_segment(aes(x = 12, xend = 24.5, y = 0.5, yend = 0.5),
               col = "black", linewidth = 1, linetype = "dashed") +
  xlab("BMI (kg/m²)") +
  ylab("Percentile") +
  labs(col = "Age (years)") +
  scale_x_continuous(limits = c(12, 50))

ggsave2("Percentile_example.jpg", width = 9, height = 7, units = "cm",
                 scale = 2, dpi = 300, path = "./outputs/appendix/")
