## Code to reproduce the figures in the main manuscript


# Settings -----------------------------------------------------------------
## load the libraries
library(tidyverse)
library(epitools)

theme_set(theme_bw())
fig_path <- "./results/figures/"

## load the data
# data <- readRDS("not-available")



# Fig1 --------------------------------------------------------------------
data |> 
  mutate(CategoriaDef = factor(CategoriaDef, 
                               levels = c("Youth", "Professional"))) |> 
  group_by(Temporada2, CategoriaDef) |> 
  summarise(nles = sum(lesio == "Si")) |> 
  ggplot(aes(x = Temporada2, y = nles, fill = CategoriaDef, 
             group = CategoriaDef)) +
  geom_col(show.legend = F) +
  # geom_point() +
  geom_text(aes(label = nles), nudge_y = 3) +
  facet_wrap(~CategoriaDef,
             labeller = labeller(CategoriaDef = c(Youth = "Youth", 
                                                  "Professional" = "Senior"))) +
  scale_fill_manual(name = "", values = c("orange", "dodgerblue")) +
  ylab("Number of injuries") + xlab("Season") + 
  theme(axis.text = element_text(size = rel(1.3)),
        axis.title = element_text(size = rel(1.3)),
        legend.text = element_text(size = rel(1.3)),
        strip.text = element_text(face = "bold", size = rel(1.3)))
ggsave(paste0(fig_path, "Figure 1.jpg"), width = 12.2, height = 4.8, dpi = 300)
ggsave(paste0(fig_path, "Figure 1.pdf"), device = "pdf", width = 12.2, height = 4.8)




# Fig2 --------------------------------------------------------------------
inc_prop_temp <- data |> 
  distinct(Temporada2, CategoriaDef, Name, lesio) |> 
  group_by(Temporada2, CategoriaDef) |> 
  summarise(n_indiv_les = sum(lesio=="Si"), 
            n_indiv = n(), 
            prev_temp = round(n_indiv_les/n_indiv*100,2),
            ci_low = binom.wilson(n_indiv_les, n_indiv)$lower*100,
            ci_up = binom.wilson(n_indiv_les, n_indiv)$upper*100)

inc_prop_temp |> 
  mutate(CategoriaDef = factor(CategoriaDef, 
                               levels = c("Youth", "Professional"))) |> 
  ggplot(aes(x = Temporada2, y = prev_temp, 
             fill = CategoriaDef, group = CategoriaDef)) +
  geom_col(show.legend = F) + 
  geom_point(size = 1) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_up),
    position = position_dodge(width = 0.9), 
    width = 0.25,      
    color = "black",
    linewidth = 0.7   
  ) +
  geom_hline(yintercept  = 100, col = "black") + 
  geom_text(aes(label = paste0(n_indiv_les, "/", n_indiv)),
            position = position_dodge(width = 0.9),
            vjust = 1,
            col = "grey25", size = 4) + 
  # geom_smooth(aes(col = CategoriaDef), se = F, show.legend = F) +
  facet_wrap(~CategoriaDef,
             labeller = labeller(CategoriaDef = c(Youth = "Youth", "Professional" = "Senior"))) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(name = "", values = c("orange", "dodgerblue")) +
  scale_color_manual(name = "", values = c("orange", "dodgerblue")) +
  xlab("Season") + ylab("Incidencie Proportion") + 
  theme(axis.text = element_text(size = rel(1.3)),
        axis.title = element_text(size = rel(1.3)),
        legend.text = element_text(size = rel(1.3)),
        strip.text = element_text(face = "bold", size = rel(1.3)),
        legend.position = "none")
ggsave(paste0(fig_path, "Figure 2.jpg"), width = 13.2, height = 5, dpi = 300)
ggsave(paste0(fig_path, "Figure 2.pdf"), device = "pdf", width = 13.2, height = 5)



# Fig4 --------------------------------------------------------------------
data_sev <- data |>
  filter(lesio == "Si") |> 
  droplevels() |> 
  mutate(days_lost = ifelse(lesio == "Si" & is.na(days_lost), 0, days_lost),
         severity = case_when(days_lost <= 3 ~ "Slight",
                              days_lost > 3 & days_lost <= 7 ~ "Minor",
                              days_lost > 7 & days_lost <= 28 ~ "Moderate",
                              days_lost > 28  ~ "Severe"))

data_sev |> 
  mutate(severity = factor(severity,
                           levels = c("Slight", "Minor", "Moderate", "Severe")),
         CategoriaDef = factor(CategoriaDef, levels = c("Youth", "Professional"))) |> 
  ggplot(aes(x = severity, fill = CategoriaDef)) +
  geom_bar(show.legend = F) +
  # geom_point() +
  geom_text(stat = "count", aes(label = after_stat(count)), nudge_y = 7) +
  facet_wrap(~CategoriaDef,
             labeller = labeller(CategoriaDef = c(Youth = "Youth", "Professional" = "Senior"))) +
  scale_fill_manual(name = "", values = c("orange", "dodgerblue")) +
  ylab("Number of injuries") + xlab("Severity") + 
  theme(axis.text = element_text(size = rel(1.3)),
        axis.title = element_text(size = rel(1.3)),
        legend.text = element_text(size = rel(1.3)),
        strip.text = element_text(face = "bold", size = rel(1.3)))
ggsave(paste0(fig_path, "Figure 4.jpg"), width = 12.5, height = 5, dpi = 300)
ggsave(paste0(fig_path, "figure 4.pdf"), device = "pdf", width = 12.2, height = 5)



