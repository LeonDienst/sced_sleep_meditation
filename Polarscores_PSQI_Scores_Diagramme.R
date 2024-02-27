library(tidyverse)
library(openxlsx)


sced <- read.csv("sced2.csv", sep = ";")

sced$Datum <- as.Date(sced$Datum, format =  "%d.%m.%Y" )

str(sced)


# pivot longer und datatype-Veränderungen-------------------------------------------------------------------------
longer_sced <- gather(sced, key = "Key", value = "Value", -Datum)
longer_sced$Value <- as.integer(longer_sced$Value)
longer_sced$Datum <- as.Date(longer_sced$Datum, format = "%d.%m.%Y")


# neue Spalte erstellen -- Treatment Status ------------------------------------
longer_sced <- longer_sced %>%
  mutate(
    Treatment_Baseline = case_when(
      (Key == "Leon.PSQI" & Datum > as.Date("2023-12-08")) |
        (Key == "Max.PSQI" & Datum > as.Date("2023-12-05")) |
        (Key == "Sascha.PSQI" & Datum > as.Date("2023-12-07")) |
        (Key == "Rica.PSQI" & Datum > as.Date("2023-12-06")) |
        (Key == "Paul.PSQI" & Datum > as.Date("2023-12-09"))  |
        (Key == "Leon.Polar" & Datum > as.Date("2023-12-08"))  |
        (Key == "Max.Polar" & Datum > as.Date("2023-12-05"))    |
        (Key == "Sascha.Polar" & Datum > as.Date("2023-12-07"))  |
        (Key == "Rica.Polar" & Datum > as.Date("2023-12-06"))   |
        (Key == "Paul.Polar" & Datum > as.Date("2023-12-09"))~ "Treatment",
      TRUE ~ "Baseline"
    )
  )



# Df nur mit .Polaruhr-Daten ----------------------------------------------
polar_longer <- filter(longer_sced, grepl("\\.Polar$", Key)) 



### Change Names for the Keys
polar_longer <- polar_longer %>%
  mutate(Key = case_when(Key == "Leon.Polar" ~ "Person 1",
                         Key == "Max.Polar" ~ "Person 2",
                         Key == "Paul.Polar" ~ "Person 3",
                         Key == "Rica.Polar" ~ "Person 4",
                         Key == "Sascha.Polar" ~ "Person 5",
                         TRUE ~ Key))

# Define a data frame with the facet names and corresponding xintercept dates
vline_data <- data.frame(
  Key = unique(polar_longer$Key),
  xintercept_date = as.Date(c("2023-12-08", "2023-12-05", "2023-12-07", "2023-12-06", "2023-12-09"))
)


## Hinzufügen von Line für Treatment bzw. Baseline
polar_longer %>% ggplot(aes(Datum, Value, col = Key))+
  geom_point()+
  geom_line()+
  facet_wrap(~Key, scales = "fixed")+
  ggtitle("Scores der Polaruhren")+
  scale_x_date(date_breaks = "1 day", date_labels = "%d.%m.%Y")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_vline(data = vline_data, aes(xintercept = xintercept_date), linetype = "dashed", color = "black", size = 1, alpha = .5)+
  scale_x_date(
    breaks = seq(from = as.Date("2023-12-01"), to = max(polar_longer$Datum), by = "2 days"),
    labels = scales::date_format("%d.%m.%Y")
  ) +
  labs(col = "Versuchsperson")+
  ylab("Polar-Score")


### Graph mit Loess (eine einzige Loess)
polar_longer %>%
  ggplot(aes(Datum, Value, col = Key)) +
  geom_point() +
  geom_line() +
  geom_smooth(se = F)+
  facet_wrap(~Key, scales = "fixed") +
  ggtitle("Scores der Polaruhren") +
  labs(col = "Versuchsperson") +  # Legend title
  ylab("Polar-Score") +  # Y-axis label
  scale_x_date(
    breaks = seq(from = as.Date("2023-12-01"), to = max(polar_longer$Datum), by = "2 days"),
    labels = scales::date_format("%d.%m.%Y")
  ) +
  theme_minimal() +  # Use a minimal theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",  # Position legend at the top
    legend.title = element_text(face = "bold"),  # Bold legend title
    legend.text = element_text(size = 9),  # Adjust legend text size
    plot.title = element_text(hjust = 0.5),  # Center plot title
    plot.caption = element_text(hjust = 0),  # Left-align plot caption
    strip.text = element_text(margin = margin(b = 10)),
    panel.spacing.x = unit(3, "cm") 
  ) +
  geom_vline(data = vline_data, aes(xintercept = xintercept_date), linetype = "dashed", color = "black", size = 1, alpha = .5)



# polar geom_smooth(method = lm) ------------------------------------------

### Graph mit Loess (eine einzige Loess)
polar_longer %>%
  ggplot(aes(Datum, Value, col = Key)) +
  geom_point() +
  geom_line() +
  geom_smooth(se = F, method = "lm")+
  facet_wrap(~Key, scales = "fixed") +
  ggtitle("Scores der Polaruhren") +
  labs(col = "Versuchsperson") +  # Legend title
  ylab("Polar-Score") +  # Y-axis label
  scale_x_date(
    breaks = seq(from = as.Date("2023-12-01"), to = max(polar_longer$Datum), by = "2 days"),
    labels = scales::date_format("%d.%m.%Y")
  ) +
  theme_minimal() +  # Use a minimal theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",  # Position legend at the top
    legend.title = element_text(face = "bold"),  # Bold legend title
    legend.text = element_text(size = 9),  # Adjust legend text size
    plot.title = element_text(hjust = 0.5),  # Center plot title
    plot.caption = element_text(hjust = 0),  # Left-align plot caption
    strip.text = element_text(margin = margin(b = 10)),
    panel.spacing.x = unit(3, "cm") 
  ) +
  geom_vline(data = vline_data, aes(xintercept = xintercept_date), linetype = "dashed", color = "black", size = 1, alpha = .5)




### Graph mit zwei Loess Funktionen
polar_longer %>%
  ggplot(aes(Datum, Value, col = Key)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x, 
              data = polar_longer %>%
                inner_join(vline_data, by = "Key") %>%
                filter(Datum < xintercept_date)) +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x, 
              data = polar_longer %>%
                inner_join(vline_data, by = "Key") %>%
                filter(Datum >= xintercept_date)) +
  facet_wrap(~Key, scales = "fixed") +
  ggtitle("Scores der Polaruhren") +
  labs(col = "Versuchsperson") +  # Legend title
  ylab("Polar-Score") +  # Y-axis label
  scale_x_date(
    breaks = seq(from = as.Date("2023-12-01"), to = max(polar_longer$Datum), by = "2 days"),
    labels = scales::date_format("%d.%m.%Y")
  ) +
  theme_minimal() +  # Use a minimal theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",  # Position legend at the top
    legend.title = element_text(face = "bold"),  # Bold legend title
    legend.text = element_text(size = 9),  # Adjust legend text size
    plot.title = element_text(hjust = 0.5),  # Center plot title
    plot.caption = element_text(hjust = 0),  # Left-align plot caption
    strip.text = element_text(margin = margin(b = 10)),
    panel.spacing.x = unit(3, "cm") 
  ) +
  geom_vline(data = vline_data, aes(xintercept = xintercept_date), linetype = "dashed", color = "black", size = 1, alpha = .5)




# Df nur mit .PSQI-Daten --------------------------------------------------
psqi_longer <- filter(longer_sced, grepl("\\.PSQI$", Key)) 



### Change Names for the Keys
psqi_longer <- psqi_longer %>%
  mutate(Key = case_when(Key == "Leon.PSQI" ~ "Person 1",
                         Key == "Max.PSQI" ~ "Person 2",
                         Key == "Paul.PSQI" ~ "Person 3",
                         Key == "Rica.PSQI" ~ "Person 4",
                         Key == "Sascha.PSQI" ~ "Person 5",
                         TRUE ~ Key))

# Define a data frame with the facet names and corresponding xintercept dates
vline_data <- data.frame(
  Key = unique(psqi_longer$Key),
  xintercept_date = as.Date(c("2023-12-08", "2023-12-05", "2023-12-07", "2023-12-06", "2023-12-09"))
)

psqi_longer %>%
  ggplot(aes(Datum, Value, col = Key)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x, 
              data = psqi_longer %>%
                inner_join(vline_data, by = "Key") %>%
                filter(Datum < xintercept_date)) +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x, 
              data = psqi_longer %>%
                inner_join(vline_data, by = "Key") %>%
                filter(Datum >= xintercept_date)) +
  facet_wrap(~Key, scales = "fixed") +
  ggtitle("PSQI-Scores") +
  labs(col = "Versuchsperson") +  # Legend title
  ylab("PSQI-Score") +  # Y-axis label
  scale_x_date(
    breaks = seq(from = as.Date("2023-12-01"), to = max(polar_longer$Datum), by = "2 days"),
    labels = scales::date_format("%d.%m.%Y")
  ) +
  theme_minimal() +  # Use a minimal theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",  # Position legend at the top
    legend.title = element_text(face = "bold"),  # Bold legend title
    legend.text = element_text(size = 9),  # Adjust legend text size
    plot.title = element_text(hjust = 0.5),  # Center plot title
    plot.caption = element_text(hjust = 0),  # Left-align plot caption
    strip.text = element_text(margin = margin(b = 10)),
    panel.spacing.x = unit(3, "cm") 
  ) +
  geom_vline(data = vline_data, aes(xintercept = xintercept_date), linetype = "dashed", color = "black", size = 1, alpha = .5)



cohen.d(polar_longer, )

# Summaries polar------------------------------------------------------------
# install.packages("effsize")
library(effsize)



# Group by 'Treatment_Baseline' and 'Key' and calculate summary statistics
summary_stats_polar <- polar_longer %>%
  group_by(Treatment_Baseline, Key) %>%
  summarise(
    Count = n(),
    Mean = mean(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    Min = min(Value, na.rm = TRUE),
    Max = max(Value, na.rm = TRUE),
    NAs = sum(is.na(Value)))


# Pivot the table wider
summary_stats_wide_polar <- summary_stats %>%
  pivot_wider(
    names_from = Treatment_Baseline,
    values_from = c(Count, Mean, SD, Min, Max, NAs),
    names_glue = "{Treatment_Baseline}_{.value}"
  )



# Summaries PQSI ----------------------------------------------------------

# Group by 'Treatment_Baseline' and 'Key' and calculate summary statistics
summary_stats_psqi <- psqi_longer %>%
  group_by(Treatment_Baseline, Key) %>%
  summarise(
    Count = n(),
    Mean = mean(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    Min = min(Value, na.rm = TRUE),
    Max = max(Value, na.rm = TRUE),
    NAs = sum(is.na(Value)))


# Pivot the table wider
summary_stats_wide_psqi <- summary_stats_psqi %>% 
  pivot_wider(
    names_from = Treatment_Baseline,
    values_from = c(Count, Mean, SD, Min, Max, NAs),
    names_glue = "{Treatment_Baseline}_{.value}")





### Graph mit Loess PSQI (eine einzige Loess)
psqi_longer %>%
  ggplot(aes(Datum, Value, col = Key)) +
  geom_point() +
  geom_line() +
  geom_smooth(se = F)+
  facet_wrap(~Key, scales = "fixed") +
  ggtitle("Scores des adaptierten PSQI-Fragebogens") +
  labs(col = "Versuchsperson") +  # Legend title
  ylab("PSQI-Score") +  # Y-axis label
  scale_x_date(
    breaks = seq(from = as.Date("2023-12-01"), to = max(polar_longer$Datum), by = "2 days"),
    labels = scales::date_format("%d.%m.%Y")
  ) +
  scale_y_continuous(limits = c(0,15) , breaks = seq(0, 15, by = 2))+
  theme_minimal() +  # Use a minimal theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",  # Position legend at the top
    legend.title = element_text(face = "bold"),  # Bold legend title
    legend.text = element_text(size = 9),  # Adjust legend text size
    plot.title = element_text(hjust = 0.5),  # Center plot title
    plot.caption = element_text(hjust = 0),  # Left-align plot caption
    strip.text = element_text(margin = margin(b = 10)),
    panel.spacing.x = unit(3, "cm") 
  ) +
  geom_vline(data = vline_data, aes(xintercept = xintercept_date), linetype = "dashed", color = "black", size = 1, alpha = .5)




# PSQI-Scores-Loess (two) -------------------------------------------------
psqi_longer %>%
  ggplot(aes(Datum, Value, col = Key)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x, 
              data = psqi_longer %>%
                inner_join(vline_data, by = "Key") %>%
                filter(Datum < xintercept_date)) +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x, 
              data = psqi_longer %>%
                inner_join(vline_data, by = "Key") %>%
                filter(Datum >= xintercept_date)) +
  facet_wrap(~Key, scales = "fixed") +
  ggtitle("Scores des adaptierten PSQI-Fragebogens") +
  labs(col = "Versuchsperson") +  # Legend title
  ylab("PSQI-Score") +  # Y-axis label
  scale_x_date(
    breaks = seq(from = as.Date("2023-12-01"), to = max(polar_longer$Datum), by = "2 days"),
    labels = scales::date_format("%d.%m.%Y")
  ) +
  scale_y_continuous(limits = c(0,15) , breaks = seq(0, 15, by = 2))+
  theme_minimal() +  # Use a minimal theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",  # Position legend at the top
    legend.title = element_text(face = "bold"),  # Bold legend title
    legend.text = element_text(size = 9),  # Adjust legend text size
    plot.title = element_text(hjust = 0.5),  # Center plot title
    plot.caption = element_text(hjust = 0),  # Left-align plot caption
    strip.text = element_text(margin = margin(b = 10)),
    panel.spacing.x = unit(3, "cm") 
  ) +
  geom_vline(data = vline_data, aes(xintercept = xintercept_date), linetype = "dashed", color = "black", size = 1, alpha = .5)






# Export ------------------------------------------------------------------
# write.xlsx(summary_stats_wide_psqi, "Summary_psqi.xlsx") 
# write.xlsx(summary_stats_wide_polar, "Summary_polar.xlsx")




# (Line-Plots (Prä und Post)) ---------------------------------------------

# psqi_subset <- summary_stats_wide_psqi[, c("Key", "Baseline_Mean", "Treatment_Mean", "Baseline_SD", "Treatment_SD")]
# psqi_prepost_long <- gather(psqi_subset, key = "Variable", value = "Value", -Key)
# 
# ggplot(psqi_prepost_long, aes(x = Variable, y = Value, group = Key, color = Key)) +
#   geom_line(aes(linetype = Key)) +
#   geom_point(aes(shape = Key)) +
#   geom_errorbar(
#     aes(ymin = Value - ifelse(Variable == "Baseline_Mean", Value, Value),
#         ymax = Value + ifelse(Variable == "Baseline_Mean", Value, Value)),
#     width = 0.2,
#     position = position_dodge(0.6)
#   ) +
#   labs(title = "Mean and SD Values for Baseline and Treatment",
#        x = "Condition",
#        y = "Value") +
#   theme_minimal()
# 
# 
# 
# psqi_subset <- summary_stats_wide_psqi[, c("Key", "Baseline_Mean", "Treatment_Mean", "Baseline_SD", "Treatment_SD")]
# psqi_prepost_long <- gather(psqi_subset, key = "Variable", value = "Value", -Key)
# 
# # Exclude "Baseline_SD" and "Treatment_SD" from the x-axis
# psqi_prepost_filtered <- psqi_prepost_long %>%
#   filter(Variable %in% c("Baseline_Mean", "Treatment_Mean"))

# # Plot using ggplot2
# library(ggplot2)
# 
# ggplot(psqi_prepost_filtered, aes(x = Variable, y = Value, group = Key, color = Key)) +
#   geom_line(aes(linetype = Key)) +
#   geom_point(aes(shape = Key)) +
#   geom_errorbar(
#     aes(ymin = Value - ifelse(Variable == "Baseline_Mean", Value, Value),
#         ymax = Value + ifelse(Variable == "Baseline_Mean", Value, Value)),
#     width = 0.2,
#     position = position_dodge2(0.6)  # Use position_dodge2 for connecting error bars to points
#   ) +
#   labs(title = "Mean and SD Values for Baseline and Treatment",
#        x = "Condition",
#        y = "Value") +
#   theme_minimal()






# Korrelation zwischen Werten des PSQI und Polar --------------------------
# Pearson Korrelation zwischen PSQI und Polar-Score


longer_sced
longer_sced_wide <- longer_sced %>%
  pivot_wider(names_from = Key,
              values_from = Value,
              values_fill = list(Value = NA)) %>%
  gather(Key, Value, -Datum, -Treatment_Baseline) %>%
  mutate(measure = ifelse(grepl("\\.polar$", Key,  ignore.case = TRUE), "Polar",
                          ifelse(grepl("\\.psqi$", Key,  ignore.case = TRUE), "PSQI", NA)))


# Pivot the data frame to a longer format
dft <- longer_sced_wide %>% pivot_wider(names_from = measure, values_from = Value)

dft <- dft %>%
  select(-Treatment_Baseline) %>%
  mutate(Key = gsub("\\..*$", "", Key))


dft <- pivot_wider(dft, id_cols = Key, values_from = c(PSQI,Polar))
# write.xlsx(dft, "corr.xlsx")
dft <- read.csv("corr.csv", sep = ";")


# Korrelation + Visualisierung -------------------------------------------------------------
library(stats)

cor.test(dft$PSQI, dft$Polar,  method = "pearson", use = "complete.obs") #--> r = -0.441 --> moderate negative Korrelation


ggplot(dft, aes(PSQI, Polar))+
  geom_point()+
  geom_jitter()+
  geom_smooth(method= "lm", se = F)+
  ggtitle("Zusammenhang von subjektiver Einschätzung der Schlafqualität und objektiver Messung")+
  theme_minimal() +  # Use a minimal theme
  theme(
    legend.position = "top",  # Position legend at the top
    legend.title = element_text(face = "bold"),  # Bold legend title
    legend.text = element_text(size = 9),  # Adjust legend text size
    plot.title = element_text(hjust = 0.5),  # Center plot title
    plot.caption = element_text(hjust = 0),  # Left-align plot caption
    strip.text = element_text(margin = margin(b = 10)),
    panel.spacing.x = unit(3, "cm") 
  )

                     