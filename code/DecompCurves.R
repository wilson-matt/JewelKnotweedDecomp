library(ggplot2)

curve_data<- read.csv("C:/GitHub/JewelKnotweedDecomp/data/AFDM_curves.csv")

curve_data$Date_numeric <- gsub("14-Oct-25", 7, curve_data$Date_numeric)
curve_data$Date_numeric <- gsub("21-Oct-25", 14, curve_data$Date_numeric)
curve_data$Date_numeric <- gsub("4-Nov-25", 28, curve_data$Date_numeric)
curve_data$Date_numeric <- gsub("18-Nov-25", 42, curve_data$Date_numeric)
curve_data$Date_numeric <- gsub("3-Dec-25", 57, curve_data$Date_numeric)
curve_data$Date_numeric <- as.numeric(curve_data$Date_numeric)

# K-calc ####
#Running corse knotweed as example:
library(ggpmisc)

fine_Jewel <- subset(curve_data, curve_data$Coarse_fine == "F" & curve_data$Species == "JW")
# Add day 0, add error bars from w2
fit <- nls(Prop_remaining ~ exp(-k * as.numeric(Date_numeric)), data = fine_Jewel, start = list(k = 0.01))
# 2. Extract k
k_val <- round(summary(fit)$coefficients[1, 1], 4)

# 3. Calculate Pseudo-R2
# Correlation between observed y and predicted y, squared
rss <- sum(residuals(fit)^2)
tss <- sum((fine_Jewel$Prop_remaining - mean(fine_Jewel$Prop_remaining))^2)
r_sq <- round(1 - (rss / tss), 3)

# 4. Create the equation string
# Using plotmath formatting for the superscript
eq_label <- paste0("W[i]/W[0] == e^{-", k_val, " * t}")
r_lab <- paste0("k == ", k_val, "~~R^2 == ", r_sq)

FJ <- ggplot(fine_Jewel, aes(x = Date_numeric, y = Prop_remaining)) +
  geom_point(size = 5, alpha = 0.6) +
  # Draw the curve using the calculated k
  stat_smooth(method = "nls", 
              formula = y ~ exp(-k * x), 
              method.args = list(start = list(k = 0.01)), 
              se = FALSE, color = "blue") +
  # Add the equation using parse = TRUE to render the LaTeX-style math
  annotate("text", x = max(fine_Jewel$Date_numeric)*0.8, y = 0.8, 
           label = eq_label, parse = TRUE, size = 8) +
  annotate("text", 
           x = max(fine_Jewel$Date_numeric) * 0.6, 
           y = 0.9, 
           label = r_lab, 
           parse = TRUE, 
           size = 8, 
           hjust = 0) +
  coord_cartesian(xlim = c(-.1, 60), ylim = c(-.01, 1.01)) +
  labs(x = "", 
       y = "Proportion of Mass Remaining",
       title = "A") +
#  title = "Fine Jewelweed") +
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=20),
        plot.title = element_text(size = 28, face = "bold"))

####Coarse Jewelweed####
coarse_Jewel <- subset(curve_data, curve_data$Coarse_fine == "C" & curve_data$Species == "JW")
# Add day 0, add error bars from w2
fit <- nls(Prop_remaining ~ exp(-k * as.numeric(Date_numeric)), data = coarse_Jewel, start = list(k = 0.01))
# 2. Extract k
k_val <- round(summary(fit)$coefficients[1, 1], 4)

# 3. Calculate Pseudo-R2
# Correlation between observed y and predicted y, squared
rss <- sum(residuals(fit)^2)
tss <- sum((coarse_Jewel$Prop_remaining - mean(coarse_Jewel$Prop_remaining))^2)
r_sq <- round(1 - (rss / tss), 3)

# 4. Create the equation string
# Using plotmath formatting for the superscript
eq_label <- paste0("W[i]/W[0] == e^{-", k_val, " * t}")
r_lab <- paste0("k == ", k_val, "~~R^2 == ", r_sq)

CJ <- ggplot(coarse_Jewel, aes(x = Date_numeric, y = Prop_remaining)) +
  geom_point(size = 5, alpha = 0.6) +
  # Draw the curve using the calculated k
  stat_smooth(method = "nls", 
              formula = y ~ exp(-k * x), 
              method.args = list(start = list(k = 0.01)), 
              se = FALSE, color = "blue") +
  # Add the equation using parse = TRUE to render the LaTeX-style math
  annotate("text", x = max(coarse_Jewel$Date_numeric)*0.8, y = 0.8, 
           label = eq_label, parse = TRUE, size = 8) +
  annotate("text", 
           x = max(coarse_Jewel$Date_numeric) * 0.6, 
           y = 0.9, 
           label = r_lab, 
           parse = TRUE, 
           size = 8, 
           hjust = 0) +
  coord_cartesian(xlim = c(-.1, 60), ylim = c(-.01, 1.01)) +
  labs(x = "", 
       y = "",
       title = "B") +
#  title = "Coarse Jewelweed") +
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=20),
        plot.title = element_text(size = 28, face = "bold"))


####Fine Knotweed####
fine_Knot <- subset(curve_data, curve_data$Coarse_fine == "F" & curve_data$Species == "KW")
# Add day 0, add error bars from w2
fit <- nls(Prop_remaining ~ exp(-k * as.numeric(Date_numeric)), data = fine_Knot, start = list(k = 0.01))
# 2. Extract k
k_val <- round(summary(fit)$coefficients[1, 1], 4)

# 3. Calculate Pseudo-R2
# Correlation between observed y and predicted y, squared
rss <- sum(residuals(fit)^2)
tss <- sum((fine_Knot$Prop_remaining - mean(fine_Knot$Prop_remaining))^2)
r_sq <- round(1 - (rss / tss), 3)

# 4. Create the equation string
# Using plotmath formatting for the superscript
eq_label <- paste0("W[i]/W[0] == e^{-", k_val, " * t}")
r_lab <- paste0("k == ", k_val, "~~R^2 == ", r_sq)

FK <- ggplot(fine_Knot, aes(x = Date_numeric, y = Prop_remaining)) +
  geom_point(size = 5, alpha = 0.6) +
  # Draw the curve using the calculated k
  stat_smooth(method = "nls", 
              formula = y ~ exp(-k * x), 
              method.args = list(start = list(k = 0.01)), 
              se = FALSE, color = "blue") +
  # Add the equation using parse = TRUE to render the LaTeX-style math
  annotate("text", x = max(fine_Knot$Date_numeric)*0.8, y = 0.8, 
           label = eq_label, parse = TRUE, size = 8) +
  annotate("text", 
           x = max(fine_Knot$Date_numeric) * 0.6, 
           y = 0.9, 
           label = r_lab, 
           parse = TRUE, 
           size = 8, 
           hjust = 0) +
  coord_cartesian(xlim = c(-.1, 60), ylim = c(-.01, 1.01)) +
  labs(x = "Days in Stream", 
       y = "Proportion of Mass Remaining",
       title = "C") +
#       title = "Fine Knotweed") +
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=20),
        plot.title = element_text(size = 28, face = "bold"))


####Coarse Knotweed####
coarse_Knot <- subset(curve_data, curve_data$Coarse_fine == "C" & curve_data$Species == "KW")
# Add day 0, add error bars from w2
fit <- nls(Prop_remaining ~ exp(-k * as.numeric(Date_numeric)), data = coarse_Knot, start = list(k = 0.01))
# 2. Extract k
k_val <- round(summary(fit)$coefficients[1, 1], 4)

# 3. Calculate Pseudo-R2
# Correlation between observed y and predicted y, squared
rss <- sum(residuals(fit)^2)
tss <- sum((coarse_Knot$Prop_remaining - mean(coarse_Knot$Prop_remaining))^2)
r_sq <- round(1 - (rss / tss), 3)

# 4. Create the equation string
# Using plotmath formatting for the superscript
eq_label <- paste0("W[i]/W[0] ==  e^{-", k_val, " * t}")
r_lab <- paste0("k == ", k_val, "~~R^2 == ", r_sq)

CK <- ggplot(coarse_Knot, aes(x = Date_numeric, y = Prop_remaining)) +
  geom_point(size = 5, alpha = 0.6) +
  # Draw the curve using the calculated k
  stat_smooth(method = "nls", 
              formula = y ~ exp(-k * x), 
              method.args = list(start = list(k = 0.01)), 
              se = FALSE, color = "blue") +
  # Add the equation using parse = TRUE to render the LaTeX-style math
  annotate("text", x = max(coarse_Knot$Date_numeric)*0.8, y = 0.8, 
           label = eq_label, parse = TRUE, size = 8) +
  annotate("text", 
           x = max(coarse_Knot$Date_numeric) * 0.6, 
           y = 0.9, 
           label = r_lab, 
           parse = TRUE, 
           size = 8, 
           hjust = 0) +
  coord_cartesian(xlim = c(-.1, 60), ylim = c(-.01, 1.01)) +
  labs(x = "Days in Stream", 
       y = "",
       title = "D") +
#  title = "Coarse Knotweed") +
theme_classic()+
theme(axis.text=element_text(size=15),
      axis.title=element_text(size=20),
      plot.title = element_text(size = 28, face = "bold"))
  
  
#For final fig:
library(ggpubr)


jpeg(filename = "C:/GitHub/JewelKnotDecomp/figures/K_multipanel_fixed.jpg",
     width = 1200, height = 1200, units = "px", quality = 500)

ggarrange(FJ, CJ,FK,CK, ncol = 2, nrow = 2)

dev.off()



