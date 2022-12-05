rm(list=ls())
process <- as.integer(as.character(commandArgs(trailingOnly = TRUE)))
setwd("/Users/falco/OneDrive - Harvard University/Research/CRE/Output")

library(ggplot2)
library(gridExtra)
library(gapminder)
library(dplyr)
library(readr)

options(digits=5)

mse_long <- read_csv("experiments/output/mse_long_appendix.csv")
bias_long <- read_csv("experiments/output/bias_long_appendix.csv")

margin_spacer <- function(x) {
  # where x is the column in your dataset
  left_length <- nchar(levels(factor(x)))[1]
  if (left_length > 8) {
    return((left_length - 8) * 4)
  }
  else
    return(0)
}

pdf("app_mse.pdf")
mse_plot <- ggplot(mse_long, aes(x = method, y = mse, fill = method)) +
  facet_wrap(~size) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(name = "Mean Squared Error (MSE)") +
  scale_x_discrete(name = "Method") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 8))
mse_plot
dev.off()

pdf("app_bias.pdf")
bias_plot <- ggplot(bias_long, aes(x = method, y = bias, fill = method)) +
  facet_wrap(~size) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(name = "Bias", limits=c(-0.3, 0.3)) +
  scale_x_discrete(name = "Method") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 8))  +
  geom_hline(aes(yintercept = 0))
bias_plot
dev.off()
