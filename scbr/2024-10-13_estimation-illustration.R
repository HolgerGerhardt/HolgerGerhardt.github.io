# This file illustrates the most important parts of the estimation of
# preference parameters performed by Holger Gerhardt & Rafael Suchy (2024),
# “Estimating Preference Parameters from Strictly Concave Budget Restrictions,”
# https://www.econtribute.de/RePEc/ajk/ajkdps/ECONtribute_336_2024.pdf.

# Version: 2024-10-13
# The results reported in this file were produced using R version 4.4.1
# (https://cloud.r-project.org/bin/macosx/big-sur-arm64/base/R-4.4.1-arm64.pkg)
# and the up-to-date versions of the packages mentioned below




# PREAMBLE ------------------------------------------------------------------------------------


# Clean up environment
rm(list = ls())

options(max.print = 9999)  # Increase limit for omitting entries in output
options(scipen = 999)  # Increase limit of using scientific notation
round_prec <- 4  # Number of decimal places for reporting the parameter estimates

# Required packages
packages_required <- c(
  "AER",  # For "tobit() function"
  "cli",  # For colored error and warning messages
  "msm",  # For "deltamethod() function"
  "maxLik"  # For maximum likelihood estimation
)
# Install packages that are not installed yet
install.packages(setdiff(packages_required, rownames(installed.packages())))
# For updating already installed packages, use the following
# install.packages(packages_required)
# Load the required packages
for (name in packages_required) {
  library(name, character.only = TRUE)
}

# Alternatively, use groundhog:
# install.packages("groundhog")
# library("groundhog")
# groundhog.library(packages_required, "2024-10-10")




# BUDGET RESTRICTIONS -------------------------------------------------------------------------


# General functional form, based on Equation (9) from
# https://www.econtribute.de/RePEc/ajk/ajkdps/ECONtribute_336_2024.pdf:
# c_{t}^{1 + z} + (1 / R)^{1 + z} c_{t + k}^{1 + z} = m^{1 + z}.
# Thus, linear budget restrictions (LBRs) for z = 0, and
# strictly concave budget restrictions (SCBRs) for z > 0.
c_2 <- function(c_1, m, R, z, b_1, b_2) {
  R * (m^(1 + z) - c_1^(1 + z))^(1 / (1 + z))
}

# Experimental parameters
# (see Table E.1 in https://www.econtribute.de/RePEc/ajk/ajkdps/ECONtribute_336_2024.pdf)
# The presentation of the different budget restrictions in the experiment by Gerhardt & Suchy
# was randomized as described in the manuscript. We abstract from this randomization here.
budget_restritions <- matrix(c(
  c(01, 1.50, 1.50, 0.0, 1, 05, 020.00, 0.70000, 1.42857, 1, 1, 0.01, 02.5, 025),
  c(02, 1.50, 1.50, 0.0, 1, 05, 017.50, 0.80000, 1.25000, 1, 1, 0.01, 02.5, 025),
  c(03, 1.50, 1.50, 0.0, 1, 05, 015.56, 0.90000, 1.11111, 1, 1, 0.01, 02.5, 025),
  c(04, 1.50, 1.50, 0.0, 1, 05, 014.70, 0.95238, 1.05000, 1, 1, 0.01, 02.5, 025),
  c(05, 1.50, 1.50, 0.0, 1, 05, 014.00, 1.00000, 1.00000, 1, 1, 0.01, 02.5, 025),
  c(06, 1.50, 1.50, 0.0, 1, 05, 014.00, 1.05000, 0.95238, 1, 1, 0.01, 02.5, 025),
  c(07, 1.50, 1.50, 0.0, 1, 05, 014.00, 1.11111, 0.90000, 1, 1, 0.01, 02.5, 025),
  c(08, 1.50, 1.50, 0.0, 1, 05, 014.00, 1.25000, 0.80000, 1, 1, 0.01, 02.5, 025),
  c(09, 1.50, 1.50, 0.0, 1, 05, 014.00, 1.42857, 0.70000, 1, 1, 0.01, 02.5, 025),
  c(10, 1.50, 1.50, 0.0, 1, 05, 025.00, 0.80000, 1.25000, 1, 1, 0.01, 02.5, 025),
  c(11, 1.50, 1.50, 0.0, 1, 05, 021.00, 0.95238, 1.05000, 1, 1, 0.01, 02.5, 025),
  c(12, 1.50, 1.50, 0.0, 1, 05, 020.00, 1.00000, 1.00000, 1, 1, 0.01, 02.5, 025),
  c(13, 1.50, 1.50, 0.0, 1, 05, 020.00, 1.05000, 0.95238, 1, 1, 0.01, 02.5, 025),
  c(14, 1.50, 1.50, 0.0, 1, 05, 020.00, 1.25000, 0.80000, 1, 1, 0.01, 02.5, 025),
  c(15, 1.50, 1.50, 0.0, 1, 10, 020.00, 0.70000, 1.42857, 1, 1, 0.01, 02.5, 025),
  c(16, 1.50, 1.50, 0.0, 1, 10, 017.50, 0.80000, 1.25000, 1, 1, 0.01, 02.5, 025),
  c(17, 1.50, 1.50, 0.0, 1, 10, 015.56, 0.90000, 1.11111, 1, 1, 0.01, 02.5, 025),
  c(18, 1.50, 1.50, 0.0, 1, 10, 014.70, 0.95238, 1.05000, 1, 1, 0.01, 02.5, 025),
  c(19, 1.50, 1.50, 0.0, 1, 10, 014.00, 1.00000, 1.00000, 1, 1, 0.01, 02.5, 025),
  c(20, 1.50, 1.50, 0.0, 1, 10, 014.00, 1.05000, 0.95238, 1, 1, 0.01, 02.5, 025),
  c(21, 1.50, 1.50, 0.0, 1, 10, 014.00, 1.11111, 0.90000, 1, 1, 0.01, 02.5, 025),
  c(22, 1.50, 1.50, 0.0, 1, 10, 014.00, 1.25000, 0.80000, 1, 1, 0.01, 02.5, 025),
  c(23, 1.50, 1.50, 0.0, 1, 10, 014.00, 1.42857, 0.70000, 1, 1, 0.01, 02.5, 025),
  c(24, 1.50, 1.50, 0.0, 1, 10, 025.00, 0.80000, 1.25000, 1, 1, 0.01, 02.5, 025),
  c(25, 1.50, 1.50, 0.0, 1, 10, 021.00, 0.95238, 1.05000, 1, 1, 0.01, 02.5, 025),
  c(26, 1.50, 1.50, 0.0, 1, 10, 020.00, 1.00000, 1.00000, 1, 1, 0.01, 02.5, 025),
  c(27, 1.50, 1.50, 0.0, 1, 10, 020.00, 1.05000, 0.95238, 1, 1, 0.01, 02.5, 025),
  c(28, 1.50, 1.50, 0.0, 1, 10, 020.00, 1.25000, 0.80000, 1, 1, 0.01, 02.5, 025),
  c(29, 1.50, 1.50, 0.0, 0, 05, 014.70, 0.95238, 1.05000, 1, 1, 0.01, 02.5, 025),
  c(30, 1.50, 1.50, 0.0, 0, 05, 014.00, 1.05000, 0.95238, 1, 1, 0.01, 02.5, 025),
  c(31, 1.50, 1.50, 0.0, 0, 05, 021.00, 0.95238, 1.05000, 1, 1, 0.01, 02.5, 025),
  c(32, 1.50, 1.50, 0.0, 0, 05, 020.00, 1.05000, 0.95238, 1, 1, 0.01, 02.5, 025),
  c(33, 1.50, 1.50, 0.0, 0, 10, 014.70, 0.95238, 1.05000, 1, 1, 0.01, 02.5, 025),
  c(34, 1.50, 1.50, 0.0, 0, 10, 014.00, 1.05000, 0.95238, 1, 1, 0.01, 02.5, 025),
  c(35, 1.50, 1.50, 0.0, 0, 10, 021.00, 0.95238, 1.05000, 1, 1, 0.01, 02.5, 025),
  c(36, 1.50, 1.50, 0.0, 0, 10, 020.00, 1.05000, 0.95238, 1, 1, 0.01, 02.5, 025),
  c(37, 1.50, 1.50, 0.4, 1, 05, 020.00, 0.70000, 1.42857, 1, 2, 0.01, 02.5, 025),
  c(38, 1.50, 1.50, 0.4, 1, 05, 017.50, 0.80000, 1.25000, 1, 2, 0.01, 02.5, 025),
  c(39, 1.50, 1.50, 0.4, 1, 05, 015.56, 0.90000, 1.11111, 1, 2, 0.01, 02.5, 025),
  c(40, 1.50, 1.50, 0.4, 1, 05, 014.70, 0.95238, 1.05000, 1, 2, 0.01, 02.5, 025),
  c(41, 1.50, 1.50, 0.4, 1, 05, 014.00, 1.00000, 1.00000, 1, 2, 0.01, 02.5, 025),
  c(42, 1.50, 1.50, 0.4, 1, 05, 014.00, 1.05000, 0.95238, 1, 2, 0.01, 02.5, 025),
  c(43, 1.50, 1.50, 0.4, 1, 05, 014.00, 1.11111, 0.90000, 1, 2, 0.01, 02.5, 025),
  c(44, 1.50, 1.50, 0.4, 1, 05, 014.00, 1.25000, 0.80000, 1, 2, 0.01, 02.5, 025),
  c(45, 1.50, 1.50, 0.4, 1, 05, 014.00, 1.42857, 0.70000, 1, 2, 0.01, 02.5, 025),
  c(46, 1.50, 1.50, 0.4, 1, 05, 025.00, 0.80000, 1.25000, 1, 2, 0.01, 02.5, 025),
  c(47, 1.50, 1.50, 0.4, 1, 05, 021.00, 0.95238, 1.05000, 1, 2, 0.01, 02.5, 025),
  c(48, 1.50, 1.50, 0.4, 1, 05, 020.00, 1.00000, 1.00000, 1, 2, 0.01, 02.5, 025),
  c(49, 1.50, 1.50, 0.4, 1, 05, 020.00, 1.05000, 0.95238, 1, 2, 0.01, 02.5, 025),
  c(50, 1.50, 1.50, 0.4, 1, 05, 020.00, 1.25000, 0.80000, 1, 2, 0.01, 02.5, 025),
  c(51, 1.50, 1.50, 0.4, 1, 10, 020.00, 0.70000, 1.42857, 1, 2, 0.01, 02.5, 025),
  c(52, 1.50, 1.50, 0.4, 1, 10, 017.50, 0.80000, 1.25000, 1, 2, 0.01, 02.5, 025),
  c(53, 1.50, 1.50, 0.4, 1, 10, 015.56, 0.90000, 1.11111, 1, 2, 0.01, 02.5, 025),
  c(54, 1.50, 1.50, 0.4, 1, 10, 014.70, 0.95238, 1.05000, 1, 2, 0.01, 02.5, 025),
  c(55, 1.50, 1.50, 0.4, 1, 10, 014.00, 1.00000, 1.00000, 1, 2, 0.01, 02.5, 025),
  c(56, 1.50, 1.50, 0.4, 1, 10, 014.00, 1.05000, 0.95238, 1, 2, 0.01, 02.5, 025),
  c(57, 1.50, 1.50, 0.4, 1, 10, 014.00, 1.11111, 0.90000, 1, 2, 0.01, 02.5, 025),
  c(58, 1.50, 1.50, 0.4, 1, 10, 014.00, 1.25000, 0.80000, 1, 2, 0.01, 02.5, 025),
  c(59, 1.50, 1.50, 0.4, 1, 10, 014.00, 1.42857, 0.70000, 1, 2, 0.01, 02.5, 025),
  c(60, 1.50, 1.50, 0.4, 1, 10, 025.00, 0.80000, 1.25000, 1, 2, 0.01, 02.5, 025),
  c(61, 1.50, 1.50, 0.4, 1, 10, 021.00, 0.95238, 1.05000, 1, 2, 0.01, 02.5, 025),
  c(62, 1.50, 1.50, 0.4, 1, 10, 020.00, 1.00000, 1.00000, 1, 2, 0.01, 02.5, 025),
  c(63, 1.50, 1.50, 0.4, 1, 10, 020.00, 1.05000, 0.95238, 1, 2, 0.01, 02.5, 025),
  c(64, 1.50, 1.50, 0.4, 1, 10, 020.00, 1.25000, 0.80000, 1, 2, 0.01, 02.5, 025),
  c(65, 1.50, 1.50, 0.4, 0, 05, 014.70, 0.95238, 1.05000, 1, 2, 0.01, 02.5, 025),
  c(66, 1.50, 1.50, 0.4, 0, 05, 014.00, 1.05000, 0.95238, 1, 2, 0.01, 02.5, 025),
  c(67, 1.50, 1.50, 0.4, 0, 05, 021.00, 0.95238, 1.05000, 1, 2, 0.01, 02.5, 025),
  c(68, 1.50, 1.50, 0.4, 0, 05, 020.00, 1.05000, 0.95238, 1, 2, 0.01, 02.5, 025),
  c(69, 1.50, 1.50, 0.4, 0, 10, 014.70, 0.95238, 1.05000, 1, 2, 0.01, 02.5, 025),
  c(70, 1.50, 1.50, 0.4, 0, 10, 014.00, 1.05000, 0.95238, 1, 2, 0.01, 02.5, 025),
  c(71, 1.50, 1.50, 0.4, 0, 10, 021.00, 0.95238, 1.05000, 1, 2, 0.01, 02.5, 025),
  c(72, 1.50, 1.50, 0.4, 0, 10, 020.00, 1.05000, 0.95238, 1, 2, 0.01, 02.5, 025),
  c(73, 1.50, 1.50, 0.0, 1, 10, 150.00, 0.80000, 1.25000, 1, 3, 0.10, 10.0, 160),
  c(74, 1.50, 1.50, 0.0, 1, 10, 126.00, 0.95238, 1.05000, 1, 3, 0.10, 10.0, 160),
  c(75, 1.50, 1.50, 0.0, 1, 10, 120.00, 1.00000, 1.00000, 1, 3, 0.10, 10.0, 160),
  c(76, 1.50, 1.50, 0.0, 1, 10, 120.00, 1.05000, 0.95238, 1, 3, 0.10, 10.0, 160),
  c(77, 1.50, 1.50, 0.0, 1, 10, 120.00, 1.25000, 0.80000, 1, 3, 0.10, 10.0, 160),
  c(78, 1.50, 1.50, 0.4, 1, 10, 150.00, 0.80000, 1.25000, 1, 4, 0.10, 10.0, 160),
  c(79, 1.50, 1.50, 0.4, 1, 10, 126.00, 0.95238, 1.05000, 1, 4, 0.10, 10.0, 160),
  c(80, 1.50, 1.50, 0.4, 1, 10, 120.00, 1.00000, 1.00000, 1, 4, 0.10, 10.0, 160),
  c(81, 1.50, 1.50, 0.4, 1, 10, 120.00, 1.05000, 0.95238, 1, 4, 0.10, 10.0, 160),
  c(82, 1.50, 1.50, 0.4, 1, 10, 120.00, 1.25000, 0.80000, 1, 4, 0.10, 10.0, 160)
), nrow = 82, byrow = TRUE)
colnames(budget_restritions) <- c(
  "DecNum", "BasePayOne", "BasePayTwo", "Curvature", "FED", "Delay", "Budget",
  "PriceOneDivPriceTwo", "PriceTwoDivPriceOne", "ProbOne", "Block", "StepSize",
  "TickDist", "PlotMax"
)

# Generate data frame so that we can include all possible allocations that could be selected
# from the different budget restrictions
budget_restritions_df <- as.data.frame(budget_restritions)
# Add necessary columns, initialized with NA
budget_restritions_df$c_1_series <- list(NA)
budget_restritions_df$c_2_series <- list(NA)
budget_restritions_df$C_series <- list(NA)
budget_restritions_df$lb <- NA
budget_restritions_df$ub <- NA

# In the actual experiment, the sooner payment was displayed on the vertical axis, and the later
# payment was displayed on the horizontal axis. For convenience, we display c_1 on the horizontal
# and c_2 on the vertical axis here.

# Populate data frame
for (i in 1:dim(budget_restritions_df)[1]) {
  c_1_series <- seq(
    0, budget_restritions_df$Budget[i], budget_restritions_df$StepSize[i]
  )
  c_2_series <- round(c_2(
    c_1_series,
    budget_restritions_df$Budget[i],
    budget_restritions_df$PriceOneDivPriceTwo[i],
    budget_restritions_df$Curvature[i],
    budget_restritions_df$BasePayOne[i],
    budget_restritions_df$BasePayTwo[i]
  ), 2)  # Rounding to 2 decimal places because we displayed amounts with a precision of €0.01
  # Keep only c_1-c_2 pairs for c_2 >= baseline payment
  c_1_series <- c_1_series[c_2_series >= budget_restritions_df$BasePayTwo[i]]
  c_2_series <- c_2_series[1:length(c_1_series)]
  # Keep only c_1-c_2 pairs for c_1 >= baseline payment
  c_2_series <- c_2_series[c_1_series >= budget_restritions_df$BasePayOne[i]]
  c_1_series <- c_1_series[c_1_series >= budget_restritions_df$BasePayOne[i]]
  # Add horizontal/vertical segment at the position of the baseline payments
  c_1_series <- c(0, c_1_series, max(c_1_series))
  c_2_series <- c(max(c_2_series), c_2_series, 0)
  budget_restritions_df[[i, "c_1_series"]] <- as.list(c_1_series)
  budget_restritions_df[[i, "c_2_series"]] <- as.list(c_2_series)
  budget_restritions_df[[i, "C_series"]] <- as.list(c_1_series / c_2_series)
  budget_restritions_df[[i, "lb"]] <- budget_restritions_df[[i, "BasePayTwo"]] / max(c_2_series)
  budget_restritions_df[[i, "ub"]] <- max(c_1_series) / budget_restritions_df[[i, "BasePayTwo"]]
}
rm(c_1_series, c_2_series, i)




# PLOT BUDGET RESTRICTIONS --------------------------------------------------------------------


plotBRs <- function(df, curv) {
  df_aux <- df[df$Curvature == curv, ]
  plot_max <- max(df_aux[, "PlotMax"])
  plot(
    NaN, NaN,
    xlim = c(0, plot_max),
    ylim = c(0, plot_max),
    axes = FALSE,
    xlab = "", ylab = "",
    asp = 1,
    main = bquote("Budget restricions with curvature" ~ italic(z) ~ "=" ~ .(curv))
  )
  axis(1, seq(0, plot_max, 20), col = "gray25", col.axis = "gray25", pos = 0)
  text(x = plot_max / 2, y = -22.5, bquote(italic(c)[italic(t)]), xpd = TRUE)
  axis(2, seq(0, plot_max, 20), col = "gray25", col.axis = "gray25", pos = 0)
  text(x = -22.5, y = plot_max / 2, bquote(italic(c)[italic(t) + italic(k)]), xpd = TRUE, srt = 90)
  for (i in 1:dim(df_aux)[1]) {
    lines( unlist(df_aux[[i, "c_1_series"]]), unlist(df_aux[[i, "c_2_series"]]), col = "navy")
  }
}

for (curv in unique(budget_restritions_df$Curvature)) {
  plotBRs(budget_restritions_df, curv)
}
rm(curv)




# SIMULATE CHOICES ----------------------------------------------------------------------------


# For replicability of the results, use particular seed for the pseudorandom draws
set.seed(42)
# For new draws, remove the fixed seed via the following line
# set.seed(NULL)

# Optimal payment ratio:
# Equation (25) in https://www.econtribute.de/RePEc/ajk/ajkdps/ECONtribute_336_2024.pdf
C_star <- function(t, k, R, z, beta, delta, rho) {
  (1 / (beta^(t == 0) * delta^k * R^(1 + z)))^(1 / (rho + z))
}

# Add noise with Gaussian distribution:
# additive normally distributed noise on log(C_star) => multiplicative log-normal noise on C_star
C_star_noisy <- function(t, k, R, z, beta, delta, rho, sigma) {
  C_star(t, k, R, z, beta, delta, rho) * exp(rnorm(length(t), mean = 0, sd = sigma))
}

# Determine optimal (noisy) points on all BRs
opt_noisy_points_on_BR <- function(df_ind, beta, delta, rho, sigma) {
  ones <- rep(1, dim(df_ind)[1])
  beta_vec <- beta * ones
  delta_vec <- delta * ones
  rho_vec <- rho * ones
  sigma_vec <- sigma * ones
  # If rho is so small (negative) that the curvature of the utility function exceeds the curvature
  # of the budget restriction, replace rho by a value that is ever so slightly larger than
  # the curvature of the budget restriction. This way, the condition for an interior allocation
  # can still be applied, ideally leading to finite values (instead of NaNs), which can then be
  # converted to corner allocations.
  rho_vec[rho_vec <= -df_ind$Curvature] <-
    -df_ind$Curvature[rho_vec <= -df_ind$Curvature] + 0.000001
  C_star_noisy_list <- C_star_noisy(
    df_ind$FED,
    df_ind$Delay,
    df_ind$PriceOneDivPriceTwo,
    df_ind$Curvature,
    beta_vec, delta_vec, rho_vec, sigma_vec
  )
  # Initialize vectors with NA
  opt_noisy_C <- opt_noisy_C_idx <- opt_noisy_c_1 <- opt_noisy_c_2 <-
    rep(NA, length(C_star_noisy_list))
  # Populate vectors by iterating of the budget restrictions
  for (i in 1:length(C_star_noisy_list)) {
    # Find the point among the discrete points on the current budget restriction that is
    # closest to the (continuous) theoretical prediction
    dist <- abs(C_star_noisy_list[i] - unlist(df_ind[i, "C_series"]))
    # Use this allocation as the simulated choice
    opt_noisy_C_idx[i] <- which.min(dist)
    opt_noisy_C[i] <- unlist(df_ind[i, "C_series"])[opt_noisy_C_idx[i]]
    opt_noisy_c_1[i] <- unlist(df_ind[i, "c_1_series"])[opt_noisy_C_idx[i]]
    opt_noisy_c_2[i] <- unlist(df_ind[i, "c_2_series"])[opt_noisy_C_idx[i]]
    # Convert predicted allocation beyond the baseline payments to corner allocation
    max_C <- unlist(df_ind[i, "C_series"])[length(unlist(df_ind[i, "C_series"])) - 1]
    min_C <- unlist(df_ind[i, "C_series"])[2]
    if (opt_noisy_C[i] > max_C || is.nan(min(dist))) {
      opt_noisy_C[i] <- df_ind[[i, "ub"]]
      opt_noisy_c_1[i] <- max(unlist(df_ind[[i, "c_1_series"]]))
      opt_noisy_c_2[i] <- df_ind[[i, "BasePayTwo"]]
    }
    if (opt_noisy_C[i] < min_C) {
      opt_noisy_C[i] <- df_ind[[i, "lb"]]
      opt_noisy_c_1[i] <- df_ind[[i, "BasePayOne"]]
      opt_noisy_c_2[i] <- max(unlist(df_ind[[i, "c_2_series"]]))
    }
  }
  rm(dist)
  return(c(
    "C" = list(opt_noisy_C),
    "c_1" = list(opt_noisy_c_1),
    "c_2" = list(opt_noisy_c_2)
  ))
}

# Simulate participants with the following preference and noise parameters
# id, beta, delta, rho, sigma
params_sim <- matrix(c(
  1, 1.00, 1.00, 0.000, 0.00,  # Will be hard to estimate with z = 0 but easy with z = 0.4
  2, 1.00, 0.99, 0.000, 0.25,  # Will be hard to estimate with z = 0 but easy with z = 0.4
  3, 0.80, 0.99, 0.000, 0.25,  # Will be hard to estimate with z = 0 but easy with z = 0.4
  4, 0.80, 0.99, 0.100, 0.50,  # Should be estimable with both z = 0 and z = 0.4
  5, 0.85, 0.95, 0.150, 0.50,  # Should be estimable with both z = 0 and z = 0.4
  6, 1.00, 1.00, 1.000, 0.00,  # Should be estimable with both z = 0 and z = 0.4
  7, 1.00, 0.99, 100.0, 0.25,  # Should be estimable with both z = 0 and z = 0.4
  8, 1.00, 1.00, 200.0, 0.05,  # Should be estimable with both z = 0 and z = 0.4
  9, 1.00, 1.00, 200.0, 0.00   # May fail to converge (rho -> Inf), since c_1 = c_2 for all choices
), ncol = 5, byrow = TRUE)
colnames(params_sim) <- c("id", "beta", "delta", "rho", "sigma")

ids <- unique(params_sim[, "id"])

# Create new empty data frame
df <- budget_restritions_df[FALSE, ]

# Populate the data frame with the (noisy) choices of the simulated participants and
# plot the simulated choices
for (id in ids) {
  df_ind <- budget_restritions_df
  # Add column with IDs
  df_ind$id <- id
  # Store simulated choices
  points_sim <- opt_noisy_points_on_BR(
    df_ind,
    params_sim[params_sim[, "id"] == id, "beta"],
    params_sim[params_sim[, "id"] == id, "delta"],
    params_sim[params_sim[, "id"] == id, "rho"],
    params_sim[params_sim[, "id"] == id, "sigma"]
  )
  df_ind$payment_1 <- unlist(points_sim["c_1"])
  df_ind$payment_2 <- unlist(points_sim["c_2"])
  df_ind$payment_ratio <- unlist(points_sim["C"])
  # Append to the existing data frame
  df <- rbind(df, df_ind)
  # Generate a plot per level of curvature of the budget restrictions
  for (curv in unique(df_ind$Curvature)) {
    plotBRs(df_ind, curv)
    points(
      x = unlist(points_sim["c_1"])[df_ind$Curvature == curv],
      y = unlist(points_sim["c_2"])[df_ind$Curvature == curv],
      col = "navy",
      pch = 20,
    )
    mtext(bquote(
      "Simulated choices for ID" ~ .(id) * ":" ~
        italic(β) ~ "=" ~ .(params_sim[params_sim[, "id"] == id, "beta"]) * "," ~
        italic(δ) ~ "=" ~ .(params_sim[params_sim[, "id"] == id, "delta"]) * "," ~
        italic(ρ) ~ "=" ~ .(params_sim[params_sim[, "id"] == id, "rho"]) * "," ~
        italic(σ) ~ "=" ~ .(params_sim[params_sim[, "id"] == id, "sigma"])
    ), side = 3, col = "navy", line = 0.25)
    Sys.sleep(0.5)
  }
}
rm(curv, df_ind, id)
rm(points_sim)




# TOBIT ESTIMATION ----------------------------------------------------------------------------


# Generating the explanatory variables, see
# https://www.econtribute.de/RePEc/ajk/ajkdps/ECONtribute_336_2024.pdf, eq. (26) on p. 15:
df$cov_1 <-
  -as.integer(df$FED == 0)  # -I[t = 0], coefficient: gamma_beta
df$cov_2 <-
  -df$Delay  # -k, coefficient: gamma_delta
df$cov_3 <-
  -(1 + df$Curvature) * log(df$PriceOneDivPriceTwo)  # -(1 + z) ln(R), coefficient: gamma_rho

ids <- unique(df$id)
tobit_estimates_report <- list()

# Estimate separately for each curvature level of the budget restrictions
for (curv in unique(df$Curvature)) {
  # Initialize collection of estimates with NA
  tobit_estimates <- matrix(rep(NA, length(ids) * 5), ncol = 5)
  tobit_estimates <- cbind(ids, tobit_estimates)
  colnames(tobit_estimates) <- c("id", "beta", "delta", "rho", "sigma", "logL")
  # Estimate each individual separately
  for (id in ids) {
    print(paste0("Tobit estimation, Subject ID: ", id, "; BR curvature: ", curv))
    df_ind <- df[df$id == id & df$Curvature == curv, ]
    # Plot simulated choices
    plotBRs(df_ind, curv)
    points(
      df_ind$payment_1,
      df_ind$payment_2,
      col = "navy",
      pch = 20
    )
    mtext(bquote(
      "Simulated choices for ID" ~ .(id) * ":" ~
        italic(β) ~ "=" ~ .(params_sim[params_sim[, "id"] == id, "beta"]) * "," ~
        italic(δ) ~ "=" ~ .(params_sim[params_sim[, "id"] == id, "delta"]) * "," ~
        italic(ρ) ~ "=" ~ .(params_sim[params_sim[, "id"] == id, "rho"]) * "," ~
        italic(σ) ~ "=" ~ .(params_sim[params_sim[, "id"] == id, "sigma"])
    ), side = 3, col = "navy", line = 0.25)
    tryCatch(
      {
        withCallingHandlers(
          # Attempt Tobit regression
          {
            model <- tobit(
              log(payment_ratio) ~ cov_1 + cov_2 + cov_3 - 1,
              # "+" here because we included the minus sign above when creating the regressors
              left = log(df_ind$lb),
              right = log(df_ind$ub),
              data = df_ind
            )
          },
          # Show potential warning messages of the Tobit regression
          warning = function(w) {
            message(style_bold(col_blue("WARNING: ", conditionMessage(w))))
            invokeRestart("muffleWarning")
          }
        )
        vcov_matrix <- vcov(model)
        coeffs <- model$coefficients
        est_beta <-
          round(exp(as.numeric(coeffs[["cov_1"]]) / as.numeric(coeffs[["cov_3"]])), round_prec)
          # beta = exp(gamma_beta / gamma_rho), eq. (27) in Gerhardt & Suchy (2024)
        est_delta <-
          round(exp(as.numeric(coeffs[["cov_2"]]) / as.numeric(coeffs[["cov_3"]])), round_prec)
          # delta = exp(gamma_delta / gamma_rho), eq. (28)
        est_rho <-
          round(1 / as.numeric(coeffs[["cov_3"]]) - curv, round_prec)
          # rho = (1 / gamma_rho) - z, eq. (29)
        est_sigma <- round(model$scale, round_prec)
        tobit_estimates[tobit_estimates[, "id"] == id, "beta"] <- est_beta
        tobit_estimates[tobit_estimates[, "id"] == id, "delta"] <- est_delta
        tobit_estimates[tobit_estimates[, "id"] == id, "rho"] <- est_rho
        tobit_estimates[tobit_estimates[, "id"] == id, "sigma"] <- est_sigma
        tobit_estimates[tobit_estimates[, "id"] == id, "logL"] <- round(logLik(model), round_prec)
        # Use delta method to calculate standard errors of structural parameters
        est_beta_se <- deltamethod(list(~ exp(x1 / x3)), coeffs, vcov(model)[1:3, 1:3])
        est_delta_se <- deltamethod(list(~ exp(x2 / x3)), coeffs, vcov(model)[1:3, 1:3])
        est_rho_se <- deltamethod(list(~ 1 / x3 - curv), coeffs, vcov(model)[1:3, 1:3])
        # Add best-fitting allocations to plot (remove random component, i.e., sigma = 0)
        points(
          unlist(opt_noisy_points_on_BR(df_ind, est_beta, est_delta, est_rho, sigma = 0)["c_1"]),
          unlist(opt_noisy_points_on_BR(df_ind, est_beta, est_delta, est_rho, sigma = 0)["c_2"]),
          col = "#FFA50099",
          pch = 20
        )
        mtext(bquote(
          "Parameter estimates:" ~
            hat(italic(β)) ~ "=" ~ .(est_beta) * "," ~
            hat(italic(δ)) ~ "=" ~ .(est_delta) * "," ~
            hat(italic(ρ)) ~ "=" ~ .(est_rho) * "," ~
            hat(italic(σ)) ~ "=" ~ .(est_sigma)
        ), side = 3, col = "#FFA500", line = -1)
      },
      # If Tobit regression fails, issue error message
      error = function(e) {
        message(style_bold(bg_red(col_br_white("ERROR: ", conditionMessage(e)))))
      }
    )
    Sys.sleep(0.25)  # Short break to update plot
  }
  # Collect and display Tobit estimates
  tobit_estimates_report[[toString(curv)]] <- tobit_estimates
  print(cbind(params_sim, tobit_estimates_report[[toString(curv)]][, 2:6]))
}
# Collect estimates
all_estimates_report <- tobit_estimates_report
rm(curv, id, df_ind, tobit_estimates)
rm(coeffs, vcov_matrix)
rm(est_beta, est_beta_se, est_delta, est_delta_se, est_rho, est_rho_se, est_sigma)




# NONLINEAR MAXIMUM LIKELIHOOD ESTIMATION -----------------------------------------------------


# Log-likelihood contribution of a single observation according to eq. (30) in
# https://www.econtribute.de/RePEc/ajk/ajkdps/ECONtribute_336_2024.pdf
LL_contrib <- function(C_star_obs, t, k, lb, ub, R, z, beta, delta, rho, sigma) {
  C_star_pred <- C_star(t, k, R, z, beta, delta, rho)
  if (
    beta < 0 || delta < 0 || sigma < 0 || rho < -z
    # Parameters must not become negative, and
    # curvature of utility beyond curvature of BR cannot be identified
  ) {
    lnf <- -999999
  } else {
    # Interior solution
    lnf <- log(dnorm((log(C_star_obs) - log(C_star_pred)) / sigma) / sigma)
    # If observation is lower bound
    if (C_star_obs < lb + 0.00001) {
      lnf <- log(pnorm((log(lb) - log(C_star_pred)) / sigma))
    }
    # If observation is upper bound
    if (C_star_obs > ub - 0.00001) {
      lnf <- log(pnorm((log(C_star_pred) - log(ub)) / sigma))
    }
  }
  # Rule out NaNs and infinite values
  if (is.na(lnf) || is.nan(lnf) || lnf == -Inf) {
    lnf <- -999999
  }
  return(lnf)
}

# Log-likelihood contributions of all observations collected in vector
# (required by some optimization methods)
LL_contrib_vec <- function(C_star_obs, t, k, lb, ub, R, z, beta, delta, rho, sigma) {
  ln <- unlist(sapply(
    1:length(C_star_obs),
    function(i) {
      LL_contrib(
        C_star_obs[i],
        t[i], k[i], lb[i], ub[i], R[i], z[i],
        beta, delta, rho, sigma
      )
    }
  ))
  return(as.vector(ln))
}

ids <- unique(df$id)
mle_estimates_report <- list()

# Estimate separately for each curvature level of the budget restrictions
for (curv in unique(df$Curvature)) {
  # Initialize collection of estimates with NA
  mle_estimates <- matrix(rep(NA, length(ids) * 5), ncol = 5)
  mle_estimates <- cbind(ids, mle_estimates)
  colnames(mle_estimates) <- c("id", "beta", "delta", "rho", "sigma", "logL")
  # Estimate each individual separately
  for (id in ids) {
    print(paste0("NL-ML estimation, Subject ID: ", id, "; BR curvature: ", curv))
    df_ind <- df[df$id == id & df$Curvature == curv, ]
    # Objective function for maxLik must not contain any arguments except params
    LL_contrib_vec_filled <- function(params) {
      LL_contrib_vec(
        df_ind$payment_ratio,
        df_ind$FED, df_ind$Delay, df_ind$lb, df_ind$ub,
        df_ind$PriceOneDivPriceTwo, df_ind$Curvature,
        params[1], params[2], params[3], params[4]
      )
    }
    # Set the initial values for the numerical nonlinear estimation procedure
    # By default, take the outcome of the Tobit regression
    tobit_for_init <- tobit_estimates_report[[toString(curv)]]
    init_vals <- round(c(
      tobit_for_init[tobit_for_init[, "id"] == id, "beta"],
      tobit_for_init[tobit_for_init[, "id"] == id, "delta"],
      tobit_for_init[tobit_for_init[, "id"] == id, "rho"],
      tobit_for_init[tobit_for_init[, "id"] == id, "sigma"]
    ), 2)
    # If the Tobit regression yields nonsensical results, set different initical value
    if (init_vals["beta"] <= 0 | is.na(init_vals["beta"])) {
      init_vals["beta"] <- 0.95
    }
    if (init_vals["delta"] <= 0 | is.na(init_vals["delta"])) {
      init_vals["delta"] <- 0.95
    }
    if (is.na(init_vals["rho"])) {
      init_vals["rho"] <- 0.05
    } else if (init_vals["rho"] <= -curv) {
      init_vals["rho"] <- 1
    }
    if (init_vals["sigma"] <= 0 | is.na(init_vals["sigma"])) {
      init_vals["sigma"] <- 0.1
    }
    # As is frequently the case with numerical optimization procedures, individual-specific
    # initial values may be required, e.g., if Tobit did not converge. This is particularly likely
    # with linear budget restrictions and less so with strictly concave budget restrictions.
    if (id == 1 && curv == 0) {
      init_vals[c("beta", "delta", "rho", "sigma")] = c(1, 1, 0.01, 0.005)
    }
    if (id %in% c(2, 3) && curv == 0) {
      init_vals[c("beta", "delta", "rho", "sigma")] = c(0.995, 0.995, 0.01, 0.2)
    }
    # sum(LL_contrib_vec_filled(init_vals))  # Helpful for finding initial values
    # Plot simulated choices
    plotBRs(df_ind, curv)
    points(
      df_ind$payment_1,
      df_ind$payment_2,
      col = "navy",
      pch = 20
    )
    mtext(bquote(
      "Simulated choices for ID" ~ .(id) * ":" ~
        italic(β) ~ "=" ~ .(params_sim[params_sim[, "id"] == id, "beta"]) * "," ~
        italic(δ) ~ "=" ~ .(params_sim[params_sim[, "id"] == id, "delta"]) * "," ~
        italic(ρ) ~ "=" ~ .(params_sim[params_sim[, "id"] == id, "rho"]) * "," ~
        italic(σ) ~ "=" ~ .(params_sim[params_sim[, "id"] == id, "sigma"])
    ), side = 3, col = "navy", line = 0.25)
    tryCatch(
      {
        withCallingHandlers(
          # Attempt NL-MLE
          {
            mle_estim <- maxLik(
              logLik = LL_contrib_vec_filled,
              start = init_vals,
              # method = "BFGS",  # Broyden/Fletcher/Goldfarb/Shanno
              # method = "BFGSR",  # Broyden/Fletcher/Goldfarb/Shanno
              # method = "BHHH",  # Berndt/Hall/Hall/Hausman
              method = "NR", # Newton/Raphson
              # method = "SANN",  # Simulated Annealing
              control = list(
                gradtol = 10^-8, # Return code 1 (normal convergence)
                tol = 10^-8, # Return code 2 (normal convergence)
                steptol = -1, # Return code 3
                iterlim = 1000, # 10^6,  # Return code 4
                reltol = 10^-8 # Return code 8 (normal convergence)
              )
            )
            est_se <- stdEr(mle_estim, eigentol = 10^(-15))
          },
          # Show potential warning messages of the NL-MLE
          warning = function(w) {
            message(style_bold(col_blue("WARNING: ", conditionMessage(w))))
            invokeRestart("muffleWarning")
          }
        )
        # Add best-fitting allocations to plot (remove random component, i.e., sigma = 0)
        points(
          unlist(opt_noisy_points_on_BR(
            df_ind,
            mle_estim$estimate["beta"], mle_estim$estimate["delta"], mle_estim$estimate["rho"],
            sigma = 0
          )["c_1"]),
          unlist(opt_noisy_points_on_BR(
            df_ind,
            mle_estim$estimate["beta"], mle_estim$estimate["delta"], mle_estim$estimate["rho"],
            sigma = 0
          )["c_2"]),
          col = "#FF450088",
          pch = 20
        )
        mle_estim$estimate <- round(mle_estim$estimate, round_prec)
        mtext(bquote(
          "Parameter estimates:" ~
            hat(italic(β)) ~ "=" ~ .(mle_estim$estimate["beta"]) * "," ~
            hat(italic(δ)) ~ "=" ~ .(mle_estim$estimate["delta"]) * "," ~
            hat(italic(ρ)) ~ "=" ~ .(mle_estim$estimate["rho"]) * "," ~
            hat(italic(σ)) ~ "=" ~ .(mle_estim$estimate["sigma"])
        ), side = 3, col = "#FF4500", line = -1)
        mle_estimates[mle_estimates[, "id"] == id, "beta"] <- mle_estim$estimate["beta"]
        mle_estimates[mle_estimates[, "id"] == id, "delta"] <- mle_estim$estimate["delta"]
        mle_estimates[mle_estimates[, "id"] == id, "rho"] <- mle_estim$estimate["rho"]
        mle_estimates[mle_estimates[, "id"] == id, "sigma"] <- mle_estim$estimate["sigma"]
        mle_estimates[mle_estimates[, "id"] == id, "logL"] <- round(mle_estim$maximum, round_prec)
      },
      # If NL-MLE fails, issue error message
      error = function(e) {
        message(style_bold(bg_red(col_br_white("ERROR: ", conditionMessage(e)))))
      }
    )
    Sys.sleep(0.25)  # Short break to update plot
  }
  # Collect NL-MLE estimates
  mle_estimates_report[[toString(curv)]] <- mle_estimates
  cols = c("beta", "delta", "rho", "sigma", "logL")
  # Collect and display all estimates
  all_estimates_report[[toString(curv)]] <- cbind(
    params_sim,
    tobit_estimates_report[[toString(curv)]][, cols],
    mle_estimates_report[[toString(curv)]][, cols]
  )
  colnames(all_estimates_report[[toString(curv)]]) <-
    c(
      "id",
      "beta_sim", "delta_sim", "rho_sim", "sigma_sim",
      "beta_tobit", "delta_tobit", "rho_tobit", "sigma_tobit", "logL_tobit",
      "beta_mle", "delta_mle", "rho_mle", "sigma_mle", "logL_mle"
    )
  print(all_estimates_report[[toString(curv)]])
}
rm(curv, id, df_ind, mle_estimates, est_se, tobit_for_init)

all_estimates_report[["0"]][, 1:10]
# This should yield
#      id beta_sim delta_sim rho_sim sigma_sim beta_tobit delta_tobit rho_tobit sigma_tobit logL_tobit
# [1,]  1     1.00      1.00    0.00      0.00         NA          NA        NA      0.0006    32.1004
# [2,]  2     1.00      0.99    0.00      0.25     0.9557      0.9910    0.0000    155.4723 -9471.4181
# [3,]  3     0.80      0.99    0.00      0.25     0.9557      0.9910    0.0000    155.4723 -9471.4181
# [4,]  4     0.80      0.99    0.10      0.50     0.7998      0.9907    0.0961      0.3942   -15.9651
# [5,]  5     0.85      0.95    0.15      0.50     0.8554      0.9514    0.1556      0.4335   -14.5728
# [6,]  6     1.00      1.00    1.00      0.00     1.0000      1.0000    0.9999      0.0001   324.9009
# [7,]  7     1.00      0.99  100.00      0.25     0.6308      0.9941   -3.9738      0.1959     8.6606
# [8,]  8     1.00      1.00  200.00      0.05     0.1828      1.0957   57.7050      0.0496    64.9979
# [9,]  9     1.00      1.00  200.00      0.00     0.9814      1.0025  196.4053      0.0007   240.3492
# With linear budget restrictions:
# IDs 1, 2, and 3: Tobit does not converge for linear utility.
# Tobit converges to strongly convex utility instead of strongly concave utility for ID 7.
# ID 8: Strongly concave utility is hard to estimate in the presence of noise.

all_estimates_report[["0"]][, c(1:5, 11:15)]
# This should yield
#      id beta_sim delta_sim rho_sim sigma_sim beta_mle delta_mle  rho_mle sigma_mle logL_mle
# [1,]  1     1.00      1.00    0.00      0.00   1.0000    1.0000   0.0110    0.0003  34.8556
# [2,]  2     1.00      0.99    0.00      0.25   0.9905    0.9900   0.0001    0.1853   0.0000
# [3,]  3     0.80      0.99    0.00      0.25   0.9905    0.9900   0.0001    0.1853   0.0000
# [4,]  4     0.80      0.99    0.10      0.50   0.7998    0.9907   0.0961    0.3942 -15.9651
# [5,]  5     0.85      0.95    0.15      0.50   0.8554    0.9514   0.1556    0.4335 -14.5728
# [6,]  6     1.00      1.00    1.00      0.00   1.0000    1.0000   0.9999    0.0001 324.9009
# [7,]  7     1.00      0.99  100.00      0.25 555.2478    1.0991  57.9643    0.2008   7.6510
# [8,]  8     1.00      1.00  200.00      0.05   0.1745    1.0984  59.2789    0.0496  64.9978
# [9,]  9     1.00      1.00  200.00      0.00   0.9800    1.0000 196.4100    0.0049 179.5666
# With linear budget restrictions (LBRs):
# IDs 1, 2, and 3: Estimation becomes possible with NL-MLE by searching for suitable initial values,
# which, however, is cumbersome and difficult to automate.
# ID 7: Our NL-MLE routine converges to strongly concave utility, thereby fixing the weak point
# of Tobit. At the same time, with strongly concave utility all allocations are characterized by
# c_1 = c_2, with beta and delta hardly having any influence. Hence, identification of beta and
# delta becomes very hard.

all_estimates_report[["0.4"]][, 1:10]
# This should yield
#      id beta_sim delta_sim rho_sim sigma_sim beta_tobit delta_tobit rho_tobit sigma_tobit logL_tobit
# [1,]  1     1.00      1.00    0.00      0.00     1.0000      1.0000   -0.0002      0.0004   260.7333
# [2,]  2     1.00      0.99    0.00      0.25     0.9697      0.9903   -0.0093      0.2442    -0.3814
# [3,]  3     0.80      0.99    0.00      0.25     0.8085      0.9913    0.0063      0.2156     4.7404
# [4,]  4     0.80      0.99    0.10      0.50     0.8394      0.9889    0.0885      0.5054   -30.1979
# [5,]  5     0.85      0.95    0.15      0.50     0.9753      0.9509    0.0958      0.4442   -24.9066
# [6,]  6     1.00      1.00    1.00      0.00     0.9997      1.0000    0.9981      0.0004   258.3382
# [7,]  7     1.00      0.99  100.00      0.25     0.8767      0.9071   12.5038      0.2460    -0.6843
# [8,]  8     1.00      1.00  200.00      0.05     1.2020      1.0142   18.5775      0.0513    63.6310
# [9,]  9     1.00      1.00  200.00      0.00     0.9419      1.0080  196.3481      0.0006   248.9800
# With strictly concave budget restrictions (SCBRs):
# IDs 1, 2, and 3: Tobit converges for linear utility. Thus, SCBRs fix the weak point of LBRs.

all_estimates_report[["0.4"]][, c(1:5, 11:15)]
# This should yield
#      id beta_sim delta_sim rho_sim sigma_sim beta_mle delta_mle  rho_mle sigma_mle logL_mle
# [1,]  1     1.00      1.00    0.00      0.00   1.0000    1.0000  -0.0002    0.0004 260.7333
# [2,]  2     1.00      0.99    0.00      0.25   0.9697    0.9903  -0.0093    0.2442  -0.3814
# [3,]  3     0.80      0.99    0.00      0.25   0.8085    0.9913   0.0063    0.2156   4.7404
# [4,]  4     0.80      0.99    0.10      0.50   0.8394    0.9889   0.0885    0.5054 -30.1979
# [5,]  5     0.85      0.95    0.15      0.50   0.9753    0.9509   0.0958    0.4442 -24.9066
# [6,]  6     1.00      1.00    1.00      0.00   0.9997    1.0000   0.9981    0.0004 258.3382
# [7,]  7     1.00      0.99  100.00      0.25   0.8767    0.9071  12.5036    0.2460  -0.6843
# [8,]  8     1.00      1.00  200.00      0.05   1.2019    1.0142  18.5773    0.0513  63.6310
# [9,]  9     1.00      1.00  200.00      0.00   0.9400    1.0100 196.3500    0.0047 181.4055
# This illustrates that with the chosen optimization method ("NR"), our NL-MLE routine delivers
# (virtually) the same results as Tobit.
