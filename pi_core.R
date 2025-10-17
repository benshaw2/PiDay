# pi_core.R
# Shared core logic for Pi estimator
# Contains full-lme4 code for local use (if available),
# but WebR will use the simplified 'lm' fallback.

#' Fit Pi model
#' df: data.frame with columns Name, Diameter, Circumference
#' model_type: 1 = fixed effects (lm), 2 = random intercept (lme4), 3 = random intercept+slope (lme4)
#' out_csv: optional path to write CSV result (for JS/WebR)
fit_pi_model <- function(df, model_type = 1, safe = TRUE) {
  if (nrow(df) < 2) {
    result <- list(ok = FALSE, message = "Need at least 2 observations", slope = NA, intercept = NA)
  } else {

    # Numeric coercion
    df$Diameter <- as.numeric(df$Diameter)
    df$Circumference <- as.numeric(df$Circumference)

    result <- list(ok = FALSE, message = "Uninitialized", slope = NA, intercept = NA)

    # Try basic lm
    lm_fit <- tryCatch(lm(Circumference ~ Diameter, data = df), error = function(e) e)

    # Mixed effects if requested and available
    if ((model_type == 2 || model_type == 3) && !safe) {
      if (requireNamespace("lme4", quietly = TRUE)) {
        fit <- tryCatch({
          if (model_type == 2)
            lme4::lmer(Circumference ~ Diameter + (1 | Name), data = df)
          else
            lme4::lmer(Circumference ~ Diameter + (1 + Diameter | Name), data = df)
        }, error = function(e) e)

        if (!inherits(fit, "try-error") && !inherits(fit, "error")) {
          coefs <- stats::fixef(fit)
          result <- list(ok = TRUE, message = "lme4 model fitted",
                         slope = as.numeric(coefs["Diameter"]), intercept = as.numeric(coefs["(Intercept)"]))
        } else {
          result <- list(ok = FALSE, message = paste("lme4 failed:", conditionMessage(fit)),
                         slope = NA, intercept = NA)
        }
      } else {
        result <- list(ok = FALSE, message = "lme4 not available in this environment",
                       slope = NA, intercept = NA)
      }
    }

    # Default to lm
    if (inherits(lm_fit, "try-error") || inherits(lm_fit, "error")) {
      result <- list(ok = FALSE, message = paste("lm failed:", conditionMessage(lm_fit)),
                     slope = NA, intercept = NA)
    } else {
      coefs <- coef(lm_fit)
      result <- list(ok = TRUE, message = "lm fitted",
                     slope = as.numeric(coefs["Diameter"]), intercept = as.numeric(coefs["(Intercept)"]))
    }
  }

  # Construct string for JS
  ok_str <- ifelse(result$ok, "TRUE", "FALSE")
  msg_str <- gsub("\\|", "/", result$message)  # escape any '|' in message
  out_str <- paste(ok_str, msg_str, result$slope, result$intercept, sep = "|")

  #print(out_str)
  return(out_str)
}



#' Create and save ggplot to PNG or SVG
#' df: data.frame with Diameter and Circumference and optional Name
#' slope, intercept: numeric - use the values to draw abline
#' outfile: path, can end with .png or .svg
make_pi_plot <- function(df, slope = NA, intercept = NA, outfile = "plot.png", width = 800, height = 600) {
  # Determine output format
  ext <- tolower(tools::file_ext(outfile))
  if (ext == "svg") {
    svg(outfile, width = width / 100, height = height / 100)
  } else {
    png(outfile, width = width, height = height)
  }

  # Ensure Name is a factor for coloring
  df$Name <- as.factor(df$Name)

  # Set up plot limits
  xlim <- c(0, max(df$Diameter, na.rm = TRUE) * 1.15)
  ylim <- c(0, max(df$Circumference, na.rm = TRUE) * 1.15)

  # Assign colors
  colors <- rainbow(length(levels(df$Name)))
  names(colors) <- levels(df$Name)

  # Plot
  plot(df$Diameter, df$Circumference,
       col = colors[df$Name],
       pch = 19,
       xlim = xlim, ylim = ylim,
       xlab = "Diameter", ylab = "Circumference",
       main = "Circumference vs Diameter",
       cex = 1.5)

  # Add regression line if available
  if (!is.na(slope) && !is.na(intercept)) {
    abline(a = intercept, b = slope, col = "firebrick", lwd = 2)
    legend("topleft", legend = paste("π ≈", round(slope, 4)),
           col = "firebrick", lwd = 2, bty = "n")
  }

  # Add legend for names
  if (length(levels(df$Name)) > 1) {
    legend("bottomright", legend = levels(df$Name),
           col = colors, pch = 19, cex = 0.9, bty = "n")
  }

  dev.off()
  #invisible(outfile)
  #return(outfile)
}

