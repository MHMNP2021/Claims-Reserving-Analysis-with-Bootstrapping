# IFRS 17 Insurance Reserve Calculator - Professional Version
# Shiny app using the 6-Phase Core Analytics Workflow (Chain Ladder + ODP Bootstrap)

library(shiny)
library(shinydashboard)
library(readxl)
library(DT)
library(ggplot2)
library(dplyr)
library(moments)
library(gridExtra)
library(reshape2)
library(scales)

# ============================================================================
# CORE ANALYTICS (from 6-Phase Workflow) AS FUNCTIONS - ALIGNED WITH CORRECTED CODE
# ============================================================================

# Phase 1: Build cumul and incr triangles from a data frame
build_triangles <- function(df) {
  # Extract accident years (first column)
  accident_years <- as.character(df[[1]])
  
  # Extract triangle matrix (remove first column)
  triangle_matrix <- as.matrix(df[, -1])
  mode(triangle_matrix) <- "numeric"
  
  # Create cumulative triangle with proper naming
  cum_triangle <- triangle_matrix
  rownames(cum_triangle) <- accident_years
  colnames(cum_triangle) <- paste0("Dev", 1:ncol(cum_triangle))
  
  # Create incremental triangle using cum2incr logic
  inc_triangle <- cum_triangle
  n <- ncol(cum_triangle)
  for(j in n:2) {
    inc_triangle[, j] <- cum_triangle[, j] - cum_triangle[, j-1]
  }
  inc_triangle[, 1] <- cum_triangle[, 1]  # First column remains the same
  
  list(cumul = cum_triangle, incr = inc_triangle, 
       n_orig = nrow(cum_triangle), n_dev = ncol(cum_triangle),
       accident_years = accident_years)
}

# Helper function: Calculate development factors (corrected version)
calculate_factors <- function(cum_tri) {
  n <- ncol(cum_tri)
  factors <- numeric(n - 1)
  for(j in 1:(n-1)) {
    # Sum of C(i, k+1) / Sum of C(i, k) for observed data
    idx <- which(!is.na(cum_tri[, j+1]))
    sum_curr <- sum(cum_tri[idx, j], na.rm = TRUE)
    sum_next <- sum(cum_tri[idx, j+1], na.rm = TRUE)
    factors[j] <- ifelse(sum_curr == 0, 1, sum_next / sum_curr)
  }
  return(factors)
}

# Helper function: Project triangle forward
project_triangle <- function(tri, factors) {
  n_rows <- nrow(tri)
  n_cols <- ncol(tri)
  full_tri <- tri
  for(i in 1:n_rows) {
    for(j in 1:(n_cols-1)) {
      if(is.na(full_tri[i, j+1])) {
        full_tri[i, j+1] <- full_tri[i, j] * factors[j]
      }
    }
  }
  return(full_tri)
}

# Phase 2: Deterministic chain ladder (aligned with corrected code)
compute_dev_factors <- function(cumul) {
  calculate_factors(cumul)
}

decide_tail_factor <- function(dev_factors) {
  if (length(dev_factors) >= 3 && mean(tail(dev_factors, 3)) > 1.001) {
    mean(tail(dev_factors, min(5, length(dev_factors))))
  } else {
    1.0
  }
}

project_cumulative <- function(cumul, dev_factors, tail_factor = 1.0) {
  n_orig <- nrow(cumul); n_dev <- ncol(cumul)
  proj_cumul <- project_triangle(cumul, dev_factors)
  
  # Apply tail factor if needed
  if (tail_factor > 1.0 && n_dev > length(dev_factors) + 1) {
    for(i in 1:n_orig) {
      known <- which(!is.na(cumul[i, ]))
      if (length(known) > 0) {
        latest <- max(known)
        if (latest < n_dev) {
          for(j in (length(dev_factors) + 2):n_dev) {
            proj_cumul[i, j] <- proj_cumul[i, j-1] * tail_factor
          }
        }
      }
    }
  }
  
  # Calculate reserves
  latest_diagonal <- apply(cumul, 1, function(x) tail(na.omit(x), 1))
  ultimate_values <- proj_cumul[, n_dev]
  reserves_by_ay <- ultimate_values - latest_diagonal
  total_reserve <- sum(reserves_by_ay, na.rm = TRUE)
  
  list(proj_cumul = proj_cumul, 
       reserves_by_ay = reserves_by_ay, 
       total_reserve = total_reserve,
       ultimate_claims = ultimate_values,
       latest_observed = latest_diagonal)
}

# Fitted incremental calculation (aligned with corrected code)
fitted_incremental <- function(cumul, dev_factors) {
  n_orig <- nrow(cumul)
  n_dev <- ncol(cumul)
  
  # Step 1: Calculate fitted cumulative using backward then forward projection
  fitted_cum <- cumul
  
  # Get latest known diagonal for each row
  row_diags <- apply(cumul, 1, function(x) tail(na.omit(x), 1))
  col_len <- apply(cumul, 1, function(x) length(na.omit(x)))
  
  # Fill backwards from latest known
  for(i in 1:n_orig) {
    curr_col <- col_len[i]
    if (curr_col > 1) {
      for(j in (curr_col-1):1) {
        fitted_cum[i, j] <- fitted_cum[i, j+1] / dev_factors[j]
      }
    }
  }
  
  # Fill forwards (standard CL)
  fitted_cum <- project_triangle(fitted_cum, dev_factors)
  
  # Step 2: Convert to incremental
  fitted_incr <- fitted_cum
  for(j in n_dev:2) {
    fitted_incr[, j] <- fitted_cum[, j] - fitted_cum[, j-1]
  }
  
  return(fitted_incr)
}

# Phase 3: Pearson residuals under ODP (aligned with corrected code)
pearson_residuals_odp <- function(incr, fitted_incr) {
  # Calculate Pearson residuals: (Actual - Fitted) / sqrt(Fitted)
  residuals_matrix <- (incr - fitted_incr) / sqrt(abs(fitted_incr))
  
  # Extract valid residuals (exclude NAs and infinite values)
  valid_residuals <- residuals_matrix[!is.na(residuals_matrix) & is.finite(residuals_matrix)]
  
  # Center residuals (subtract mean)
  centered_residuals <- valid_residuals - mean(valid_residuals, na.rm = TRUE)
  
  return(centered_residuals)
}

# Phase 4: Bootstrap using residuals (aligned with corrected code)
bootstrap_reserves_core <- function(cumul, fitted_incr, residuals, dev_factors, tail_factor, n_sim = 5000, seed = 123) {
  set.seed(seed)
  n_orig <- nrow(cumul)
  n_dev <- ncol(cumul)
  sim_res <- numeric(n_sim)
  
  # Create incremental triangle from cumulative
  incr_triangle <- cumul
  for(j in n_dev:2) {
    incr_triangle[, j] <- cumul[, j] - cumul[, j-1]
  }
  incr_triangle[, 1] <- cumul[, 1]
  
  for(s in 1:n_sim) {
    tryCatch({
      # 1. Sample residuals with replacement
      pseudo_resid <- matrix(
        sample(residuals, length(incr_triangle), replace = TRUE),
        nrow = n_orig,
        ncol = n_dev
      )
      
      # 2. Calculate Pseudo-Incremental: Fit + Resid * sqrt(Fit)
      pseudo_inc <- fitted_incr + pseudo_resid * sqrt(abs(fitted_incr))
      
      # 3. Convert to Pseudo-Cumulative
      pseudo_cum <- pseudo_inc
      for(j in 2:n_dev) {
        pseudo_cum[, j] <- pseudo_cum[, j-1] + pseudo_inc[, j]
      }
      
      # 4. Mask the future (keep only the upper triangle observed structure)
      pseudo_cum[is.na(cumul)] <- NA
      
      # 5. Recalculate development factors
      sim_factors <- calculate_factors(pseudo_cum)
      
      # 6. Re-project triangle
      sim_full_tri <- project_triangle(pseudo_cum, sim_factors)
      
      # Apply tail factor if needed
      if (tail_factor > 1.0 && n_dev > length(sim_factors) + 1) {
        for(i in 1:n_orig) {
          known <- which(!is.na(pseudo_cum[i, ]))
          if (length(known) > 0) {
            latest <- max(known)
            if (latest < n_dev) {
              for(j in (length(sim_factors) + 2):n_dev) {
                sim_full_tri[i, j] <- sim_full_tri[i, j-1] * tail_factor
              }
            }
          }
        }
      }
      
      # 7. Calculate reserve
      sim_latest <- apply(pseudo_cum, 1, function(x) tail(na.omit(x), 1))
      sim_ultimate <- sim_full_tri[, n_dev]
      sim_reserve <- sum(sim_ultimate - sim_latest, na.rm = TRUE)
      
      sim_res[s] <- max(0, sim_reserve)
      
    }, error = function(e) {
      sim_res[s] <- NA
    })
  }
  
  # Clean NAs
  sim_res <- na.omit(sim_res)
  return(sim_res)
}

# Phase 5: Risk metrics (updated with TVaR calculation)
compute_risk_metrics <- function(sim_reserves, levels = c(0.5, 0.75, 0.80, 0.90, 0.95, 0.995)) {
  results <- list()
  m <- mean(sim_reserves, na.rm = TRUE)
  
  for (cl in levels) {
    var_val <- as.numeric(quantile(sim_reserves, probs = cl, names = FALSE, na.rm = TRUE))
    
    # Calculate TVaR (Conditional Tail Expectation)
    tail_vals <- sim_reserves[sim_reserves > var_val]
    tvar <- if (length(tail_vals) > 0) mean(tail_vals, na.rm = TRUE) else var_val
    
    risk_adjustment <- max(tvar - m, 0)
    
    results[[paste0("CL_", round(cl * 100))]] <- list(
      confidence_level = cl,
      var = var_val,
      tvar = tvar,
      risk_adjustment = risk_adjustment,
      cte_rate = ifelse(m > 0, (tvar - m) / m * 100, NA_real_)
    )
  }
  results
}

# Phase 6: IFRS 17 liability (updated)
compute_ifrs17_liability <- function(deterministic_be, simulated_mean, risk_measure, confidence_level, method = "TVaR") {
  risk_adj <- max(risk_measure - simulated_mean, 0)
  liability <- deterministic_be + risk_adj
  
  list(
    deterministic_be = deterministic_be,
    simulated_mean = simulated_mean,
    risk_measure = risk_measure,
    risk_adjustment = risk_adj,
    liability = liability,
    ra_percentage = ifelse(deterministic_be == 0, NA_real_, risk_adj / deterministic_be * 100),
    confidence_level = confidence_level,
    method = method
  )
}

# Diagnostics (updated)
compute_diagnostics <- function(residuals, reserve_dist, deterministic_be) {
  results <- list()
  
  # Residual diagnostics
  if (length(residuals) >= 3) {
    shapiro_test <- shapiro.test(residuals)
    results$shapiro_pvalue <- shapiro_test$p.value
    results$is_normal <- shapiro_test$p.value > 0.05
  } else {
    results$shapiro_pvalue <- NA_real_
    results$is_normal <- NA
  }
  
  results$residual_count <- length(residuals)
  results$residual_mean <- mean(residuals, na.rm = TRUE)
  results$residual_sd <- sd(residuals, na.rm = TRUE)
  results$residual_skewness <- moments::skewness(residuals, na.rm = TRUE)
  results$residual_kurtosis <- moments::kurtosis(residuals, na.rm = TRUE)
  
  # Reserve distribution diagnostics
  results$reserve_mean <- mean(reserve_dist, na.rm = TRUE)
  results$reserve_sd <- sd(reserve_dist, na.rm = TRUE)
  results$reserve_cv <- ifelse(results$reserve_mean > 0, 
                               results$reserve_sd / results$reserve_mean, 
                               NA_real_)
  results$reserve_iqr <- IQR(reserve_dist, na.rm = TRUE)
  results$reserve_range <- range(reserve_dist, na.rm = TRUE)
  results$deterministic_be <- deterministic_be
  results$difference_pct <- ifelse(deterministic_be == 0, NA_real_, 
                                   (results$reserve_mean - deterministic_be) / deterministic_be * 100)
  
  # Convergence check
  n_sims <- length(reserve_dist)
  check_points <- c(100, 500, 1000, min(5000, n_sims))
  convergence <- list()
  
  for (n in check_points) {
    if (n <= n_sims) {
      convergence[[as.character(n)]] <- list(
        mean = mean(reserve_dist[1:n], na.rm = TRUE),
        sd = sd(reserve_dist[1:n], na.rm = TRUE)
      )
    }
  }
  
  results$convergence <- convergence
  return(results)
}

# ============================================================================
# SHINY UI
# ============================================================================

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = span(icon("calculator"), "IFRS 17 Reserve Calculator v3.0"),
    titleWidth = 350
  ),
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      menuItem("📊 Data Upload", tabName = "upload", icon = icon("upload")),
      menuItem("⚙️ Analysis Config", tabName = "config", icon = icon("sliders")),
      menuItem("📈 Results Dashboard", tabName = "results", icon = icon("chart-bar")),
      menuItem("🔍 Diagnostics", tabName = "diagnostics", icon = icon("microscope")),
      menuItem("📖 Documentation", tabName = "docs", icon = icon("book"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Custom styling */
        .box {
          border-top: 3px solid #3c8dbc;
          box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
        }
        .box-header { font-weight: bold; }
        .small-box { border-radius: 5px; margin-bottom: 10px; }
        .dataTables_wrapper { overflow-x: auto; }
        .alert { border-radius: 4px; margin-bottom: 15px; }
        .content-wrapper { background-color: #f4f6f9; }
        h2 {
          color: #2c3e50;
          border-bottom: 2px solid #3c8dbc;
          padding-bottom: 10px;
          margin-bottom: 20px;
        }
        .info-box { min-height: 100px; }
      "))
    ),
    tabItems(
      # ========== TAB 1: DATA UPLOAD ==========
      tabItem(tabName = "upload",
              h2("Data Upload & Validation"),
              fluidRow(
                box(
                  title = "Upload Claims Triangle",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  fileInput("file_input",
                            "Choose Excel or CSV File",
                            accept = c(".xlsx", ".xls", ".csv"),
                            width = "100%"),
                  tags$div(style = "background: #e8f4fc; padding: 15px; border-radius: 5px; margin-top: 15px;",
                           h4(icon("info-circle"), "Data Format Requirements:", style = "color: #2c3e50;"),
                           tags$ul(
                             tags$li(strong("First column:"), "Origin periods (e.g., 2017, 2018)"),
                             tags$li(strong("Other columns:"), "Cumulative claims by development period"),
                             tags$li(strong("Format:"), "Numeric values only"),
                             tags$li(strong("Missing values:"), "Leave blank or NA for future development"),
                             tags$li(strong("Minimum:"), "2×2 data matrix required")
                           ),
                           tags$p("Example format from PDF:", style = "margin-top: 10px; font-weight: bold;"),
                           tags$pre("Origin Period, 3m, 6m, 9m, 12m, ...\n2017, 549054, 2602782, 5569629, 14045024, ...\n2018, 6881149, 21794807, 53536790, 99814964, ...")
                  )
                )
              ),
              fluidRow(
                box(
                  title = "File Information",
                  status = "info",
                  solidHeader = TRUE,
                  width = 4,
                  height = "250px",
                  verbatimTextOutput("file_stats")
                ),
                box(
                  title = "Data Preview",
                  status = "success",
                  solidHeader = TRUE,
                  width = 8,
                  height = "400px",
                  div(style = 'overflow-x: auto; height: 350px;',
                      DTOutput("preview_table")
                  ),
                  tags$p(icon("mouse-pointer"), "Use horizontal scroll to view all columns",
                         style = "margin-top: 10px; color: #666; font-size: 12px; text-align: center;")
                )
              )
      ),
      # ========== TAB 2: ANALYSIS CONFIGURATION ==========
      tabItem(tabName = "config",
              h2("Analysis Configuration"),
              fluidRow(
                box(
                  title = "Bootstrap Settings",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  numericInput("n_simulations",
                               "Number of Simulations",
                               value = 5000, min = 1000, max = 100000, step = 1000),
                  tags$div(class = "help-block", "Higher values give more stable results but take longer to compute."),
                  br(),
                  numericInput("random_seed",
                               "Random Seed",
                               value = 12345, min = 1, max = 999999),
                  tags$div(class = "help-block", "Set seed for reproducible results."),
                  br(),
                  selectInput("confidence_predefined",
                              "Predefined Confidence Levels",
                              choices = c("75%", "80%", "90%", "95%", "99.5%"),
                              selected = c("80%", "90%", "95%"),
                              multiple = TRUE),
                  tags$div(class = "help-block", "Select confidence levels for analysis.")
                ),
                box(
                  title = "Risk Adjustment Method",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  radioButtons("ra_method",
                               "Risk Adjustment Method",
                               choices = list(
                                 "Tail Value at Risk (CTE/TVaR)" = "tvar",
                                 "Value at Risk (Percentile)" = "var",
                                 "Standard Deviation Multiple" = "sd"
                               ),
                               selected = "tvar"),
                  conditionalPanel(
                    condition = "input.ra_method == 'sd'",
                    numericInput("sd_multiplier",
                                 "Standard Deviation Multiplier",
                                 value = 1.0, min = 0.5, max = 3.0, step = 0.1)
                  ),
                  br(),
                  h4("Advanced Options"),
                  checkboxInput("use_deterministic_be",
                                "Use Deterministic Best Estimate for Liability",
                                value = TRUE),
                  tags$div(class = "help-block",
                           "If checked, uses chain ladder BE for liability calculation (IFRS 17 compliant).")
                )
              ),
              fluidRow(
                box(
                  title = "Execute Analysis",
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,
                  actionButton("run_btn",
                               icon = icon("rocket"),
                               " Run Comprehensive Analysis",
                               class = "btn-success",
                               style = "padding: 15px 40px; font-size: 18px; font-weight: bold; width: 100%;"),
                  br(), br(),
                  uiOutput("run_message"),
                  conditionalPanel(
                    condition = "input.run_btn > 0",
                    div(style = "margin-top: 20px;",
                        h4("Analysis Progress"),
                        verbatimTextOutput("analysis_progress")
                    )
                  )
                )
              )
      ),
      # ========== TAB 3: RESULTS DASHBOARD ==========
      tabItem(tabName = "results",
              h2("IFRS 17 Liability Results"),
              uiOutput("results_header"),
              uiOutput("results_content")
      ),
      # ========== TAB 4: DIAGNOSTICS ==========
      tabItem(tabName = "diagnostics",
              h2("Model Diagnostics & Validation"),
              uiOutput("diagnostics_content")
      ),
      # ========== TAB 5: DOCUMENTATION ==========
      tabItem(tabName = "docs",
              h2("IFRS 17 Methodology Documentation"),
              box(
                title = "Methodology Overview",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                collapsible = TRUE,
                collapsed = FALSE,
                h3("1. Chain Ladder Method", style = "color: #2c3e50;"),
                p("Deterministic chain ladder with volume-weighted age-to-age factors, optional tail, and reserves as ultimate minus latest."),
                tags$ul(
                  tags$li("Development factors calculated using observed cumulative data"),
                  tags$li("Triangle projected to ultimate for each origin period"),
                  tags$li("Reserves calculated as Ultimate − Latest"),
                  tags$li("Provides deterministic Best Estimate (BE)")
                ),
                h3("2. Bootstrap Stochastic Analysis", style = "color: #2c3e50; margin-top: 20px;"),
                p("Pearson residuals under ODP; residuals centered; residual resampling; re-estimation of factors each simulation."),
                tags$ol(
                  tags$li("Compute development factors and fitted incremental values"),
                  tags$li("Compute Pearson residuals: (Actual − Fitted)/sqrt(Fitted)"),
                  tags$li("Center residuals to mean 0"),
                  tags$li("Resample residuals with replacement"),
                  tags$li("Generate boot triangles: Boot_incremental = Fitted + r × sqrt(Fitted)"),
                  tags$li("Recompute development factors per simulation"),
                  tags$li("Project to ultimate and calculate reserves"),
                  tags$li("Repeat for specified number of simulations")
                ),
                h3("3. Risk Metrics", style = "color: #2c3e50; margin-top: 20px;"),
                tags$ul(
                  tags$li(strong("Value at Risk (VaR):"), " α-th percentile of reserve distribution"),
                  tags$li(strong("Tail Value at Risk (TVaR/CTE):"), " Expected loss in worst (1−α)% of cases"),
                  tags$li(strong("Risk Adjustment:"), " Selected risk measure minus simulated mean")
                ),
                h3("4. IFRS 17 Liability Calculation", style = "color: #2c3e50; margin-top: 20px;"),
                p(strong("Liability = Best Estimate + Risk Adjustment")),
                p("Where:", style = "margin-left: 20px;"),
                tags$ul(
                  tags$li("Best Estimate = Deterministic chain ladder reserve"),
                  tags$li("Risk Adjustment = max(Risk Measure − Simulated Mean, 0)"),
                  tags$li("Risk Measure can be VaR, TVaR, or Mean + k×SD")
                ),
                br(),
                h4("Key Features of This Implementation:", style = "color: #27ae60;"),
                tags$ul(
                  tags$li("✓ ODP Pearson residuals"),
                  tags$li("✓ Residual centering"),
                  tags$li("✓ Parameter risk via factor re-estimation"),
                  tags$li("✓ Multiple confidence levels"),
                  tags$li("✓ Comprehensive diagnostics"),
                  tags$li("✓ Professional visualization")
                )
              )
      )
    )
  )
)

# ============================================================================
# SERVER LOGIC
# ============================================================================

server <- function(input, output, session) {
  
  # Reactive storage
  data_state <- reactiveValues(
    raw_data = NULL,
    analysis_results = NULL,
    analysis_done = FALSE,
    error_msg = "",
    progress_text = ""
  )
  
  # Upload handler
  observeEvent(input$file_input, {
    req(input$file_input)
    tryCatch({
      file_path <- input$file_input$datapath
      file_ext <- tolower(tools::file_ext(input$file_input$name))
      if (file_ext == "csv") {
        data <- read.csv(file_path, check.names = FALSE, stringsAsFactors = FALSE)
      } else if (file_ext %in% c("xlsx", "xls")) {
        data <- as.data.frame(readxl::read_excel(file_path))
      } else {
        stop("Unsupported file format. Please upload CSV or Excel file.")
      }
      if (nrow(data) < 2 || ncol(data) < 2) {
        stop("Minimum 2x2 matrix required. Your data has ", nrow(data), " rows and ", ncol(data), " columns.")
      }
      data_state$raw_data <- data
      data_state$error_msg <- ""
      data_state$progress_text <- paste("File uploaded successfully:", input$file_input$name)
    }, error = function(e) {
      data_state$error_msg <- paste("Upload Error:", e$message)
      data_state$raw_data <- NULL
      data_state$progress_text <- ""
    })
  })
  
  # File info
  output$file_stats <- renderText({
    req(data_state$raw_data)
    data <- data_state$raw_data
    paste(
      "File: ", input$file_input$name, "\n",
      "Rows: ", nrow(data), "\n",
      "Columns: ", ncol(data), "\n",
      "First Column: ", names(data)[1], "\n",
      "Data Type: ", class(data[[1]]), "\n",
      "Numeric Columns: ", sum(sapply(data, is.numeric)), "\n",
      "\nPreview available to the right →"
    )
  })
  
  # Data preview
  output$preview_table <- renderDT({
    req(data_state$raw_data)
    data <- data_state$raw_data
    formatted_data <- data
    for (col in names(formatted_data)) {
      if (is.numeric(formatted_data[[col]])) {
        formatted_data[[col]] <- format(round(formatted_data[[col]], 2),
                                        big.mark = ",",
                                        scientific = FALSE,
                                        trim = TRUE)
      }
    }
    datatable(
      formatted_data,
      options = list(
        scrollX = TRUE,
        scrollY = "300px",
        pageLength = 10,
        lengthMenu = c(5, 10, 15, 20),
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'copy', title = NULL),
          list(extend = 'csv', title = "IFRS17_Data"),
          list(extend = 'excel', title = "IFRS17_Data")
        ),
        columnDefs = list(
          list(targets = '_all', className = 'dt-center')
        )
      ),
      extensions = 'Buttons',
      class = 'cell-border stripe hover nowrap',
      rownames = FALSE,
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: center;',
        'Data Preview - Showing first 10 rows. Use scrollbars to navigate.'
      )
    ) %>%
      formatStyle(columns = names(formatted_data), fontSize = '12px')
  })
  
  # Run analysis
  observeEvent(input$run_btn, {
    req(data_state$raw_data)
    data_state$progress_text <- "Starting analysis..."
    withProgress(message = 'Running Comprehensive Analysis', value = 0, {
      tryCatch({
        df <- data_state$raw_data
        
        # Phase 1: Build triangles from raw data
        data_state$progress_text <- "Phase 1: Building triangles..."
        incProgress(0.1, detail = "Data preparation")
        triangles <- build_triangles(df)
        cumul <- triangles$cumul
        incr  <- triangles$incr
        n_orig <- triangles$n_orig
        n_dev  <- triangles$n_dev
        
        # Phase 2: Deterministic Chain Ladder
        data_state$progress_text <- "Phase 2: Chain ladder projection..."
        incProgress(0.25, detail = "Deterministic reserving")
        dev_factors <- compute_dev_factors(cumul)
        tail_factor <- decide_tail_factor(dev_factors)
        proj <- project_cumulative(cumul, dev_factors, tail_factor)
        BE <- proj$total_reserve
        reserves_by_ay <- proj$reserves_by_ay
        ultimate_claims <- proj$ultimate_claims
        latest_observed <- proj$latest_observed
        
        # Calculate fitted incremental (aligned with corrected code)
        fitted_incr <- fitted_incremental(cumul, dev_factors)
        
        # Phase 3: Residuals
        data_state$progress_text <- "Phase 3: Residual diagnostics..."
        incProgress(0.4, detail = "Residual analysis")
        residuals <- pearson_residuals_odp(incr, fitted_incr)
        
        if (length(residuals) < 10) stop("Insufficient residuals for bootstrap.")
        
        # Phase 4: Bootstrap
        data_state$progress_text <- "Phase 4: Running bootstrap..."
        incProgress(0.7, detail = "Bootstrap simulation")
        sim_reserves <- bootstrap_reserves_core(
          cumul = cumul,
          fitted_incr = fitted_incr,
          residuals = residuals,
          dev_factors = dev_factors,
          tail_factor = tail_factor,
          n_sim = input$n_simulations,
          seed = input$random_seed
        )
        
        # Distribution stats
        sim_mean <- mean(sim_reserves, na.rm = TRUE)
        sim_sd <- sd(sim_reserves, na.rm = TRUE)
        sim_median <- median(sim_reserves, na.rm = TRUE)
        sim_cv <- ifelse(sim_mean > 0, sim_sd / sim_mean, NA_real_)
        
        # Phase 5: Risk metrics
        data_state$progress_text <- "Phase 5: Computing risk metrics..."
        incProgress(0.85, detail = "Risk metrics")
        conf_levels <- as.numeric(gsub("%", "", input$confidence_predefined)) / 100
        if (length(conf_levels) == 0) conf_levels <- c(0.80)  # Default to 80%
        
        # Add 99.5% if selected
        if ("99.5%" %in% input$confidence_predefined) {
          conf_levels <- c(conf_levels, 0.995)
        }
        
        risk_metrics <- compute_risk_metrics(sim_reserves, conf_levels)
        
        # Select RA method
        primary_cl <- conf_levels[1]
        cl_key <- paste0("CL_", round(primary_cl * 100))
        
        if (input$ra_method == "tvar") {
          risk_measure_val <- risk_metrics[[cl_key]]$tvar
          method_name <- "TVaR"
        } else if (input$ra_method == "var") {
          risk_measure_val <- risk_metrics[[cl_key]]$var
          method_name <- "VaR"
        } else {
          risk_measure_val <- sim_mean + input$sd_multiplier * sim_sd
          method_name <- paste0("Mean + ", input$sd_multiplier, "×SD")
        }
        
        # Phase 6: IFRS 17 Liability
        data_state$progress_text <- "Phase 6: IFRS 17 liability..."
        incProgress(0.95, detail = "IFRS 17")
        be_for_liability <- if (isTRUE(input$use_deterministic_be)) BE else sim_mean
        liability <- compute_ifrs17_liability(
          deterministic_be = BE,
          simulated_mean = sim_mean,
          risk_measure = risk_measure_val,
          confidence_level = primary_cl,
          method = method_name
        )
        
        # Diagnostics
        diag <- compute_diagnostics(
          residuals = residuals,
          reserve_dist = sim_reserves,
          deterministic_be = BE
        )
        
        # Store results
        data_state$analysis_results <- list(
          chain_ladder = list(
            dev_factors = dev_factors,
            projected_triangle = proj$proj_cumul,
            ultimate_claims = ultimate_claims,
            latest_observed = latest_observed,
            reserves_by_ay = reserves_by_ay,
            total_reserve = BE,
            n_years = n_orig,
            n_dev = n_dev,
            tail_factor = tail_factor
          ),
          bootstrap = list(
            reserve_distribution = sim_reserves,
            mean = sim_mean,
            sd = sim_sd,
            median = sim_median,
            cv = sim_cv,
            residuals = residuals,
            deterministic_be = BE
          ),
          risk_metrics = risk_metrics,
          liability = liability,
          diagnostics = diag,
          triangle = cumul,
          origin_labels = triangles$accident_years,
          primary_confidence = primary_cl
        )
        
        data_state$analysis_done <- TRUE
        data_state$error_msg <- ""
        data_state$progress_text <- "Analysis completed successfully!"
        incProgress(1.0, detail = "Complete!")
        
      }, error = function(e) {
        data_state$error_msg <- paste("Analysis Error:", e$message)
        data_state$analysis_done <- FALSE
        data_state$progress_text <- paste("Error:", e$message)
      })
    })
  })
  
  # Analysis progress
  output$analysis_progress <- renderText({
    data_state$progress_text
  })
  
  # Messages
  output$run_message <- renderUI({
    if (data_state$analysis_done) {
      div(class = "alert alert-success",
          icon("check-circle"),
          strong(" Success!"),
          " Analysis completed. Navigate to Results tab.")
    } else if (data_state$error_msg != "") {
      div(class = "alert alert-danger",
          icon("exclamation-circle"),
          strong(" Error: "),
          data_state$error_msg)
    }
  })
  
  # Results header
  output$results_header <- renderUI({
    if (!data_state$analysis_done) {
      return(
        div(class = "alert alert-info",
            icon("info-circle"),
            " Please run the analysis first from the Analysis Config tab.")
      )
    }
    res <- data_state$analysis_results
    fluidRow(
      valueBox(
        value = paste("LKR", format(round(res$liability$liability), big.mark = ",")),
        subtitle = "IFRS 17 Liability",
        icon = icon("balance-scale"),
        color = "green",
        width = 12
      )
    )
  })
  
  # Results content
  output$results_content <- renderUI({
    req(data_state$analysis_done)
    res <- data_state$analysis_results
    be <- res$bootstrap$deterministic_be
    ra <- res$liability$risk_adjustment
    
    tagList(
      fluidRow(
        valueBox(
          value = paste("LKR", format(round(be), big.mark = ",")),
          subtitle = "Deterministic Best Estimate",
          icon = icon("calculator"),
          color = "light-blue",
          width = 4
        ),
        valueBox(
          value = paste("LKR", format(round(ra), big.mark = ",")),
          subtitle = paste("Risk Adjustment (", res$liability$method, ")"),
          icon = icon("shield-alt"),
          color = "yellow",
          width = 4
        ),
        valueBox(
          value = paste(round(res$liability$ra_percentage, 1), "%"),
          subtitle = "Risk Margin % of BE",
          icon = icon("percent"),
          color = "purple",
          width = 4
        )
      ),
      br(),
      fluidRow(
        box(
          title = "Reserve Distribution",
          status = "primary",
          solidHeader = TRUE,
          width = 6,
          plotOutput("reserve_dist_plot")
        ),
        box(
          title = "Key Statistics",
          status = "info",
          solidHeader = TRUE,
          width = 6,
          tableOutput("key_stats_table")
        )
      ),
      fluidRow(
        box(
          title = "Risk Metrics at Different Confidence Levels",
          status = "success",
          solidHeader = TRUE,
          width = 12,
          div(style = 'overflow-x: auto;',
              tableOutput("risk_metrics_table")
          )
        )
      ),
      fluidRow(
        box(
          title = "Accident Year Analysis",
          status = "warning",
          solidHeader = TRUE,
          width = 12,
          div(style = 'overflow-x: auto;',
              tableOutput("ay_analysis_table")
          )
        )
      ),
      fluidRow(
        box(
          title = "Development Factors",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          tableOutput("dev_factors_table")
        )
      )
    )
  })
  
  # Reserve distribution plot
  output$reserve_dist_plot <- renderPlot({
    req(data_state$analysis_done)
    res <- data_state$analysis_results
    dist <- res$bootstrap$reserve_distribution
    be <- res$bootstrap$deterministic_be
    mean_sim <- res$bootstrap$mean
    
    primary_cl <- res$primary_confidence
    cl_key <- paste0("CL_", round(primary_cl * 100))
    var_val <- res$risk_metrics[[cl_key]]$var
    tvar_val <- res$risk_metrics[[cl_key]]$tvar
    
    par(mar = c(5, 4, 4, 4) + 0.1)
    hist(dist,
         breaks = 50,
         main = "Bootstrap Reserve Distribution",
         xlab = "Reserve Amount ($)",
         ylab = "Frequency",
         col = "lightblue",
         border = "white",
         cex.main = 1.2,
         cex.lab = 1.1,
         freq = TRUE)
    abline(v = be, col = "blue", lwd = 3, lty = 2)
    abline(v = mean_sim, col = "red", lwd = 3, lty = 1)
    abline(v = var_val, col = "darkgreen", lwd = 3, lty = 3)
    abline(v = tvar_val, col = "purple", lwd = 3, lty = 4)
    legend("topright",
           legend = c(
             paste("BE:", format(round(be), big.mark = ",")),
             paste("Sim Mean:", format(round(mean_sim), big.mark = ",")),
             paste(primary_cl*100, "% VaR:", format(round(var_val), big.mark = ",")),
             paste(primary_cl*100, "% TVaR:", format(round(tvar_val), big.mark = ","))
           ),
           col = c("blue", "red", "darkgreen", "purple"),
           lwd = 3,
           lty = c(2, 1, 3, 4),
           bg = "white",
           box.lwd = 1)
    par(new = TRUE)
    plot(density(dist),
         col = "darkred",
         lwd = 2,
         axes = FALSE,
         xlab = "",
         ylab = "",
         main = "")
    axis(4)
    mtext("Density", side = 4, line = 2.5)
  })
  
  # Key statistics table
  output$key_stats_table <- renderTable({
    req(data_state$analysis_done)
    res <- data_state$analysis_results
    data.frame(
      Statistic = c(
        "Deterministic Best Estimate",
        "Simulated Mean",
        "Difference (%)",
        "Standard Deviation",
        "Coefficient of Variation",
        "Median",
        "IQR",
        "Minimum",
        "Maximum"
      ),
      Value = c(
        format(round(res$bootstrap$deterministic_be), big.mark = ","),
        format(round(res$bootstrap$mean), big.mark = ","),
        paste(round((res$bootstrap$mean - res$bootstrap$deterministic_be) /
                      res$bootstrap$deterministic_be * 100, 1), "%"),
        format(round(res$bootstrap$sd), big.mark = ","),
        round(res$bootstrap$cv, 4),
        format(round(res$bootstrap$median), big.mark = ","),
        format(round(IQR(res$bootstrap$reserve_distribution, na.rm = TRUE)), big.mark = ","),
        format(round(min(res$bootstrap$reserve_distribution, na.rm = TRUE)), big.mark = ","),
        format(round(max(res$bootstrap$reserve_distribution, na.rm = TRUE)), big.mark = ",")
      )
    )
  }, striped = TRUE, hover = TRUE, width = "100%", align = "lr")
  
  # Risk metrics table
  output$risk_metrics_table <- renderTable({
    req(data_state$analysis_done)
    res <- data_state$analysis_results
    risk_data <- data.frame()
    for (cl_name in names(res$risk_metrics)) {
      cl <- res$risk_metrics[[cl_name]]
      risk_data <- rbind(risk_data, data.frame(
        Confidence = paste0(round(cl$confidence_level * 100), "%"),
        VaR = format(round(cl$var), big.mark = ","),
        TVaR = format(round(cl$tvar), big.mark = ","),
        `Risk Adj (TVaR)` = format(round(cl$risk_adjustment), big.mark = ","),
        `CTE Rate %` = paste(round(cl$cte_rate, 2), "%"),
        check.names = FALSE
      ))
    }
    risk_data
  }, striped = TRUE, hover = TRUE, width = "100%", align = "c")
  
  # Accident year analysis table
  output$ay_analysis_table <- renderTable({
    req(data_state$analysis_done)
    cl <- data_state$analysis_results$chain_ladder
    origin_periods <- if (!is.null(data_state$analysis_results$origin_labels)) {
      data_state$analysis_results$origin_labels
    } else {
      1:cl$n_years
    }
    data.frame(
      `Origin Period` = origin_periods,
      `Latest Observed` = format(round(cl$latest_observed), big.mark = ","),
      `Ultimate Claims` = format(round(cl$ultimate_claims), big.mark = ","),
      `Reserve` = format(round(cl$reserves_by_ay), big.mark = ","),
      `% of Total` = paste(round(cl$reserves_by_ay / cl$total_reserve * 100, 1), "%"),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, width = "100%", align = "c")
  
  # Development factors table
  output$dev_factors_table <- renderTable({
    req(data_state$analysis_done)
    res <- data_state$analysis_results
    factors <- res$chain_ladder$dev_factors
    tail_factor <- res$chain_ladder$tail_factor
    
    factor_data <- data.frame(
      `From-To` = paste0("Dev", 1:length(factors), "-Dev", 2:(length(factors)+1)),
      `Factor` = round(factors, 6)
    )
    
    if (tail_factor > 1.0) {
      factor_data <- rbind(factor_data, 
                           data.frame(`From-To` = "Tail Factor", 
                                      `Factor` = round(tail_factor, 6)))
    }
    
    factor_data
  }, striped = TRUE, hover = TRUE, width = "100%", align = "c")
  
  # Diagnostics content
  output$diagnostics_content <- renderUI({
    req(data_state$analysis_done)
    tagList(
      fluidRow(
        box(
          title = "Residual Analysis",
          status = "primary",
          solidHeader = TRUE,
          width = 6,
          tableOutput("diagnostics_residuals")
        ),
        box(
          title = "Residuals Distribution",
          status = "warning",
          solidHeader = TRUE,
          width = 6,
          plotOutput("residuals_plot")
        )
      ),
      fluidRow(
        box(
          title = "Bootstrap Convergence",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          tableOutput("convergence_table")
        )
      ),
      fluidRow(
        box(
          title = "QQ Plot of Residuals",
          status = "success",
          solidHeader = TRUE,
          width = 6,
          plotOutput("qq_plot")
        ),
        box(
          title = "Residuals vs Fitted Values",
          status = "danger",
          solidHeader = TRUE,
          width = 6,
          plotOutput("resid_vs_fitted")
        )
      )
    )
  })
  
  # Diagnostics residuals table
  output$diagnostics_residuals <- renderTable({
    req(data_state$analysis_done)
    diag <- data_state$analysis_results$diagnostics
    data.frame(
      Statistic = c(
        "Number of Residuals",
        "Mean",
        "Standard Deviation",
        "Skewness",
        "Kurtosis",
        "Shapiro-Wilk p-value",
        "Normality Conclusion"
      ),
      Value = c(
        diag$residual_count,
        round(diag$residual_mean, 4),
        round(diag$residual_sd, 4),
        round(diag$residual_skewness, 4),
        round(diag$residual_kurtosis, 4),
        round(diag$shapiro_pvalue, 6),
        if (isTRUE(diag$is_normal)) "Normal" else "Not Normal"
      )
    )
  }, striped = TRUE, hover = TRUE, width = "100%", align = "lr")
  
  # Residuals plot
  output$residuals_plot <- renderPlot({
    req(data_state$analysis_done)
    res <- data_state$analysis_results$bootstrap
    hist(res$residuals,
         breaks = 30,
         main = "Distribution of Pearson Residuals",
         xlab = "Residuals",
         col = "lightgreen",
         border = "white",
         probability = TRUE)
    x <- seq(min(res$residuals), max(res$residuals), length = 200)
    y <- dnorm(x, mean = mean(res$residuals), sd = sd(res$residuals))
    lines(x, y, col = "red", lwd = 2)
    legend("topright",
           legend = c("Residuals (hist)", "Normal curve"),
           col = c("lightgreen", "red"),
           lwd = c(NA, 2),
           fill = c("lightgreen", NA),
           border = c("white", NA))
  })
  
  # Convergence table
  output$convergence_table <- renderTable({
    req(data_state$analysis_done)
    diag <- data_state$analysis_results$diagnostics
    conv_data <- data.frame()
    for (n in names(diag$convergence)) {
      conv <- diag$convergence[[n]]
      conv_data <- rbind(conv_data, data.frame(
        Simulations = n,
        Mean = format(round(conv$mean), big.mark = ","),
        `Std Dev` = format(round(conv$sd), big.mark = ","),
        `CV` = round(conv$sd / conv$mean, 4),
        check.names = FALSE
      ))
    }
    conv_data
  }, striped = TRUE, hover = TRUE, width = "100%", align = "c")
  
  # QQ plot
  output$qq_plot <- renderPlot({
    req(data_state$analysis_done)
    res <- data_state$analysis_results$bootstrap
    qqnorm(res$residuals,
           main = "Q-Q Plot of Residuals",
           pch = 19,
           col = "blue",
           cex = 0.7)
    qqline(res$residuals, col = "red", lwd = 2)
  })
  
  # Residuals vs fitted (index proxy)
  output$resid_vs_fitted <- renderPlot({
    req(data_state$analysis_done)
    res <- data_state$analysis_results$bootstrap
    plot(res$residuals,
         main = "Residuals Plot",
         xlab = "Index",
         ylab = "Residuals",
         pch = 19,
         col = "darkblue",
         cex = 0.7)
    abline(h = 0, col = "red", lwd = 2, lty = 2)
    lines(lowess(res$residuals), col = "green", lwd = 2)
  })
}

# ============================================================================
# RUN APPLICATION
# ============================================================================

shinyApp(ui = ui, server = server)
