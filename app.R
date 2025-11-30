# app.R
# Compound Poisson-Gamma Shiny App
# Single-window, light-pink theme, draggable parameters icon
# Clean deployment-ready (no tokens, no installs)

# Check and load required packages (fail early with clear message)
required_pkgs <- c("shiny", "shinyWidgets", "shinyjqui", "ggplot2")
missing <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  stop("Missing required packages: ", paste(missing, collapse = ", "), ". Please install them before running the app.")
}

# Now load (attach) the packages
lapply(required_pkgs, function(p) library(p, character.only = TRUE))

# CSS for the single-window layout and styling
app_css <- "
html, body { height:100%; margin:0; padding:0; overflow:hidden; font-family: 'Inter', system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', Arial; }
#app-container { background: #FFE6EC; height:100vh; width:100vw; box-sizing:border-box; padding:18px; }
.card { background: white; border-radius:10px; box-shadow: 0 6px 18px rgba(0,0,0,0.08); padding:12px; }
#context-card { color: #3B1F2B; max-width: 420px; }
#controls-panel { position: absolute; width:320px; top: 18px; left: 18px; z-index: 200; }
#main-area { position: absolute; top: 18px; right: 18px; left: 360px; bottom: 18px; }
#plot-area { height: calc(100% - 140px); }
#summary-box { height:120px; overflow:auto; }
/* Draggable icon */
#drag-icon { position:absolute; width:56px; height:56px; border-radius:28px; background:white; display:flex; align-items:center; justify-content:center; box-shadow:0 6px 18px rgba(0,0,0,0.12); cursor:grab; }
#drag-icon:active { cursor:grabbing; }
.btn-small { padding:6px 10px; border-radius:8px; }
.downloads { display:flex; gap:8px; }
#interpretation { margin-top:10px; }
@media (max-width:1100px) {
  #controls-panel{ position:absolute; left: 12px; top: 12px; width:300px; }
  #main-area { left: 340px; }
}
"

ui <- fluidPage(
  tags$head(
    tags$style(HTML(app_css)),
    # ensure tiny script is loaded (will be written to tempdir in onStart)
    tags$script(src = "app_js/app_bindings.js")
  ),
  div(id = "app-container",
      # Draggable icon (starts at top-left corner)
      jqui_draggable(div(id = "drag-icon", class = "card",
                         title = "Drag to move. Click to toggle controls",
                         span(icon("sliders"), style = "font-size:20px; color:#3B1F2B")
      ), options = list(containment = "parent", snap = TRUE, snapMode = "inner", snapTolerance = 20)),

      # Controls panel (initially visible)
      div(id = "controls-panel", class = "card",
          h4("Parameters", style = "margin-top:4px; margin-bottom:8px; color:#3B1F2B;"),
          # lambda
          numericInput("lambda_in", "λ (lambda) — arrival rate", value = 0.5, min = 0.01, max = 5, step = 0.01),
          sliderInput("lambda", NULL, min = 0.01, max = 5, value = 0.5, step = 0.01),

          # mu
          numericInput("mu_in", "μ (mu) — exp. rate for Xᵢ", value = 1, min = 0.01, max = 5, step = 0.01),
          sliderInput("mu", NULL, min = 0.01, max = 5, value = 1, step = 0.01),

          # t
          numericInput("t_in", "t — time horizon", value = 100, min = 1, max = 10000, step = 1),
          sliderInput("t", NULL, min = 1, max = 10000, value = 100, step = 1),

          # number of simulations and seed
          numericInput("nsim", "Number of simulations", value = 2000, min = 1, max = 200000, step = 1),
          numericInput("seed", "Random seed (optional)", value = NA, min = 1, max = 1e9, step = 1),

          # Buttons
          actionButton("run", "Run", class = "btn-primary btn-small"),
          actionButton("reset", "Reset", class = "btn-default btn-small"),
          tags$hr(),
          span("Tip: Drag the ⚙ icon to move controls. Click it to show/hide.", style = "font-size:12px; color:#333;")
      ),

      # Main area: plot + summary + interpretation
      div(id = "main-area",
          div(id = "context-card", class = "card",
              h3("Compound Poisson – Gamma Explorer", style = "margin:6px 0; color:#3B1F2B;"),
              em("Interactive simulator and visualiser for a compound Poisson process."),
              p("N(t) ~ Poisson(λt); claim sizes X_i ~ Exponential(μ). Study how λ, μ, and t affect aggregate S(t).", style = "margin-top:6px; font-size:14px; color:#3B1F2B;" )
          ),

          div(id = "plot-area", class = "card", style = "margin-top:10px; padding:8px; height:60%;",
              plotOutput("histPlot", height = "100%"),
              div(style = "display:flex; justify-content:space-between; align-items:center; margin-top:8px;",
                  div(class = "downloads",
                      downloadButton("download_data", "Download CSV", class = "btn-small"),
                      downloadButton("download_plot", "Export PNG", class = "btn-small")
                  ),
                  span(textOutput("runInfo"))
              )
          ),

          div(class = "card", id = "summary-box", style = "margin-top:10px; padding:10px; display:flex; gap:12px;",
              div(style = "flex:1;",
                  h4("Summary stats", style = "margin:0 0 8px 0; color:#3B1F2B;"),
                  tableOutput("summaryTable")
              ),
              div(style = "width:320px;", h4("Interpretation", style = "margin:0 0 8px 0; color:#3B1F2B;"),
                  uiOutput("interpretation")
              )
          )
      )
  )
)

server <- function(input, output, session) {
  # Keep numericInput & slider synced
  observeEvent(input$lambda_in, { updateSliderInput(session, "lambda", value = input$lambda_in) })
  observeEvent(input$lambda, { updateNumericInput(session, "lambda_in", value = input$lambda) })
  observeEvent(input$mu_in, { updateSliderInput(session, "mu", value = input$mu_in) })
  observeEvent(input$mu, { updateNumericInput(session, "mu_in", value = input$mu) })
  observeEvent(input$t_in, { updateSliderInput(session, "t", value = input$t_in) })
  observeEvent(input$t, { updateNumericInput(session, "t_in", value = input$t) })

  # Reset button
  observeEvent(input$reset, {
    updateNumericInput(session, "lambda_in", value = 0.5)
    updateSliderInput(session, "lambda", value = 0.5)
    updateNumericInput(session, "mu_in", value = 1)
    updateSliderInput(session, "mu", value = 1)
    updateNumericInput(session, "t_in", value = 100)
    updateSliderInput(session, "t", value = 100)
    updateNumericInput(session, "nsim", value = 2000)
    updateNumericInput(session, "seed", value = NA)
  })

  # Reactive values to store simulation results
  sim_data <- reactiveVal(NULL)
  last_run_meta <- reactiveVal(list())

  # Run simulations when Run button clicked
  observeEvent(input$run, {
    nsim <- as.integer(input$nsim)
    if (is.na(nsim) || nsim < 1) {
      showNotification("Number of simulations must be a positive integer", type = "error")
      return()
    }

    # Warn if too large
    if (nsim > 100000) {
      showModal(modalDialog(
        title = "Large simulation",
        paste0("You requested ", nsim, " simulations. This may take a while and use a lot of memory. Continue?"),
        footer = tagList(modalButton("Cancel"), actionButton("confirmRun", "Continue"))
      ))
      # wait for confirm
      observeEvent(input$confirmRun, {
        removeModal()
        run_sim()
      }, once = TRUE)
    } else {
      run_sim()
    }
  })

  run_sim <- function() {
    nsim <- as.integer(input$nsim)
    lambda <- input$lambda
    mu <- input$mu
    tval <- input$t
    seed <- input$seed

    if (!is.na(seed) && nzchar(as.character(seed))) set.seed(seed)

    # Simulate with progress
    withProgress(message = "Simulating", value = 0, {
      # We'll simulate in batches to show progress so big nsim doesn't block UI fully
      batch_size <- ifelse(nsim <= 5000, nsim, 2000)
      n_batches <- ceiling(nsim / batch_size)

      res <- numeric(nsim)
      idx <- 1
      for (b in seq_len(n_batches)) {
        start <- idx
        end <- min(nsim, idx + batch_size - 1)
        cur_n <- end - start + 1;

        # For each replicate, generate N ~ Poisson(lambda * t), then sum of N exponentials
        Ns <- rpois(cur_n, lambda * tval)
        # For Ns==0, sum is 0
        sums <- numeric(cur_n)
        has_pos <- which(Ns > 0);
        if (length(has_pos) > 0) {
          # generate using rgamma: sum of Ns exponentials ~ Gamma(shape=Ns, rate=mu)
          sums[has_pos] <- rgamma(length(has_pos), shape = Ns[has_pos], rate = mu);
        }
        res[start:end] <- sums;

        idx <- end + 1;
        incProgress(1 / n_batches);
      }

      sim_data(data.frame(S = res))
      last_run_meta(list(lambda = lambda, mu = mu, t = tval, nsim = nsim))
    })
  }

  # Plot
  output$histPlot <- renderPlot({
    df <- sim_data();
    if (is.null(df)) {
      ggplot() + theme_void() + annotate("text", x = 0, y = 0, label = "Run the simulation to see the histogram", size = 6) + xlim(-1,1);
    } else {
      # plot histogram, KDE and fitted Gamma when possible
      gg <- ggplot(df, aes(x = S)) +
        geom_histogram(aes(y = ..density..), bins = 60, fill = "#F6C6D1", color = "#C75482", alpha = 0.9) +
        geom_density(size = 1);

      # Fit Gamma to positive S values (exclude zeros for fitting)
      pos <- df$S[df$S > 0];
      if (length(pos) >= 20) {
        fit_shape <- tryCatch({
          m <- mean(pos); v <- var(pos); shape <- m^2 / v; rate <- m / v; list(shape = shape, rate = rate)
        }, error = function(e) NULL);
        if (!is.null(fit_shape)) {
          gg <- gg + stat_function(fun = function(x) dgamma(x, shape = fit_shape$shape, rate = fit_shape$rate), size = 1, linetype = "dashed", color = "#2F2B3A");
        }
      }
      gg + labs(title = paste0("Histogram of S(t) — last run: nsim=", last_run_meta()$nsim), x = "S(t)", y = "Density") + theme_minimal() + theme(plot.title = element_text(color = "#3B1F2B", family = "Inter", size = 14));
    }
  });

  # Summary table
  output$summaryTable <- renderTable({
    df <- sim_data();
    if (is.null(df)) return(NULL);
    x <- df$S;
    vals <- c(mean = mean(x), var = var(x), median = median(x), q2.5 = quantile(x, 0.025), q97.5 = quantile(x, 0.975));
    E_Nt <- input$lambda * input$t;
    E_X <- 1 / input$mu;
    expected_St <- E_Nt * E_X;
    out <- data.frame(Statistic = c("Mean", "Variance", "Median", "2.5%", "97.5%", "E[N(t)]", "E[S(t)]"), Value = round(c(vals["mean"], vals["var"], vals["median"], vals["q2.5"], vals["q97.5"], E_Nt, expected_St), 6));
    out;
  });

  # run info
  output$runInfo <- renderText({
    meta <- last_run_meta();
    if (length(meta) == 0) return("");
    paste0("λ=", meta$lambda, "  μ=", meta$mu, "  t=", meta$t);
  });

  # Interpretation
  output$interpretation <- renderUI({
    meta <- last_run_meta();
    df <- sim_data();
    if (length(meta) == 0 || is.null(df)) {
      return(tags$div("No run yet. Change parameters and click Run."));
    }
    # Basic heuristics to describe impact
    lambda <- meta$lambda; mu <- meta$mu; tval <- meta$t;
    E_Nt <- lambda * tval; E_X <- 1 / mu;
    mean_s <- mean(df$S);
    bullets <- c(
      paste0("Expected arrivals E[N(t)] = ", round(E_Nt, 3), "."),
      paste0("Expected claim size E[X] = ", round(E_X, 4), "."),
      paste0("Empirical mean of S(t) ≈ ", round(mean_s, 4), "."),
      if (E_Nt > 50) "Distribution is likely smooth and approximately normal (by CLT)." else "Distribution may be skewed; many zeros or a heavy right tail are possible. "
    );
    tags$ul(lapply(bullets, tags$li));
  });

  # Downloads
  output$download_data <- downloadHandler(
    filename = function() paste0("sim_data_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- sim_data();
      if (is.null(df)) {
        write.csv(data.frame(), file, row.names = FALSE);
      } else {
        write.csv(df, file, row.names = FALSE);
      }
    }
  );

  output$download_plot <- downloadHandler(
    filename = function() paste0("sim_plot_", Sys.Date(), ".png"),
    content = function(file) {
      png(file, width = 1200, height = 800);
      print(isolate({
        df <- sim_data();
        if (is.null(df)) {
          plot.new(); text(0.5,0.5,"Run the simulation to see the plot");
        } else {
          ggplot(df, aes(x = S)) + geom_histogram(aes(y = ..density..), bins = 60, fill = "#F6C6D1", color = "#C75482", alpha = 0.9) + geom_density() + labs(x = "S(t)", y = "Density") + theme_minimal() + theme(plot.title = element_text(color = "#3B1F2B"));
        }
      }));
      dev.off();
    }
  );

  # small client-side toggle is handled by injected JS file
}

# client JS (written to tempdir in onStart and served via addResourcePath)
js_code <- "
document.addEventListener('DOMContentLoaded', function() {
  var icon = document.getElementById('drag-icon');
  if (!icon) return;
  icon.addEventListener('click', function(e){
    var panel = document.getElementById('controls-panel');
    if (!panel) return;
    if (panel.style.display === 'none' || panel.style.visibility === 'hidden') {
      panel.style.display = 'block'; panel.style.visibility = 'visible';
    } else {
      panel.style.display = 'none'; panel.style.visibility = 'hidden';
    }
  });
});
"

onStart <- function() {
  tmp <- tempdir();
  dir.create(file.path(tmp, "app_js"), showWarnings = FALSE);
  writeLines(js_code, file.path(tmp, "app_bindings.js"));
  shiny::addResourcePath('app_js', tmp);
}

shinyApp(ui = ui, server = server, onStart = onStart)
