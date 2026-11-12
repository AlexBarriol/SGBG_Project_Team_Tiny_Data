# ============================================================
# Project 2 — R Shiny app
# 완성: Shoe Search, Explain PCA  (Our Shoes, Data 1, AI는 템플릿 그대로)
# ============================================================
library(shiny)
source("helpers.R")   # pca, get_scores(), rebuild(), search_top(), predict_cat(), img_to_vec()

# ---- 앱이 켜질 때 한 번만 준비하는 것 ----------------------------------
sc_labels <- read.csv("data/selfcheck/labels.csv")          # photo, category
cats      <- sort(unique(pca$meta$category))                 # Boots, Sandals, Shoes, Slippers
shoe_path <- function(id) sprintf("data/shoes/%04d.png", id) # id → 파일 경로
explain   <- readRDS("data/explain.rds")                     # prep.R Step 4 결과 (K별 정확도, confusion)
cat_cols  <- c(Boots = "#1b9e77", Sandals = "#d95f02", Shoes = "#7570b3", Slippers = "#e7298a")

# ---- Our Shoes: prep.R Step 5 결과 (사진이 아직 없으면 NULL) ------------
our <- if (file.exists("data/our_shoes.rds")) readRDS("data/our_shoes.rds") else NULL

# 우리 사진 i번 검색: 앞코 방향이 달라도 되게, 원래/좌우반전 중 더 가까운 쪽을 사용
our_search <- function(i, K) {
  v  <- our$vecs[[i]]
  vf <- mirror_vec(v)
  t1 <- search_top(v,  K)
  t2 <- search_top(vf, K)
  if (t2$distance[1] < t1$distance[1]) list(x = vf, top = t2, mirrored = TRUE)
  else                                 list(x = v,  top = t1, mirrored = FALSE)
}

# 길이 4096 벡터 → 64×64 흑백 그림 한 칸
show_img <- function(v, title = "") {
  plot(as.raster(matrix(v, 64, 64)))
  title(title, cex.main = 1.5, font.main = 1)
}

# 부호가 있는 값(x − x̄, eigenshoe) → 빨강(+) / 흰색(0) / 파랑(−) 그림
show_signed <- function(v, title = "", lim = max(abs(v))) {
  lim <- max(lim, 1e-12)
  pal <- colorRampPalette(c("#2166ac", "white", "#b2182b"))(101)
  idx <- round((pmin(pmax(v / lim, -1), 1) + 1) * 50) + 1
  plot(as.raster(matrix(pal[idx], 64, 64)))
  title(title, cex.main = 1.5, font.main = 1)
}

# ============================================================
# UI
# ============================================================
ui <- navbarPage("TODO: team name",
                 
                 # ---------------- Shoe Search ----------------
                 tabPanel("Shoe Search",
                          sidebarLayout(
                            sidebarPanel(width = 3,
                                         radioButtons("src", "Photo", c("Catalogue", "Self-check", "Upload"), inline = TRUE),
                                         
                                         # 고른 방식에 맞는 입력만 보이기
                                         conditionalPanel("input.src == 'Catalogue'",
                                                          numericInput("cat_id", "Catalogue id", 1, 1, 2000)),
                                         conditionalPanel("input.src == 'Self-check'",
                                                          selectInput("sc_file", "Self-check photo", sprintf("selfcheck_%02d.png", 1:4))),
                                         conditionalPanel("input.src == 'Upload'",
                                                          fileInput("photo", "Upload", accept = c(".jpg", ".jpeg", ".png")),
                                                          selectInput("up_cat", "True category (optional)", c("Unknown", cats))),
                                         
                                         sliderInput("K", "Number of PCs (K)", 1, 100, 30),
                                         helpText("K = 30 keeps about 80% of the variance and gave the best",
                                                  "leave-one-out accuracy (74%) in our test.")
                            ),
                            mainPanel(width = 9,
                                      h4("Best match"),
                                      plotOutput("best_plot", height = "240px"),
                                      uiOutput("prediction"),
                                      hr(),
                                      h4("Top 5"),
                                      plotOutput("top5_plot", height = "190px"),
                                      tableOutput("top5_table")
                            )
                          )
                 ),
                 
                 # ---------------- Explain PCA ----------------
                 tabPanel("Explain PCA",
                          p("For the photo chosen in Shoe Search, show how PCA finds the match."),
                          uiOutput("ex_header"),
                          
                          fluidRow(
                            column(4,
                                   h4("Step 1. Average shoe"), p("x̅, the mean of the 2,000 photos."),
                                   plotOutput("ex_avg", height = "260px")),
                            column(8,
                                   h4("Step 2. Photo − average"),
                                   p("x − x̅, where the photo is brighter (red) or darker (blue) than the average shoe."),
                                   plotOutput("ex_diff", height = "260px"))
                          ),
                          hr(),
                          h4("Step 3. Score map"),
                          p("All catalogue photos on PC1 and PC2, colored by category, with your photo marked.",
                            "The match uses all K PCs, so the best match can look farther away here than it really is."),
                          plotOutput("ex_map", height = "460px"),
                          hr(),
                          h4("Step 4. Eigenshoes"),
                          p("PC1–PC8 drawn as 64 × 64 images (red = positive loading, blue = negative), with the share of variance each explains."),
                          plotOutput("ex_eigen", height = "330px"),
                          hr(),
                          h4("Step 5. Scores"),
                          p("The photo’s first K PC scores, the numbers used to find the match, next to the best match’s scores."),
                          plotOutput("ex_scores", height = "300px"),
                          verbatimTextOutput("ex_scores_txt"),
                          hr(),
                          h4("Step 6. Rebuild"),
                          p("The photo rebuilt from the average shoe and the first K eigenshoes, for several K."),
                          plotOutput("ex_rebuild", height = "230px"),
                          hr(),
                          h4("More: how many PCs, and which shoes are hard?"),
                          p("Leave-one-out test: each of the 2,000 photos is searched against the other 1,999."),
                          fluidRow(
                            column(7, plotOutput("ex_acc", height = "330px")),
                            column(5, h5("Confusion matrix at K = 30"), tableOutput("ex_conf")))
                 ),
                 
                 # ---------------- 나머지 탭: 템플릿 그대로 (다음 단계에서 채움) ----------------
                 # ---------------- Our Shoes ----------------
                 tabPanel("Our Shoes",
                          p("Can our app recognize real shoes it has never seen? Each member photographed 3 of their own shoes."),
                          if (is.null(our)) {
                            div(class = "alert alert-warning",
                                "No photos yet: put Name_Category_N.jpg files in data/our_shoes and run prep.R Step 5.")
                          } else tagList(
                            h4("All our shoes"),
                            plotOutput("our_gallery", height = paste0(ceiling(nrow(our$info) / 6) * 210, "px")),
                            hr(),
                            sidebarLayout(
                              sidebarPanel(width = 3,
                                           selectInput("our_pick", "Choose a shoe",
                                                       setNames(seq_len(nrow(our$info)), our$info$label)),
                                           sliderInput("our_K", "Number of PCs (K)", 1, 100, 30),
                                           helpText("Each photo is turned grayscale, brightened so the background is white,",
                                                    "cropped to the shoe and shrunk to 64 × 64. If the toe points the other way,",
                                                    "the mirrored photo is used.")
                              ),
                              mainPanel(width = 9,
                                        h4("Best match"),
                                        plotOutput("our_best", height = "240px"),
                                        uiOutput("our_pred"),
                                        hr(),
                                        h4("Top 5"),
                                        plotOutput("our_top5", height = "190px"),
                                        tableOutput("our_top5_table")
                              )
                            ),
                            hr(),
                            h4("How many of our shoes did the app get right?"),
                            uiOutput("our_acc"),
                            tableOutput("our_all")
                          )
                 ),
                 tabPanel("Data 1",
                          p("One Project 1 dataset, descriptive results only (filter, sort, count, mean, median, rate, group summary)."),
                          p("It should include:"),
                          tags$ul(
                            tags$li("the question you are trying to answer;"),
                            tags$li("the inputs that show it (e.g., the filter, grouping or time range you choose);"),
                            tags$li("the result in numbers;"),
                            tags$li("why it matters;"),
                            tags$li("how this finding helps your next analysis of this dataset."))
                 ),
                 tabPanel("AI",
                          h4("Ethics statement"),
                          p("We understand the rules on data and AI use for this project, including following the license and privacy rules",
                            "of every dataset we used, understanding all of our code, and knowing that presenting AI output as our own work may be",
                            "academic misconduct. We certify that we have followed all of them. Any violation is our responsibility,",
                            "and we agree to receive an F in this course if we have not."),
                          p("Signed: TODO (every member)"),
                          h4("What AI did and what we did"),
                          tableOutput("ai_table"),
                          h4("Reflection"),
                          tableOutput("ai_reflection")
                 )
)

# ============================================================
# Server
# ============================================================
server <- function(input, output, session) {
  
  # ---- ① 고른 사진을 준비: 벡터 x, 이름, 정답, 검색에서 뺄 id ----------
  query <- reactive({
    if (input$src == "Catalogue") {
      id <- input$cat_id
      validate(need(!is.na(id) && id %in% pca$meta$id, "Enter a catalogue id from 1 to 2000."))
      list(x       = img_to_vec(shoe_path(id)),
           name    = paste("Catalogue id", id),
           truth   = pca$meta$category[pca$meta$id == id],
           exclude = id)                                    # 자기 자신은 검색에서 제외
    } else if (input$src == "Self-check") {
      list(x       = img_to_vec(file.path("data/selfcheck", input$sc_file)),
           name    = input$sc_file,
           truth   = sc_labels$category[sc_labels$photo == input$sc_file],
           exclude = NULL)
    } else {
      validate(need(input$photo, "Upload a JPG or PNG photo of a shoe."))
      list(x       = img_to_vec(input$photo$datapath),
           name    = input$photo$name,
           truth   = if (input$up_cat == "Unknown") NA else input$up_cat,
           exclude = NULL)
    }
  })
  
  # ---- ② 검색: Top 5와 예측 (여러 출력이 함께 씀) ----------------------
  result <- reactive({
    q   <- query()
    top <- search_top(q$x, input$K, n = 5, exclude_id = q$exclude)
    list(q = q, top = top, pred = predict_cat(top))
  })
  
  # ---- ③ Best match: 내 사진 · 가장 가까운 사진 · 평균 신발 · K개로 복원 ----
  output$best_plot <- renderPlot({
    r    <- result()
    best <- r$top$id[1]
    par(mfrow = c(1, 4), mar = c(0.5, 0.5, 2.5, 0.5))
    show_img(r$q$x, paste("Your photo:", r$q$name))
    show_img(img_to_vec(shoe_path(best)),
             sprintf("Best match: id %d (%s)", best, r$top$category[1]))
    show_img(pca$center, "Average shoe")
    show_img(rebuild(r$q$x, input$K), paste0("Your photo rebuilt, K = ", input$K))
  })
  
  # ---- ④ 예측 + 맞았는지 ------------------------------------------------
  output$prediction <- renderUI({
    r     <- result()
    truth <- r$q$truth
    verdict <-
      if (length(truth) == 0 || is.na(truth)) span("(true category unknown)", style = "color:#6c7a89;")
    else if (r$pred == truth) span("✓ Correct", style = "color:#1f7a3a; font-weight:700;")
    else span(paste0("✗ Wrong (true: ", truth, ")"), style = "color:#c0392b; font-weight:700;")
    votes <- table(r$top$category)
    div(style = "font-size:1.25em; margin:10px 0;",
        strong("Prediction: "), r$pred, " — ", verdict,
        div(style = "font-size:0.75em; color:#6c7a89;",
            "Votes in top 5: ", paste(names(votes), votes, sep = " ", collapse = " · ")))
  })
  
  # ---- ⑤ Top 5 사진 -----------------------------------------------------
  output$top5_plot <- renderPlot({
    top <- result()$top
    par(mfrow = c(1, 5), mar = c(0.5, 0.5, 2.5, 0.5))
    for (i in seq_len(nrow(top))) {
      show_img(img_to_vec(shoe_path(top$id[i])),
               sprintf("#%d  id %d\n%s", i, top$id[i], top$category[i]))
    }
  })
  
  # ---- ⑥ Top 5 표 ---------------------------------------------------------
  output$top5_table <- renderTable({
    top <- result()$top
    data.frame(Rank = seq_len(nrow(top)), id = top$id,
               category = top$category, distance = round(top$distance, 3))
  }, digits = 3)
  
  # ======================= Explain PCA =======================
  # Shoe Search의 result()를 그대로 다시 씀 → 같은 사진, 같은 K
  
  output$ex_header <- renderUI({
    r <- result()
    div(style = "background:#eef2fc; padding:8px 12px; border-radius:6px; margin-bottom:8px;",
        strong("Explaining: "), r$q$name, " · K = ", input$K,
        " · best match id ", r$top$id[1], " (", r$top$category[1], ")",
        " · prediction ", r$pred)
  })
  
  # Step 1
  output$ex_avg <- renderPlot({
    par(mar = c(0.5, 0.5, 2.5, 0.5))
    show_img(pca$center, "Average shoe x̄")
  })
  
  # Step 2
  output$ex_diff <- renderPlot({
    x <- result()$q$x
    par(mfrow = c(1, 3), mar = c(0.5, 0.5, 2.5, 0.5))
    show_img(x, "Your photo x")
    show_img(pca$center, "Average x̄")
    show_signed(x - pca$center, "x − x̄", lim = 1)
  })
  
  # Step 3
  output$ex_map <- renderPlot({
    r  <- result()
    S  <- pca$scores
    q  <- get_scores(r$q$x, 2)
    b  <- which(pca$meta$id == r$top$id[1])
    par(mar = c(4.5, 4.5, 1, 1))
    plot(S[, 1], S[, 2], pch = 16, cex = 0.7,
         col = adjustcolor(cat_cols[pca$meta$category], 0.45),
         xlab = sprintf("PC1 (%.1f%% of variance)", 100 * pca$var_exp[1]),
         ylab = sprintf("PC2 (%.1f%% of variance)", 100 * pca$var_exp[2]))
    points(S[b, 1], S[b, 2], pch = 21, bg = "gold", cex = 2.2, lwd = 1.5)
    points(q[1], q[2], pch = 8, cex = 3, lwd = 3)
    legend("topright", bty = "n",
           legend = c(names(cat_cols), "Your photo", "Best match"),
           pch = c(rep(16, 4), 8, 21), col = c(cat_cols, "black", "black"),
           pt.bg = c(rep(NA, 4), NA, "gold"), pt.cex = c(rep(1.2, 4), 2, 1.8))
  })
  
  # Step 4
  output$ex_eigen <- renderPlot({
    par(mfrow = c(2, 4), mar = c(0.5, 0.5, 2.5, 0.5))
    for (j in 1:8) {
      show_signed(pca$rotation[, j],
                  sprintf("PC%d · %.1f%%", j, 100 * pca$var_exp[j]))
    }
  })
  
  # Step 5
  output$ex_scores <- renderPlot({
    r  <- result(); K <- input$K
    sq <- get_scores(r$q$x, K)
    sb <- pca$scores[which(pca$meta$id == r$top$id[1]), 1:K]
    k  <- min(K, 20)                       # 그래프는 앞 20개까지만
    par(mar = c(4.5, 4.5, 3, 1))
    barplot(rbind(sq[1:k], sb[1:k]), beside = TRUE, las = 2,
            names.arg = paste0("PC", 1:k), col = c("#2d5bd7", "#f2b134"), border = NA,
            ylab = "score", legend.text = c("Your photo", "Best match"),
            args.legend = list(x = "topright", bty = "n"))
    abline(h = 0, col = "grey60")
    title(sprintf("Distance using all K = %d scores: %.3f%s", K, r$top$distance[1],
                  if (K > 20) "  (first 20 shown)" else ""), font.main = 1)
  })
  
  output$ex_scores_txt <- renderPrint({
    sq <- get_scores(result()$q$x, input$K)
    names(sq) <- paste0("PC", seq_along(sq))
    round(sq, 3)
  })
  
  # Step 6
  output$ex_rebuild <- renderPlot({
    x  <- result()$q$x
    Ks <- sort(unique(c(1, 5, 10, input$K, 50, 100)))
    par(mfrow = c(1, length(Ks) + 1), mar = c(0.5, 0.5, 2.5, 0.5))
    show_img(x, "Original")
    for (k in Ks) show_img(rebuild(x, k), paste0("K = ", k, if (k == input$K) " ◀" else ""))
  })
  
  # More: K에 따른 정확도와 누적 variance
  output$ex_acc <- renderPlot({
    a  <- explain$acc_by_K
    cv <- cumsum(pca$var_exp)[a$K]
    par(mar = c(4.5, 4.5, 1, 1))
    plot(a$K, 100 * a$accuracy, type = "b", pch = 16, col = "#2d5bd7", lwd = 2,
         ylim = c(0, 100), log = "x", xlab = "Number of PCs K (log scale)", ylab = "%")
    lines(a$K, 100 * cv, type = "b", pch = 17, col = "#6c7a89", lwd = 2, lty = 2)
    abline(v = input$K, col = "#c0392b", lty = 3)
    abline(h = 25, col = "grey70", lty = 3)
    legend("bottomright", bty = "n",
           legend = c("Leave-one-out accuracy", "Cumulative variance explained",
                      "Your K", "Random guess (25%)"),
           col = c("#2d5bd7", "#6c7a89", "#c0392b", "grey70"),
           lty = c(1, 2, 3, 3), pch = c(16, 17, NA, NA), lwd = c(2, 2, 1, 1))
  })
  
  output$ex_conf <- renderTable({
    m <- unclass(explain$conf30)
    data.frame(True = rownames(m), m, Accuracy = sprintf("%.0f%%", 100 * diag(m) / rowSums(m)),
               check.names = FALSE)
  }, align = "l")
  
  
  # ======================= Our Shoes =======================
  if (!is.null(our)) {
    
    # 전체 사진 갤러리 (컬러, 이름 · 카테고리)
    output$our_gallery <- renderPlot({
      n <- nrow(our$info)
      par(mfrow = c(ceiling(n / 6), 6), mar = c(0.5, 0.5, 2.5, 0.5))
      for (i in seq_len(n)) {
        plot(as.raster(our$thumbs[[i]]))
        title(our$info$label[i], cex.main = 1.3, font.main = 1)
      }
      for (j in seq_len(ceiling(n / 6) * 6 - n)) plot.new()   # 남는 칸 비우기
    })
    
    # 고른 사진 하나 검색
    our_res <- reactive({
      i <- as.integer(input$our_pick)
      s <- our_search(i, input$our_K)
      c(s, list(i = i, truth = our$info$category[i], pred = predict_cat(s$top)))
    })
    
    output$our_best <- renderPlot({
      r <- our_res(); best <- r$top$id[1]
      par(mfrow = c(1, 4), mar = c(0.5, 0.5, 2.5, 0.5))
      plot(as.raster(our$thumbs[[r$i]])); title("Our photo", cex.main = 1.5, font.main = 1)
      show_img(r$x, paste0("What the app sees", if (r$mirrored) " (mirrored)" else ""))
      show_img(img_to_vec(shoe_path(best)), sprintf("Best match: id %d (%s)", best, r$top$category[1]))
      show_img(rebuild(r$x, input$our_K), paste0("Rebuilt, K = ", input$our_K))
    })
    
    output$our_pred <- renderUI({
      r <- our_res()
      verdict <- if (r$pred == r$truth) span("✓ Correct", style = "color:#1f7a3a; font-weight:700;")
      else span(paste0("✗ Wrong (true: ", r$truth, ")"), style = "color:#c0392b; font-weight:700;")
      votes <- table(r$top$category)
      div(style = "font-size:1.25em; margin:10px 0;",
          strong("Prediction: "), r$pred, " — ", verdict,
          div(style = "font-size:0.75em; color:#6c7a89;",
              "Votes in top 5: ", paste(names(votes), votes, sep = " ", collapse = " · ")))
    })
    
    output$our_top5 <- renderPlot({
      top <- our_res()$top
      par(mfrow = c(1, 5), mar = c(0.5, 0.5, 2.5, 0.5))
      for (i in seq_len(nrow(top))) {
        show_img(img_to_vec(shoe_path(top$id[i])),
                 sprintf("#%d  id %d\n%s", i, top$id[i], top$category[i]))
      }
    })
    
    output$our_top5_table <- renderTable({
      top <- our_res()$top
      data.frame(Rank = seq_len(nrow(top)), id = top$id,
                 category = top$category, distance = round(top$distance, 3))
    }, digits = 3)
    
    # 우리 사진 전체 결과 (같은 K로)
    our_all <- reactive({
      n <- nrow(our$info)
      pred <- sapply(seq_len(n), function(i) predict_cat(our_search(i, input$our_K)$top))
      data.frame(Photo = our$info$label, True = our$info$category, Predicted = pred,
                 Result = ifelse(pred == our$info$category, "Correct", "Wrong"))
    })
    
    output$our_acc <- renderUI({
      d <- our_all(); k <- sum(d$Result == "Correct")
      div(style = "font-size:1.2em; margin:6px 0;",
          strong(sprintf("%d of %d correct (%.0f%%)", k, nrow(d), 100 * k / nrow(d))),
          span(sprintf(" with K = %d", input$our_K), style = "color:#6c7a89;"))
    })
    
    output$our_all <- renderTable(our_all())
  }
  
  
  
  # ---- AI 탭: 템플릿 그대로 (열 유지) -------------------------------------
  output$ai_table <- renderTable(data.frame(
    Tab           = c("Shoe Search", "Explain PCA", "Our Shoes", "Data 1"),
    `What AI did` = c("TODO", "TODO", "TODO", "TODO"),
    `What we did` = c("TODO", "TODO", "TODO", "TODO"),
    check.names = FALSE))
  output$ai_reflection <- renderTable(data.frame(
    Member     = c("TODO", "TODO", "TODO", "TODO", "TODO"),
    Reflection = c("TODO: one or two paragraphs: what AI helped with, where it failed, what you learned.",
                   "TODO", "TODO", "TODO", "TODO")))
}

shinyApp(ui, server)