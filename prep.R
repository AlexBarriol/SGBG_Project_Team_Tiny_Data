# ===== prep.R : Step 1. 사진 → 행렬 =====
library(png)

meta  <- read.csv("data/shoes_meta.csv")              # id, category
files <- sprintf("data/shoes/%04d.png", meta$id)      # id 1 → "data/shoes/0001.png"

# 사진 한 장을 읽어 64×64 흑백 행렬로 돌려주는 함수
read_gray <- function(f) {
  a <- readPNG(f)                      # 0~1 사이 숫자
  if (length(dim(a)) == 3) a <- a[, , 1]   # 채널이 여러 개면 첫 번째(밝기)만 사용
  a
}

# 2,000장을 읽어서 한 줄씩 펼친 뒤 쌓기
X <- t(sapply(files, function(f) as.vector(read_gray(f))))
rownames(X) <- NULL

dim(X)        # 2000 4096 이 나와야 함
range(X)      # 0 ~ 1 사이여야 함
table(meta$category)

# 확인: 첫 번째 신발을 다시 64×64로 접어서 그려보기
plot(as.raster(matrix(X[1, ], 64, 64)))

saveRDS(list(X = X, meta = meta), "data/shoes_matrix.rds")













# ===== prep.R : Step 2. PCA 미리 계산 =====
dat  <- readRDS("data/shoes_matrix.rds")
X    <- dat$X
meta <- dat$meta

# PCA (픽셀이 모두 같은 단위라 scale. = FALSE)
system.time(
  pca <- prcomp(X, center = TRUE, scale. = FALSE)
)

# 각 PC가 설명하는 variance 비율
var_exp <- pca$sdev^2 / sum(pca$sdev^2)
cum_exp <- cumsum(var_exp)
round(cum_exp[c(1, 2, 5, 10, 20, 50, 100)], 3)   # 누적 설명 비율
which(cum_exp >= 0.80)[1]                        # 80%를 넘기는 PC 개수
which(cum_exp >= 0.90)[1]                        # 90%를 넘기는 PC 개수

# 확인 1: average shoe
plot(as.raster(matrix(pca$center, 64, 64)))

# 확인 2: PC1 eigenshoe (음수도 있어서 0~1로 늘려서 그림)
to01 <- function(v) (v - min(v)) / (max(v) - min(v))
plot(as.raster(matrix(to01(pca$rotation[, 1]), 64, 64)))

# 앱에 필요한 것만 골라 저장 (PC 100개까지)
K_MAX <- 100
pca_small <- list(
  center   = pca$center,                 # average shoe (길이 4096)
  rotation = pca$rotation[, 1:K_MAX],    # eigenshoes (4096 × 100)
  scores   = pca$x[, 1:K_MAX],           # 카탈로그 score (2000 × 100)
  var_exp  = var_exp,                    # 모든 PC의 설명 비율
  meta     = meta                        # id, category
)
saveRDS(pca_small, "data/pca.rds")
file.size("data/pca.rds") / 1e6          # 파일 크기 (MB)











# ===== Step 3. 테스트 =====
source("helpers.R")
dat <- readRDS("data/shoes_matrix.rds")

# (a) 검산: 함수로 구한 score = prcomp가 준 score 인가?
all.equal(get_scores(dat$X[1, ], 5), unname(pca$scores[1, 1:5]))   # TRUE면 OK

# (b) 카탈로그 id 1번으로 검색 (자기 자신은 제외)
top <- search_top(dat$X[1, ], K = 30, exclude_id = 1)
top
predict_cat(top); dat$meta$category[1]

# (c) K별 정확도: 2,000장 모두를 하나씩 "처음 보는 사진"처럼 검색
loo_acc <- function(K) {
  D <- as.matrix(dist(pca$scores[, 1:K]))   # 2000 × 2000 거리표
  diag(D) <- Inf                            # 자기 자신 제외
  pred <- apply(D, 1, function(d) {
    top <- data.frame(category = pca$meta$category[order(d)[1:5]])
    predict_cat(top)
  })
  mean(pred == pca$meta$category)
}
Ks  <- c(5, 10, 20, 30, 50, 100)
acc <- sapply(Ks, loo_acc)
data.frame(K = Ks, accuracy = round(acc, 3))









# ===== Step 3-2. selfcheck 테스트 =====
source("helpers.R")
lab <- read.csv("data/selfcheck/labels.csv")

for (i in seq_len(nrow(lab))) {
  x   <- img_to_vec(file.path("data/selfcheck", lab$photo[i]))
  top <- search_top(x, K = 30)                       # selfcheck는 카탈로그에 없으니 제외할 것 없음
  cat("\n", lab$photo[i], "| 정답:", lab$category[i],
      "| 예측:", predict_cat(top), "\n")
  print(top)
}

# 확인: selfcheck 1번 사진이 제대로 읽혔는지 그림으로
plot(as.raster(matrix(img_to_vec("data/selfcheck/selfcheck_01.png"), 64, 64)))







# ===== prep.R : Step 4. Explain PCA용 결과 미리 계산 =====
source("helpers.R")

# K별 leave-one-out 예측 (2,000장 각각을 나머지 1,999장에서 검색)
loo_pred <- function(K) {
  D <- as.matrix(dist(pca$scores[, 1:K]))   # 2000 × 2000 거리표
  diag(D) <- Inf                            # 자기 자신 제외
  apply(D, 1, function(d) predict_cat(data.frame(category = pca$meta$category[order(d)[1:5]])))
}

Ks    <- c(1, 2, 3, 5, 10, 15, 20, 30, 40, 50, 70, 100)
preds <- lapply(Ks, loo_pred)
acc_by_K <- data.frame(K = Ks,
                       accuracy = sapply(preds, function(p) mean(p == pca$meta$category)))

# K = 30일 때 무엇을 무엇으로 예측했나 (confusion matrix)
conf30 <- table(True = pca$meta$category, Predicted = preds[[which(Ks == 30)]])

saveRDS(list(acc_by_K = acc_by_K, conf30 = conf30), "data/explain.rds")
acc_by_K
conf30






# ===== prep.R : Step 5. Our Shoes 사진 미리 처리 =====
source("helpers.R")

files <- list.files("data/our_shoes", pattern = "\\.(jpe?g|png)$",
                    ignore.case = TRUE, full.names = TRUE)
if (length(files) == 0) stop("data/our_shoes 폴더에 jpg/png 사진이 없어요. 폴더 이름과 위치를 확인하세요.")
parts <- strsplit(tools::file_path_sans_ext(basename(files)), "_")

# 파일 이름 형식 검사: 이름_카테고리_번호
bad <- lengths(parts) != 3
if (any(bad)) stop("파일 이름을 Name_Category_N.jpg 형식으로 바꿔 주세요: ",
                   paste(basename(files)[bad], collapse = ", "))
info <- data.frame(file     = basename(files),
                   name     = sapply(parts, `[`, 1),
                   category = sapply(parts, `[`, 2),
                   num      = sapply(parts, `[`, 3))
wrong_cat <- !info$category %in% unique(pca$meta$category)
if (any(wrong_cat)) stop("카테고리 철자를 Boots / Sandals / Shoes / Slippers 중 하나로: ",
                         paste(info$file[wrong_cat], collapse = ", "))
info$label <- paste0(info$name, " - ", info$category, " ", info$num)

vecs   <- lapply(files, photo_to_vec)    # 앱이 검색에 쓸 64×64 흑백 (사진당 몇 초)
thumbs <- lapply(files, photo_thumb)     # 갤러리에 보여줄 작은 컬러 사진

saveRDS(list(info = info, vecs = vecs, thumbs = thumbs), "data/our_shoes.rds")
info

# 확인: 앱이 실제로 보는 64×64 모습
par(mfrow = c(ceiling(length(vecs) / 5), 5), mar = c(0.5, 0.5, 2, 0.5))
for (i in seq_along(vecs)) {
  plot(as.raster(matrix(vecs[[i]], 64, 64)))
  title(info$label[i], font.main = 1)
}