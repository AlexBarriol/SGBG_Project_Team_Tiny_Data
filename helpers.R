# ===== helpers.R : 앱과 테스트가 함께 쓰는 함수들 =====
pca <- readRDS("data/pca.rds")

# 1) 사진(길이 4096) → PC score K개
get_scores <- function(x, K) {
  as.vector((x - pca$center) %*% pca$rotation[, 1:K, drop = FALSE])
}

# 2) PC K개로 사진 다시 만들기 (average shoe + K개 eigenshoe의 조합)
rebuild <- function(x, K) {
  s   <- get_scores(x, K)
  out <- pca$center + pca$rotation[, 1:K, drop = FALSE] %*% s
  pmin(pmax(as.vector(out), 0), 1)          # 밝기를 0~1 범위로 자르기
}

# 3) score가 가장 가까운 카탈로그 사진 n장
search_top <- function(x, K, n = 5, exclude_id = NULL) {
  s <- get_scores(x, K)
  S <- pca$scores[, 1:K, drop = FALSE]
  d <- sqrt(colSums((t(S) - s)^2))          # 2,000장 각각과의 거리
  res <- data.frame(id = pca$meta$id, category = pca$meta$category, distance = d)
  if (!is.null(exclude_id)) res <- res[res$id != exclude_id, ]   # 자기 자신 빼기
  res <- res[order(res$distance), ]
  rownames(res) <- NULL
  head(res, n)
}

# 4) 다수결 예측 (동점이면 더 가까운 사진의 category)
predict_cat <- function(top) {
  tab     <- table(top$category)
  winners <- names(tab)[tab == max(tab)]
  top$category[top$category %in% winners][1]
}









# ---------- 사진 파일 → 길이 4096 벡터 ----------
library(png)
library(jpeg)

to_gray <- function(a) {
  if (length(dim(a)) == 2) return(a)                 # 이미 흑백
  n_ch  <- dim(a)[3]
  alpha <- if (n_ch %in% c(2, 4)) a[, , n_ch] else 1 # 투명도 채널이 있으면 꺼내기
  gray  <- if (n_ch >= 3) 0.299 * a[, , 1] + 0.587 * a[, , 2] + 0.114 * a[, , 3] else a[, , 1]
  gray * alpha + 1 * (1 - alpha)                     # 투명한 곳은 흰색(1)으로
}

pad_square <- function(a, fill = 1) {                # 흰 여백으로 정사각형 만들기
  h <- nrow(a); w <- ncol(a); n <- max(h, w)
  out <- matrix(fill, n, n)
  r0 <- (n - h) %/% 2; c0 <- (n - w) %/% 2
  out[r0 + seq_len(h), c0 + seq_len(w)] <- a
  out
}

resize_mat <- function(a, size = 64) {               # 칸마다 평균 내서 64×64로
  if (nrow(a) == size && ncol(a) == size) return(a)
  if (nrow(a) < size) a <- a[round(seq(1, nrow(a), length.out = size)), ]  # 작은 사진은 늘리기
  if (ncol(a) < size) a <- a[, round(seq(1, ncol(a), length.out = size))]
  ri <- cut(seq_len(nrow(a)), size, labels = FALSE)
  ci <- cut(seq_len(ncol(a)), size, labels = FALSE)
  a1 <- rowsum(a, ri) / as.vector(table(ri))
  t(rowsum(t(a1), ci) / as.vector(table(ci)))
}

img_to_vec <- function(path) {
  ext <- tolower(tools::file_ext(path))
  a   <- if (ext %in% c("jpg", "jpeg")) readJPEG(path) else readPNG(path)
  a   <- resize_mat(pad_square(to_gray(a)))
  as.vector(a)
}















# ---------- 직접 찍은 사진(Our Shoes)용 전처리 ----------
# 배경을 흰색으로 맞추기 → 신발 부분만 잘라내기 → 여백 두고 정사각형 → 64×64
photo_to_vec <- function(path, margin = 0.06, thr = 0.75) {
  ext <- tolower(tools::file_ext(path))
  a   <- if (ext %in% c("jpg", "jpeg")) readJPEG(path) else readPNG(path)
  g   <- to_gray(a)
  g   <- pmin(g / max(quantile(g, 0.9), 1e-6), 1)    # 밝은 배경(상위 10%)을 흰색(1)으로
  dark <- g < thr                                     # 신발로 볼 어두운 픽셀
  rows <- which(rowMeans(dark) > 0.01)
  cols <- which(colMeans(dark) > 0.01)
  if (length(rows) > 1 && length(cols) > 1)
    g <- g[min(rows):max(rows), min(cols):max(cols)]  # 신발이 있는 영역만 자르기
  sq  <- pad_square(g)
  n   <- nrow(sq); p <- round(n * margin)
  big <- matrix(1, n + 2 * p, n + 2 * p)              # 카탈로그처럼 둘레에 흰 여백
  big[p + seq_len(n), p + seq_len(n)] <- sq
  as.vector(resize_mat(big))
}

# 갤러리에 보여줄 작은 컬러 사진 (size × size)
photo_thumb <- function(path, size = 200) {
  ext <- tolower(tools::file_ext(path))
  a   <- if (ext %in% c("jpg", "jpeg")) readJPEG(path) else readPNG(path)
  if (length(dim(a)) == 2) a <- array(a, c(dim(a), 3))   # 흑백이면 3채널로
  a   <- a[, , 1:3, drop = FALSE]                        # 투명도 채널은 버림
  ch  <- lapply(1:3, function(k) resize_mat(pad_square(a[, , k]), size))
  array(unlist(ch), c(size, size, 3))
}

# 좌우 반전 (앞코 방향 맞추기용)
mirror_vec <- function(v) as.vector(matrix(v, 64, 64)[, 64:1])