# ─────────────────────────────────────────────────────────────
# Download all Monst monster icons from GameWith's Firebase
# master JSON. Uses the "アイコン" field to build image URLs.
# ─────────────────────────────────────────────────────────────

library(httr2)
library(jsonlite)
library(progress)
library(future.apply)

# ---------- CONFIG ----------
JSON_URL    <- "https://firebasestorage.googleapis.com/v0/b/walkthrough-tool.appspot.com/o/monst%2Fmaster%2Fcharacters.json?alt=media"
OUT_DIR     <- file.path(getwd(), "monst_images")   # NOT your home folder
IMG_URL     <- "https://img.gamewith.jp/article_tools/monst/gacha/%s.jpg"
ICON_FIELD  <- "アイコン"                            # ← confirmed field
NAME_FIELDS <- c("モンスター名", "name", "名前")     # for alt text (tries in order)

N_WORKERS   <- 8
REQ_TIMEOUT <- 15
MAX_RETRIES <- 2
RETRY_WAIT  <- 0.5
# ----------------------------

if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)
cat("Output folder:", normalizePath(OUT_DIR), "\n\n")

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

safe_name <- function(u) {
  gsub("[^A-Za-z0-9._-]", "_", basename(sub("\\?.*$", "", u)))
}

# ---------- 1. Fetch master JSON ----------
cat("Fetching master JSON...\n")
resp <- request(JSON_URL) |>
  req_user_agent("Mozilla/5.0") |>
  req_timeout(60) |>
  req_perform()

data <- resp_body_json(resp, simplifyVector = FALSE)
cat(sprintf("  ✓ %d monsters loaded\n\n", length(data)))

# ---------- 2. Extract IDs and names ----------
ids <- vapply(data, function(m) {
  v <- m[[ICON_FIELD]]
  if (is.null(v) || length(v) == 0) NA_character_ else as.character(v)
}, character(1))

# Pick the first name-like field that exists
name_field <- NULL
for (f in NAME_FIELDS) {
  if (!is.null(data[[1]][[f]])) { name_field <- f; break }
}
cat(sprintf("Name field used: %s\n", name_field %||% "(none)"))

alts <- if (!is.null(name_field)) {
  vapply(data, function(m) as.character(m[[name_field]] %||% ""), character(1))
} else rep("", length(ids))

# Drop entries without a valid numeric ID
keep <- !is.na(ids) & grepl("^[0-9]+$", ids)
cat(sprintf("Valid IDs: %d / %d\n\n", sum(keep), length(ids)))

ids  <- ids[keep]
alts <- alts[keep]
srcs <- sprintf(IMG_URL, ids)

# ---------- 3. Parallel download ----------
cat(sprintf("Downloading %d icons with %d workers...\n\n",
            length(srcs), N_WORKERS))

plan(multisession, workers = N_WORKERS)

download_one <- function(i, src, alt) {
  fname <- sprintf("%s_%s", sprintf("%05d", i), safe_name(src))
  dest  <- file.path(OUT_DIR, fname)
  
  if (file.exists(dest) && file.size(dest) > 0) {
    return(list(i = i, status = "skip", url = src, alt = alt))
  }
  
  for (attempt in seq_len(MAX_RETRIES + 1)) {
    ok <- tryCatch({
      request(src) |>
        req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36") |>
        req_timeout(REQ_TIMEOUT) |>
        req_perform(path = dest)
      TRUE
    }, error = function(e) FALSE)
    
    if (ok) return(list(i = i, status = "ok", url = src, alt = alt))
    if (attempt <= MAX_RETRIES) Sys.sleep(RETRY_WAIT)
  }
  
  if (file.exists(dest)) try(file.remove(dest), silent = TRUE)
  list(i = i, status = "fail", url = src, alt = alt)
}

# Progress bar via a shared counter (works around parallel env)
pb <- progress_bar$new(
  format = "  [:bar] :current/:total (:percent) | ok: :ok fail: :fail skip: :skip | eta: :eta",
  total = length(srcs), clear = FALSE, width = 95
)

counter <- new.env(); counter$ok <- 0; counter$fail <- 0; counter$skip <- 0

results <- future_lapply(
  seq_along(srcs),
  function(i) {
    r <- download_one(i, srcs[i], alts[i])
    if (r$status == "ok")   counter$ok   <- counter$ok   + 1
    if (r$status == "fail") counter$fail <- counter$fail + 1
    if (r$status == "skip") counter$skip <- counter$skip + 1
    pb$tick(tokens = list(ok = counter$ok, fail = counter$fail,
                          skip = counter$skip))
    r
  },
  future.seed = TRUE
)

plan(sequential)
pb$terminate()

# ---------- 4. Report ----------
st   <- vapply(results, `[[`, character(1), "status")
ok   <- sum(st == "ok")
skip <- sum(st == "skip")
fail <- sum(st == "fail")

cat(sprintf("\n✓ Done. %d downloaded, %d skipped, %d failed\n",
            ok, skip, fail))
cat(sprintf("  Output: %s\n", normalizePath(OUT_DIR)))

if (fail > 0) {
  failed <- results[st == "fail"]
  log_file <- file.path(getwd(), "failed_urls.txt")
  writeLines(vapply(failed, `[[`, character(1), "url"), log_file)
  cat(sprintf("  → %d failed URLs written to %s\n", fail, log_file))
  cat("  → Rerun the script to retry only these (existing files are skipped).\n")
}