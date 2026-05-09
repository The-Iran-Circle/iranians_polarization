# ════════════════════════════════════════════════════════════════════════════
#  build_widget_data.R   (v2)
#
#  Reads raw DPP survey data, applies the same recoding used in
#  Story_Telling_DPP.Rmd, and writes pre-computed JSON files that the
#  Quarto datapage explorer reads.
#
#  Run from the project root:
#      Rscript R/build_widget_data.R
#
#  Outputs (all overwritten each run):
#      data/responses.json   tidy long-format raw responses + jitter offsets
#      data/summary.json     per (item × group): n, mean, SD, SE, 95% CI
#      data/stats.json       per item:  ANOVA F/p/eta², pairwise Cohen's d
#      data/labels.json      bilingual labels for batteries, items, groupings
# ════════════════════════════════════════════════════════════════════════════

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(forcats)
  library(stringr)
  library(purrr)
  library(jsonlite)
})

DATA_FILE <- "DPP_March 23.xlsx"
OUT_DIR   <- "data"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

set.seed(42)   # deterministic jitter

# ─── Load raw ──────────────────────────────────────────────────────────────
raw <- read_excel(DATA_FILE, sheet = "Sheet0", col_names = TRUE)
raw <- raw[-1, ]   # drop the Farsi label row

# ─── Build pol_group (matches Story_Telling_DPP.Rmd) ───────────────────────
label_map <- c(
  "جمهوری خواه"   = "Republican",
  "پادشاهی خواه" = "Monarchist",
  "چپ"            = "Left",
  "اصلاح طلب"     = "Reformist",
  "اصولگرا"       = "Principlist",
  "هیچکدام"       = "None",
  "موارد دیگر"    = "Other"
)

keep_combos <- c("Republican", "Monarchist", "Left + Republican",
                 "Monarchist + Republican", "Left")

# canonical display order (left → right on political spectrum)
pol_order <- c("Left", "Left + Republican", "Republican",
               "Monarchist + Republican", "Monarchist")

raw <- raw |>
  filter(!is.na(Political_label)) |>
  mutate(
    combo = str_split(Political_label, ",") |>
            map(~ str_trim(.x)) |>
            map(~ recode(.x, !!!label_map)) |>
            map_chr(~ paste(sort(unique(.x)), collapse = " + ")),
    pol_group = if_else(combo %in% keep_combos, combo, NA_character_),
    pol_group = factor(pol_group, levels = pol_order)
  )

cat("pol_group counts:\n")
print(raw |> count(pol_group))
cat("\n")

# ─── Persian → numeric mappers (3 scale types) ─────────────────────────────
# We use \uXXXX Unicode escapes for the Persian keys instead of literal
# Persian characters. This is critical on Windows: when source() reads a
# UTF-8 file, R uses the system locale (often CP1252 / Windows-1256) and
# the literal Persian strings get re-encoded into something that no longer
# matches the data. Pure-ASCII Unicode escapes are immune to that.
#
# Each key below is followed by an inline comment showing the Persian
# value for human reference.

likert7 <- c(
  "\u06a9\u0627\u0645\u0644\u0627\u064b \u0645\u062e\u0627\u0644\u0641\u0645" = 1,        # کاملاً مخالفم
  "\u0645\u062e\u0627\u0644\u0641\u0645" = 2,                                               # مخالفم
  "\u062a\u0627 \u062d\u062f\u06cc \u0645\u062e\u0627\u0644\u0641\u0645" = 3,              # تا حدی مخالفم
  "\u0646\u0647 \u0645\u0648\u0627\u0641\u0642\u0645 \u0646\u0647 \u0645\u062e\u0627\u0644\u0641\u0645" = 4,  # نه موافقم نه مخالفم
  "\u0646\u0647 \u0645\u062e\u0627\u0644\u0641\u0645 \u0646\u0647 \u0645\u0648\u0627\u0641\u0642" = 4,         # نه مخالفم نه موافق
  "\u062a\u0627 \u062d\u062f\u06cc \u0645\u0648\u0627\u0641\u0642\u0645" = 5,              # تا حدی موافقم
  "\u0645\u0648\u0627\u0641\u0642\u0645" = 6,                                               # موافقم
  "\u06a9\u0627\u0645\u0644\u0627\u064b \u0645\u0648\u0627\u0641\u0642\u0645" = 7         # کاملاً موافقم
)

importance5 <- c(
  "\u0627\u0635\u0644\u0627\u064b \u0627\u0647\u0645\u06cc\u062a \u0646\u062f\u0627\u0631\u062f" = 1,  # اصلاً اهمیت ندارد
  "\u0627\u0647\u0645\u06cc\u062a \u06a9\u0645\u06cc \u062f\u0627\u0631\u062f" = 2,                    # اهمیت کمی دارد
  "\u062a\u0627 \u062d\u062f\u06cc \u0627\u0647\u0645\u06cc\u062a \u062f\u0627\u0631\u062f" = 3,       # تا حدی اهمیت دارد
  "\u0627\u0647\u0645\u06cc\u062a \u0632\u06cc\u0627\u062f\u06cc \u062f\u0627\u0631\u062f" = 4,        # اهمیت زیادی دارد
  "\u0628\u0633\u06cc\u0627\u0631 \u0627\u0647\u0645\u06cc\u062a \u062f\u0627\u0631\u062f" = 5         # بسیار اهمیت دارد
)

extent5 <- c(
  "\u0627\u0635\u0644\u0627" = 1,                                  # اصلا
  "\u06a9\u0645" = 2,                                                # کم
  "\u062a\u0627 \u062d\u062f\u06cc" = 3,                            # تا حدی
  "\u0632\u06cc\u0627\u062f" = 4,                                    # زیاد
  "\u0628\u0633\u06cc\u0627\u0631 \u0632\u06cc\u0627\u062f" = 5     # بسیار زیاد
)

all_maps <- c(likert7, importance5, extent5)

to_num <- function(x) {
  if (is.numeric(x)) return(x)
  # Force UTF-8 encoding declaration on the input (defensive — readxl
  # already returns UTF-8, but on Windows the encoding flag can be lost
  # when objects pass through certain pipelines).
  ch <- enc2utf8(as.character(x))
  unname(all_maps[ch])
}

# ─── Battery definitions (items + scale type) ──────────────────────────────
batteries_def <- list(
  Political_ideas = list(
    items = paste0("Political_ideas_", 1:9),
    scale = "importance5"
  ),
  SDO = list(
    items = c("SDO_dom_pro_1","SDO_dom_pro_2","SDO_dom_con_1","SDO_dom_con_2",
              "SDO_antiEgal_pro_1","SDO_antiEgal_pro_2",
              "SDO_antiEgal_con_1","SDO_antiEgal_con_2"),
    scale = "likert7"
  ),
  ACT = list(
    items = c("ACT_submis_pro_1","ACT_submis_pro_2","ACT_submis_pro_3","ACT_submis_con_1",
              "ACT_conserv_pro_1","ACT_conserv_pro_2","ACT_conserv_con_1","ACT_conserv_con_2",
              "ACT_agress_con_1","ACT_agress_con_2","ACT_agress_pro_1","ACT_agress_con_3"),
    scale = "likert7"
  ),
  MFQ = list(
    items = c("MFQ_Auth_1","MFQ_InGroup_1","MFQ_InGroup_2",
              "MFQ_Harm_1","MFQ_Fair_1","MFQ_Purity_1"),
    scale = "likert7"
  ),
  Policy_attitudes = list(
    items = c("Foreign_Inter","Violence_peop","Federalism","SociSec",
              "Compromise","Maternal_lang","Palestine","WomenRights",
              "DistributiveJustice","HomoTrans",
              "Afghan_Immig_eco","Afghan_Immig_cul"),
    scale = "likert7"
  ),
  Ethnic_Civic = list(
    items = paste0("Ethnic/Civic_ident_", 1:8),
    scale = "importance5"
  ),
  Symbolic_nation = list(
    items = paste0("symbolic_nation_", 1:13),
    scale = "extent5"
  )
)

# ─── Cohen's d (pooled SD) ─────────────────────────────────────────────────
cohens_d <- function(a, b) {
  a <- a[!is.na(a)]; b <- b[!is.na(b)]
  if (length(a) < 2 || length(b) < 2) return(NA_real_)
  s_pooled <- sqrt(((length(a)-1)*var(a) + (length(b)-1)*var(b)) /
                   (length(a) + length(b) - 2))
  if (s_pooled == 0) return(NA_real_)
  (mean(a) - mean(b)) / s_pooled
}

d_label <- function(d) {
  if (is.na(d)) return(NA_character_)
  a <- abs(d)
  if (a < 0.2) "negligible"
  else if (a < 0.5) "small"
  else if (a < 0.8) "medium"
  else "large"
}

# ─── Build records for one battery ─────────────────────────────────────────
build_battery <- function(battery_id, items, grouping_var, group_levels) {

  d <- raw |>
    filter(!is.na(.data[[grouping_var]])) |>
    select(all_of(c(grouping_var, items))) |>
    mutate(across(all_of(items), to_num))

  # 1) Long-form responses with PRE-COMPUTED jitter offsets.
  # We compute jitter in R rather than in the browser because
  # Observable Plot's dx/dy callbacks evaluate ONCE per render, not
  # per-row, which would collapse all dots at the same score onto
  # the same offset. Pre-computing per row fixes that and also makes
  # the chart deterministic between renders.
  responses <- d |>
    pivot_longer(all_of(items), names_to = "item", values_to = "value") |>
    filter(!is.na(value)) |>
    transmute(
      item,
      grouping = grouping_var,
      group    = as.character(.data[[grouping_var]]),
      value    = round(value, 3),
      jx       = round(runif(n(), -0.18, 0.18), 3),
      jy       = round(runif(n(), -0.32, 0.32), 3)
    )

  # 2) summary
  summary <- d |>
    pivot_longer(all_of(items), names_to = "item", values_to = "value") |>
    filter(!is.na(value)) |>
    group_by(item, group = as.character(.data[[grouping_var]])) |>
    summarise(
      n     = n(),
      mean  = round(mean(value), 3),
      sd    = round(sd(value), 3),
      se    = round(sd(value)/sqrt(n()), 3),
      ci_lo = round(mean(value) - 1.96 * sd(value)/sqrt(n()), 3),
      ci_hi = round(mean(value) + 1.96 * sd(value)/sqrt(n()), 3),
      .groups = "drop"
    ) |>
    mutate(grouping = grouping_var) |>
    select(item, grouping, group, n, mean, sd, se, ci_lo, ci_hi)

  # 3) stats: ANOVA + pairwise Cohen's d
  stats_list <- lapply(items, function(it) {
    sub <- d |> select(value = all_of(it), grp = all_of(grouping_var)) |>
      filter(!is.na(value))
    if (length(unique(sub$grp)) < 2 || nrow(sub) < 5) return(NULL)

    fit <- aov(value ~ grp, data = sub)
    a   <- summary(fit)[[1]]
    F_  <- a[["F value"]][1]
    p_  <- a[["Pr(>F)"]][1]
    ss_b <- a[["Sum Sq"]][1]
    ss_t <- sum(a[["Sum Sq"]])
    eta2 <- if (ss_t > 0) ss_b / ss_t else 0

    grps_present <- intersect(group_levels, unique(as.character(sub$grp)))
    pairs <- combn(grps_present, 2, simplify = FALSE)
    pairwise <- lapply(pairs, function(pr) {
      a_v <- sub$value[as.character(sub$grp) == pr[1]]
      b_v <- sub$value[as.character(sub$grp) == pr[2]]
      d_  <- cohens_d(a_v, b_v)
      list(
        group1    = pr[1], group2 = pr[2],
        d         = if (is.na(d_)) NA else round(d_, 3),
        size      = d_label(d_),
        mean_diff = round(mean(a_v, na.rm = TRUE) - mean(b_v, na.rm = TRUE), 3)
      )
    })

    list(
      item        = it,
      grouping    = grouping_var,
      F           = round(F_, 3),
      p           = signif(p_, 4),
      eta_squared = round(eta2, 3),
      n_total     = nrow(sub),
      pairwise    = pairwise
    )
  })
  stats_list <- Filter(Negate(is.null), stats_list)
  
  list(
    responses = responses,
    summary   = summary,
    stats     = stats_list
  )
}

# ─── Run all batteries ─────────────────────────────────────────────────────
all_responses <- list()
all_summary   <- list()
all_stats     <- list()

for (battery_id in names(batteries_def)) {
  cat(sprintf("Building battery: %s ...\n", battery_id))
  res <- build_battery(
    battery_id   = battery_id,
    items        = batteries_def[[battery_id]]$items,
    grouping_var = "pol_group",
    group_levels = pol_order
  )
  all_responses[[battery_id]] <- res$responses
  all_summary[[battery_id]]   <- res$summary
  all_stats[[battery_id]]     <- res$stats
}

all_responses <- bind_rows(all_responses)
all_summary   <- bind_rows(all_summary)
all_stats     <- unlist(all_stats, recursive = FALSE)
names(all_stats) <- NULL

# ════════════════════════════════════════════════════════════════════════════
#  Bilingual labels for batteries, items, scales, groupings
# ════════════════════════════════════════════════════════════════════════════

# ─── Scale definitions ─────────────────────────────────────────────────────
scales_def <- list(
  likert7 = list(
    min = 1, max = 7, midpoint = 4,
    ticks_en = c("Strongly\ndisagree", "", "", "Neutral", "", "", "Strongly\nagree"),
    ticks_fa = c("کاملاً\nمخالفم", "", "", "نه موافق\nنه مخالف", "", "", "کاملاً\nموافقم")
  ),
  importance5 = list(
    min = 1, max = 5, midpoint = 3,
    ticks_en = c("Not at all\nimportant", "", "Somewhat\nimportant", "", "Very\nimportant"),
    ticks_fa = c("اصلاً\nاهمیت ندارد", "", "تا حدی", "", "بسیار\nاهمیت دارد")
  ),
  extent5 = list(
    min = 1, max = 5, midpoint = 3,
    ticks_en = c("Not at all", "", "Moderately", "", "Very much"),
    ticks_fa = c("اصلاً", "", "تا حدودی", "", "خیلی زیاد")
  )
)

# ─── Item labels (English + Persian) ───────────────────────────────────────
# Edit these freely; they don't affect computation.
item_labels <- list(

  # ── Political ideas (1–9) — importance scale ─────────────────────────────
  # Stem: "از نظر سیاسی، هر یک از موضوعات زیر تا چه اندازه برای شما اهمیت دارد؟"
  Political_ideas_1 = list(en = "Human rights & individual freedoms",     fa = "حقوق بشر و آزادی‌های فردی"),
  Political_ideas_2 = list(en = "Social justice & workers' rights",       fa = "عدالت اجتماعی و حقوق کارگران"),
  Political_ideas_3 = list(en = "Nationalism / Iran-centrism",            fa = "ملی‌گرایی / ایران‌گرایی"),
  Political_ideas_4 = list(en = "Environment",                            fa = "محیط‌زیست"),
  Political_ideas_5 = list(en = "Free-market economy",                    fa = "اقتصاد بازار آزاد"),
  Political_ideas_6 = list(en = "Ethnic minority rights & decentralization", fa = "حقوق اقلیت‌های قومی و تمرکززدایی"),
  Political_ideas_7 = list(en = "Separation of religion & state",         fa = "جدایی دین از حکومت"),
  Political_ideas_8 = list(en = "Religious & traditional values",         fa = "ارزش‌های دینی و سنتی"),
  Political_ideas_9 = list(en = "LGBTQ rights",                           fa = "حقوق همجنسگرایان و اقلیت‌های جنسی"),
  # ── SDO ──────────────────────────────────────────────────────────────────
  SDO_dom_pro_1      = list(en = "Some groups should dominate", fa = "برخی گروه‌ها باید مسلط باشند"),
  SDO_dom_pro_2      = list(en = "Inferior groups stay in place", fa = "گروه‌های پایین در جای خود بمانند"),
  SDO_dom_con_1      = list(en = "No group dominance (rev.)", fa = "بدون سلطهٔ گروهی (معکوس)"),
  SDO_dom_con_2      = list(en = "Equal status across groups (rev.)", fa = "برابری گروه‌ها (معکوس)"),
  SDO_antiEgal_pro_1 = list(en = "Equality is unrealistic", fa = "برابری غیرواقع‌بینانه است"),
  SDO_antiEgal_pro_2 = list(en = "Less equality, fewer problems", fa = "برابری کمتر، مشکلات کمتر"),
  SDO_antiEgal_con_1 = list(en = "Push toward equality (rev.)", fa = "حرکت به سوی برابری (معکوس)"),
  SDO_antiEgal_con_2 = list(en = "Equal income/opportunity (rev.)", fa = "درآمد و فرصت برابر (معکوس)"),

  # ── ACT (RWA-style) ──────────────────────────────────────────────────────
  ACT_submis_pro_1  = list(en = "Obey authority", fa = "اطاعت از مقامات"),
  ACT_submis_pro_2  = list(en = "Respect authority figures", fa = "احترام به صاحبان قدرت"),
  ACT_submis_pro_3  = list(en = "Trust the leadership", fa = "اعتماد به رهبری"),
  ACT_submis_con_1  = list(en = "Question authority (rev.)", fa = "زیر سؤال بردن قدرت (معکوس)"),
  ACT_conserv_pro_1 = list(en = "Stick to traditional values", fa = "پایبندی به ارزش‌های سنتی"),
  ACT_conserv_pro_2 = list(en = "Old ways are best", fa = "روش‌های قدیمی بهترند"),
  ACT_conserv_con_1 = list(en = "Embrace change (rev.)", fa = "پذیرش تغییر (معکوس)"),
  ACT_conserv_con_2 = list(en = "New values welcome (rev.)", fa = "ارزش‌های نو خوش‌آیندند (معکوس)"),
  ACT_agress_con_1  = list(en = "Tolerance for dissenters (rev.)", fa = "تحمل مخالفان (معکوس)"),
  ACT_agress_con_2  = list(en = "Forgiveness over punishment (rev.)", fa = "بخشش بر مجازات (معکوس)"),
  ACT_agress_pro_1  = list(en = "Strong punishment for deviance", fa = "مجازات شدید انحراف"),
  ACT_agress_con_3  = list(en = "Soft on crime (rev.)", fa = "نرمش با مجرمان (معکوس)"),

  # ── Moral Foundations ────────────────────────────────────────────────────
  MFQ_Auth_1    = list(en = "Authority/respect matters", fa = "اقتدار و احترام مهم است"),
  MFQ_InGroup_1 = list(en = "Loyalty to one's group", fa = "وفاداری به گروه خودی"),
  MFQ_InGroup_2 = list(en = "Group loyalty (2)", fa = "وفاداری گروهی (۲)"),
  MFQ_Harm_1    = list(en = "Avoiding harm", fa = "پرهیز از آسیب"),
  MFQ_Fair_1    = list(en = "Fairness", fa = "انصاف"),
  MFQ_Purity_1  = list(en = "Purity/sanctity", fa = "پاکی و قداست"),

  # ── Policy attitudes ─────────────────────────────────────────────────────
  Foreign_Inter       = list(en = "Foreign intervention",                fa = "مداخلهٔ خارجی"),
  Violence_peop       = list(en = "Violence in politics",                fa = "خشونت در سیاست"),
  Federalism          = list(en = "Federalism",                          fa = "فدرالیسم"),
  SociSec             = list(en = "Social security",                     fa = "تأمین اجتماعی"),
  Compromise          = list(en = "Political compromise",                fa = "سازش سیاسی"),
  Maternal_lang       = list(en = "Education in mother tongue",          fa = "آموزش به زبان مادری"),
  Palestine           = list(en = "Palestine cause",                     fa = "مسئلهٔ فلسطین"),
  WomenRights         = list(en = "Women's rights",                      fa = "حقوق زنان"),
  DistributiveJustice = list(en = "Distributive justice",                fa = "عدالت توزیعی"),
  HomoTrans           = list(en = "LGBTQ rights",                        fa = "حقوق دگرباشان"),
  Afghan_Immig_eco    = list(en = "Afghan immigrants — economy",         fa = "مهاجران افغان — اقتصاد"),
  Afghan_Immig_cul    = list(en = "Afghan immigrants — culture",         fa = "مهاجران افغان — فرهنگ"),

  # ── Ethnic/civic identity (8 items) ──────────────────────────────────────
  `Ethnic/Civic_ident_1` = list(en = "Born in Iran",                  fa = "تولد در ایران"),
  `Ethnic/Civic_ident_2` = list(en = "Iranian ancestry",              fa = "نیاکان ایرانی"),
  `Ethnic/Civic_ident_3` = list(en = "Speak Persian",                 fa = "تسلط به فارسی"),
  `Ethnic/Civic_ident_4` = list(en = "Live in Iran long-term",        fa = "اقامت طولانی در ایران"),
  `Ethnic/Civic_ident_5` = list(en = "Respect Iranian customs",       fa = "احترام به آداب ایرانی"),
  `Ethnic/Civic_ident_6` = list(en = "Feel Iranian",                  fa = "احساس ایرانی بودن"),
  `Ethnic/Civic_ident_7` = list(en = "Iranian citizenship",           fa = "تابعیت ایرانی"),
  `Ethnic/Civic_ident_8` = list(en = "Contribute to Iranian society", fa = "مشارکت در جامعهٔ ایرانی"),

  # ── Symbolic nationalism (13 items) ──────────────────────────────────────
  symbolic_nation_1  = list(en = "Cyrus the Great",              fa = "کوروش بزرگ"),
  symbolic_nation_2  = list(en = "Yalda Night",                  fa = "شب یلدا"),
  symbolic_nation_3  = list(en = "Nowruz",                       fa = "نوروز"),
  symbolic_nation_4  = list(en = "Woman Life Freedom movement",  fa = "جنبش زن، زندگی، آزادی"),
  symbolic_nation_5  = list(en = "Constitutional Revolution",    fa = "انقلاب مشروطه"),
  symbolic_nation_6  = list(en = "Traditional Iranian music",    fa = "موسیقی سنتی ایرانی"),
  symbolic_nation_7  = list(en = "Iranian art cinema",           fa = "سینمای هنری ایران"),
  symbolic_nation_8  = list(en = "Iranian food",                 fa = "غذای ایرانی"),
  symbolic_nation_9  = list(en = "Hafez poetry",                 fa = "شعر حافظ"),
  symbolic_nation_10 = list(en = "Shahnameh (Ferdowsi)",         fa = "شاهنامهٔ فردوسی"),
  symbolic_nation_11 = list(en = "Iranian wrestling",            fa = "کشتی ایرانی"),
  symbolic_nation_12 = list(en = "Lion & Sun flag",              fa = "پرچم شیر و خورشید"),
  symbolic_nation_13 = list(en = "Persepolis",                   fa = "تخت جمشید")
)

# ─── Battery-level metadata ────────────────────────────────────────────────
battery_meta <- list(
  Political_ideas  = list(label_en = "Political Ideas",
                          label_fa = "ایده‌های سیاسی",
                          description_fa = "میزان موافقت شما با هر یک از ایده‌های سیاسی زیر چقدر است؟"),
  SDO              = list(label_en = "Social Dominance Orientation",
                          label_fa = "گرایش به سلطهٔ اجتماعی",
                          description_fa = "میزان موافقت با گزاره‌های مربوط به نابرابری گروه‌های اجتماعی"),
  ACT              = list(label_en = "Authoritarianism (RWA)",
                          label_fa = "اقتدارگرایی",
                          description_fa = "اطاعت، محافظه‌کاری و خشونت در نگرش‌های اقتدارگرایانه"),
  MFQ              = list(label_en = "Moral Foundations",
                          label_fa = "بنیادهای اخلاقی",
                          description_fa = "اهمیت ارزش‌های اخلاقی پایه‌ای در داوری شما"),
  Policy_attitudes = list(label_en = "Policy Attitudes",
                          label_fa = "نگرش‌های سیاست‌گذاری",
                          description_fa = "میزان موافقت با موضع‌گیری‌های مشخص سیاست‌گذاری"),
  Ethnic_Civic     = list(label_en = "Ethnic vs. Civic Identity",
                          label_fa = "هویت قومی-مدنی",
                          description_fa = "برای ایرانی بودن «واقعی»، هر یک از ویژگی‌های زیر چقدر اهمیت دارند؟"),
  Symbolic_nation  = list(label_en = "Symbolic National Identity",
                          label_fa = "هویت ملی نمادین",
                          description_fa = "فکر کردن به هر یک از این نمادها تا چه حد در شما حس افتخار به ایرانی بودن ایجاد می‌کند؟")
)

# ─── Assemble labels.json ──────────────────────────────────────────────────
make_battery_json <- function(battery_id) {
  scale_id <- batteries_def[[battery_id]]$scale
  items    <- batteries_def[[battery_id]]$items
  meta     <- battery_meta[[battery_id]]

  list(
    label_en       = meta$label_en,
    label_fa       = meta$label_fa,
    description_fa = meta$description_fa,
    scale          = scales_def[[scale_id]],
    items = lapply(items, function(it) {
      lab <- item_labels[[it]]
      if (is.null(lab)) lab <- list(en = it, fa = it, full_fa = NULL)
      list(
        id        = it,
        label_en  = lab$en,
        label_fa  = lab$fa,
        full_fa   = if (is.null(lab$full_fa)) lab$fa else lab$full_fa  # fall back to short label
      )
    })
  )
}

labels <- list(
  batteries = setNames(
    lapply(names(batteries_def), make_battery_json),
    names(batteries_def)
  ),
  groupings = list(
    pol_group = list(
      label_en = "Political Group",
      label_fa = "گروه سیاسی",
      levels = list(
        list(id = "Left",                    label_en = "Left",                    label_fa = "چپ"),
        list(id = "Left + Republican",       label_en = "Left + Republican",       label_fa = "چپ + جمهوری‌خواه"),
        list(id = "Republican",              label_en = "Republican",              label_fa = "جمهوری‌خواه"),
        list(id = "Monarchist + Republican", label_en = "Monarchist + Republican", label_fa = "پادشاهی‌خواه + جمهوری‌خواه"),
        list(id = "Monarchist",              label_en = "Monarchist",              label_fa = "پادشاهی‌خواه")
      )
    )
  ),
  item_to_battery = setNames(
    as.list(rep(names(batteries_def), times = sapply(batteries_def, function(b) length(b$items)))),
    unlist(lapply(batteries_def, function(b) b$items))
  )
)

# ─── Write JSON ────────────────────────────────────────────────────────────
# Note: jsonlite::write_json on a length-0 list emits "{}" (object), but
# the browser code expects "[]" (array). We coerce explicitly.

write_json_array <- function(x, path) {
  if (length(x) == 0) {
    writeLines("[]", path, useBytes = TRUE)
  } else {
    write_json(x, path, auto_unbox = TRUE, pretty = TRUE)
  }
}

write_json(all_responses, file.path(OUT_DIR, "responses.json"),
           dataframe = "rows", auto_unbox = TRUE)
write_json(all_summary,   file.path(OUT_DIR, "summary.json"),
           dataframe = "rows", auto_unbox = TRUE, pretty = TRUE)
write_json_array(all_stats,    file.path(OUT_DIR, "stats.json"))

# ════════════════════════════════════════════════════════════════════════════
#  Affective thermometers (rater × target heatmap)
# ════════════════════════════════════════════════════════════════════════════

therm_cols <- c("Monarchist_thermo_1", "Republic_thermo_1", "Leftist_thermo_1",
                "Reformist_thermo_1", "Conserve_thermo_1", "MEK_thermo_2")

therm_targets <- c(
  "Monarchist_thermo_1" = "Monarchists",
  "Republic_thermo_1"   = "Republicans",
  "Leftist_thermo_1"    = "Leftists",
  "Reformist_thermo_1"  = "Reformists",
  "Conserve_thermo_1"   = "Principlists",
  "MEK_thermo_2"        = "MEK"
)

therm_target_order <- c("Monarchists", "Republicans", "Leftists",
                        "Reformists", "Principlists", "MEK")

# Per (rater × target) cell: n, mean, SD
thermometers_summary <- raw |>
  filter(!is.na(pol_group)) |>
  select(pol_group, all_of(therm_cols)) |>
  mutate(across(all_of(therm_cols), ~ suppressWarnings(as.numeric(.)))) |>
  pivot_longer(all_of(therm_cols), names_to = "target_var", values_to = "warmth") |>
  filter(!is.na(warmth), warmth >= 0, warmth <= 100) |>
  mutate(
    rater  = as.character(pol_group),
    target = unname(therm_targets[target_var])
  ) |>
  group_by(rater, target) |>
  summarise(
    n    = n(),
    mean = round(mean(warmth), 1),
    sd   = round(sd(warmth),   1),
    .groups = "drop"
  )




# ─── Auto-populate full_fa from row 1 of the Excel ─────────────────────────
# Row 1 contains Qualtrics-exported question text. We pull it directly
# rather than typing each item's full question by hand.

questions_row <- read_excel(DATA_FILE, sheet = "Sheet0",
                            skip = 1, n_max = 1,
                            col_names = names(read_excel(DATA_FILE, sheet = "Sheet0", n_max = 1)))

# For every item across all batteries, attach full_fa from row 1
for (battery_id in names(labels$batteries)) {
  labels$batteries[[battery_id]]$items <- lapply(
    labels$batteries[[battery_id]]$items,
    function(it) {
      q <- as.character(questions_row[[it$id]])
      if (!is.na(q) && nchar(q) > 0) {
        it$full_fa <- q
      }
      it
    }
  )
}


# Add the thermometer block to labels (so the page reads it from labels.json)
labels$thermometers <- list(
  label_en       = "Affective Thermometers",
  label_fa       = "ترموسنج عاطفی",
  description_fa = "احساس گرما (\u06f0 \u062a\u0627 \u06f1\u06f0\u06f0) هر اردوگاه نسبت به اردوگاه‌های دیگر",
  rater_order    = pol_order,                                   # rows
  target_order   = therm_target_order,                          # columns
  raters = list(   # bilingual labels for rows
    list(id = "Left",                    label_fa = "چپ"),
    list(id = "Left + Republican",       label_fa = "چپ + جمهوری‌خواه"),
    list(id = "Republican",              label_fa = "جمهوری‌خواه"),
    list(id = "Monarchist + Republican", label_fa = "پادشاهی‌خواه + جمهوری‌خواه"),
    list(id = "Monarchist",              label_fa = "پادشاهی‌خواه")
  ),
  targets = list(  # bilingual labels for columns
    list(id = "Monarchists",  label_fa = "پادشاهی‌خواهان"),
    list(id = "Republicans",  label_fa = "جمهوری‌خواهان"),
    list(id = "Leftists",     label_fa = "چپ‌گرایان"),
    list(id = "Reformists",   label_fa = "اصلاح‌طلبان"),
    list(id = "Principlists", label_fa = "اصول‌گرایان"),
    list(id = "MEK",          label_fa = "مجاهدین خلق")
  )
)
write_json(labels,             file.path(OUT_DIR, "labels.json"),
           auto_unbox = TRUE, pretty = TRUE)

write_json(thermometers_summary, file.path(OUT_DIR, "thermometers.json"),
           dataframe = "rows", auto_unbox = TRUE, pretty = TRUE)

cat(sprintf("\n✔ responses.json  %d rows\n",  nrow(all_responses)))
cat(sprintf("✔ summary.json    %d rows\n",  nrow(all_summary)))
cat(sprintf("✔ stats.json      %d items across %d batteries\n",
            length(all_stats), length(batteries_def)))
cat(sprintf("✔ labels.json     %d batteries, %d grouping(s)\n",
            length(labels$batteries), length(labels$groupings)))
cat(sprintf("✔ thermometers.json  %d cells\n", nrow(thermometers_summary)))
