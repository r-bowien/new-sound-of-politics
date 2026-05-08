# Claude Coding Log — The New Sound of Politics
**Project:** pol42050 — Bundestag populist rhetoric analysis  
**Working directory:** `c:\Users\rober\UCD\pol42050\new-sound-of-politics`  
**Models used:** Claude Sonnet 4.6 (Claude Code)

This log covers the full iterative coding conversation between the researcher and Claude Code across three sessions. Session 1 and Session 2 are partially reconstructed from stored transcripts and compaction summaries. Session 3 is verbatim.

---

## Session 1 — 2026-05-07

*Source: full transcript at `2e3461a6-d745-446f-ac2f-b592cd594838.jsonl`*

---

**USER:** Create code to implement this table into a quarto document:

> Table 2. Performance of the model on the 20% test set.
> Dimension / Precision / Recall / F1
> Anti-elitism: 0.81 / 0.88 / 0.84
> People-centrism: 0.70 / 0.73 / 0.71
> Left-wing ideology: 0.69 / 0.77 / 0.73
> Right-wing ideology: 0.68 / 0.66 / 0.67
> micro avg: 0.75 / 0.80 / 0.77
> macro avg: 0.72 / 0.76 / 0.74

Add a source you can find in the references.bib file to the table: erhard et al PopBert

**ASSISTANT:** Searched `references.bib` for the Erhard et al. citation — found key `erhard_popbert_2025`. Inserted a new R chunk `tbl-popbert-performance` in the appendix of `index.qmd` creating a `popbert_perf` data frame with six rows (four dimensions + micro/macro averages) rendered via `kable`/`kableExtra`. Caption includes `[@erhard_popbert_2025]`. A horizontal rule separates dimension rows from the average rows via `row_spec(4, hline_after = TRUE)`.

---

**USER:** Create a regression model in the index.qmd in the last code cell that says `# regression analysis`. The regression should include the variables specified in the key variable table in the theory section. Use the bt_speeches data. Think about if the PopBERT scores are the better dependent variables or the PopBERT scores_pred. The first is ratio variables and the second is binary. Employ the respective model and explain to me why it is the best here. Create a huxtable to have a nice output for the rendered pdf.

**ASSISTANT:** Examined the key variable table (Anti-Elitism, People-Centrism, Left-Wing Host-Ideology, Right-Wing Host-Ideology as DVs; Topic, Time, Party, Speaker as predictors). Inspected `6_final_wrangling.r` to confirm column names: `Anti.Elitism_pred`, `People.Centrism_pred`, `Left.Wing.Host.Ideology_pred`, `Right.Wing.Host.Ideology_pred` (binary), plus `Date`, `Party`, `Name`, `top_topic`, `score`, `populist_dimension` in the long format.

**Reasoning for binary outcome / logistic regression:** The ratio scores are transformer confidence outputs; modelling them would answer "how certain is PopBERT?" rather than "is this sentence populist?" The `_pred` binary columns represent PopBERT's actual classification decision. Logistic regression (binomial GLM) is statistically appropriate for binary 0/1 outcomes.

Added `library(huxtable)` to setup chunk. Replaced the empty `# regression analysis` chunk with:
- A `make_subset()` function filtering by `populist_dimension`, converting `Date` to numeric (`date_num`), relevelling `Party` with SPD as reference, factoring `top_topic`
- Four `glm(..., family = binomial)` models: `m_ae`, `m_pc`, `m_lw`, `m_rw`
- `huxreg()` table showing intercept, time trend, party dummies (topic FEs included but hidden); footnote explains this

---

**USER:** *(message cut off mid-sentence — no further input in this session)*

**ASSISTANT:** Asked for clarification on what the regression should include.

---

**USER:** Do not use the speak_ger dataset. Use `data/bt_speeches_long.rds`

**ASSISTANT:** Confirmed `bt_speeches_long.rds` is the pivoted version of `bt_speeches` with columns: `populist_dimension`, `score`, `Date`, `Party`, `Name`, `top_topic`, `top_topic_name`, `sentence`, `populist_mean`. Updated the regression chunk to load from `bt_speeches_long.rds` rather than speak_ger.

---

**USER:** Error in `make_label()`: quarto cell labels do not work with huxtable in TeX for quarto version 1.4 or above. Use huxtable labels instead via `label()` or `set_label()`.

**ASSISTANT:** Fixed the chunk label conflict. With Quarto ≥ 1.4, `tbl-*` chunk labels are intercepted before huxtable can handle them. Two changes:
1. Removed `#| label: tbl-regression` from chunk header; renamed chunk to `reg-table`
2. Added `set_label("tbl-regression")` to the huxtable pipeline so the LaTeX `\label{}` is set correctly
3. Fixed typo: `m_ea` → `m_ae` in the setup chunk

---

## Session 2 — 2026-05-07 (continued/extended session)

*Source: reconstructed from context compaction summary — exact wording of prompts and responses is paraphrased but content is accurate*

---

**USER:** Adjust the regression table (not the model calculation) to only include party and the topics that are significant at the 0.01 level for at least one dimension. Change topic labels to names of the topic. Have a look at the data and make changes only in `6_final_wrangling.r` and `index.qmd`.

**ASSISTANT:** Added `topic_nice_labels` lookup vector and `topic_coef_labels` object to `6_final_wrangling.r`:

```r
topic_coef_labels <- bt_speeches |>
  filter(!is.na(top_topic), !is.na(top_topic_name)) |>
  select(top_topic, top_topic_name) |>
  distinct() |>
  arrange(top_topic) |>
  mutate(
    short_name = str_remove(top_topic_name, "^\\d+_"),
    nice_name  = {
      lkp <- topic_nice_labels[short_name]
      ifelse(!is.na(lkp), lkp, str_to_title(short_name))
    },
    coef_name  = paste0("top_topic", top_topic)
  )
saveRDS(topic_coef_labels, "data/topic_coef_labels.rds")
```

Updated `index.qmd` setup chunk to load `topic_coef_labels` and build `topic_label_map`. Updated the regression chunk to:
1. Compute p-value matrix across all four models for all topic coefficients
2. Filter to only topics significant at p < 0.01 in at least one dimension (`sig_topics`)
3. Build human-readable `coef_names` vector using `topic_label_map`
4. Pass filtered `coef_names` to `huxreg()` via the `coefs` argument

---

**USER:** Adjust the lollipop plot to instead of the top 3 include the top 2 most and least speakers. Make changes where necessary in wrangling and index.

**ASSISTANT:** Updated `6_final_wrangling.r` to use `slice_max(n = 2)` and `slice_min(n = 2)` instead of `slice_max(n = 3)`:

```r
fig_speaker_pop <- bind_rows(
  speaker_summary |> group_by(Party) |> slice_max(mean_pop, n = 2),
  speaker_summary |> group_by(Party) |> slice_min(mean_pop, n = 2)
) |>
  distinct(Name, Party, .keep_all = TRUE) |>
  ...
```

Updated the figure caption in `index.qmd` accordingly.

---

**USER:** Now also add the median populist as a line for each party in the lollipop plot.

**ASSISTANT:** Added `party_medians` computation in `6_final_wrangling.r` and joined it to `fig_speaker_pop`:

```r
party_medians <- speaker_summary |>
  group_by(Party) |>
  summarise(median_pop = median(mean_pop, na.rm = TRUE), .groups = "drop")

fig_speaker_pop <- ... |>
  left_join(party_medians, by = "Party") |>
  ...
```

Added `geom_vline(aes(xintercept = median_pop), linetype = "dashed", colour = "grey50", linewidth = 0.5)` to the lollipop `ggplot` call in `index.qmd`. Updated caption to mention dashed line.

---

**USER:** m_ae finished. Following Warning messages: [convergence warnings — `max|grad| = 6.40275`, large eigenvalue ratio in Hessian]

**ASSISTANT:** Diagnosed the convergence warning: `date_num` was raw days since Unix epoch (~19,000), causing a large eigenvalue ratio in the Hessian. Fixed by standardising in `make_subset()`:

```r
df$date_num <- scale(as.numeric(df$Date))[, 1]
```

This makes the coefficient interpretable as change per standard deviation of time.

---

**USER:** The fitting of m_ae is going on for about one hour now. What do you think is the reason for this and what is the methodologically sound way to speed up the process?

**ASSISTANT:** Explained the cause: the default `glm()` was replaced by `glmer()` with `nAGQ = 1` (Laplace approximation). With ~300,000+ rows and ~700 speakers, the Laplace E-step is extremely slow. Proposed `nAGQ = 0` (penalised quasi-likelihood approximation), which is 3–10× faster. Methodological justification: PQL bias is negligible when groups have many observations (which is true here — most speakers have dozens to hundreds of sentences). This is the standard choice in large-scale parliamentary text analysis.

---

**USER:** [Rejected the glmerControl edit] Before you continue, explain what the `glmerControl` line does.

**ASSISTANT:** Explained:
- `glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))`: selects the BOBYQA (Bound Optimisation BY Quadratic Approximation) optimizer, which is more numerically stable than the default `nlminbwrap` for mixed models, and raises the maximum function evaluations from the default ~10,000 to 200,000 to prevent premature convergence failure on large models.

---

**USER:** Proceed.

**ASSISTANT:** Implemented the mixed-effects model. Updated `6_final_wrangling.r`:

```r
library(lme4)  # added; removed library(sandwich)

ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
m_ae <- glmer(score ~ date_num + Party + top_topic + (1 | Name), data = ae_d, family = binomial, control = ctrl, nAGQ = 0)
m_pc <- glmer(score ~ date_num + Party + top_topic + (1 | Name), data = pc_d, family = binomial, control = ctrl, nAGQ = 0)
m_lw <- glmer(score ~ date_num + Party + top_topic + (1 | Name), data = lw_d, family = binomial, control = ctrl, nAGQ = 0)
m_rw <- glmer(score ~ date_num + Party + top_topic + (1 | Name), data = rw_d, family = binomial, control = ctrl, nAGQ = 0)
```

The random intercept `(1 | Name)` handles within-speaker correlation without collinearity with `Party` (which fixed effects for `Name` would have caused). Removed the `vcovCL` block entirely as clustered SEs on a `glmerMod` are not appropriate.

---

**USER:** Check the regression table in `index.qmd`. Is it still appropriate for a mixed effects model? If not, explain the differences to me before adjusting. Adhere to the highest methodological standards.

**ASSISTANT:** Identified three problems with the existing table for a GLMM:

1. **Clustered SEs (`vcovCL`)**: Doubly approximate when combined with `nAGQ = 0`. The random intercept already accounts for within-speaker correlation; applying clustered SEs on top is methodologically unjustifiable. Should use model-based SEs from the observed Fisher information matrix.
2. **Missing ICC**: The intraclass correlation coefficient (ICC = σ²_speaker / (σ²_speaker + π²/3)) should be reported — it quantifies how much of the residual variance is attributable to the speaker random intercept.
3. **Statistics reported**: Log-likelihood and AIC become less meaningful with `nAGQ = 0`. Should report N and ICC instead.

---

**USER:** Implement the changes. Make sure the labelling of the table is updated respectively.

**ASSISTANT:** Implemented `fe_coeftest()` workaround in the `reg-table` chunk to extract fixed-effects-only coefficients from `glmerMod`:

```r
fe_coeftest <- function(m) {
  cf <- summary(m)$coefficients
  class(cf) <- c("coeftest", "matrix")
  cf
}
```

This bypasses `lmtest::coeftest.default` which calls `coef.merMod` and returns all ~700 conditional modes (speaker BLUPs), causing `diag()` to hit R's long-vector limit. `summary(m)$coefficients` returns only the fixed-effects table.

Added `icc_fmt()` function:
```r
icc_fmt <- function(m) {
  v <- as.numeric(VarCorr(m)$Name)
  sprintf("%.3f", v / (v + pi^2 / 3))
}
```

Updated the `huxreg()` note to include N per model and ICC per model. Set `statistics = character(0)` to suppress AIC/logLik. Wrapped in `suppressWarnings()` to handle harmless glance/logLik warnings from the coeftest objects.

---

**USER:** [Error: No tidy() method for objects of class glmerMod]

**ASSISTANT:** This occurred because `huxreg` initially tried to call `broom::tidy()` on the raw model objects. Fixed by reverting to the `ct_ae / ct_pc / ct_lw / ct_rw` coeftest objects as inputs to `huxreg()`, not the raw `glmerMod` objects.

---

**USER:** [Error: `diag()` long vectors not supported]

**ASSISTANT:** Caused by `lmtest::coeftest.default` calling `coef(m_ae)` on a `glmerMod`, which returns all conditional modes (~700 speakers × parameters) as a named vector, making `diag()` exceed R's vector limit. Fixed with the `fe_coeftest()` function described above — uses `summary(m)$coefficients` which only contains the fixed-effects table.

---

**USER:** [Error: make_label() quarto cell labels do not work with huxtable]

**ASSISTANT:** Chunk label `tbl-regression` was still present in a second location. Renamed chunk to `reg-table` throughout while keeping `set_label("tbl-regression")` on the huxtable object.

---

**USER:** [Warning: Error calling glance on model 1, of class coeftest]

**ASSISTANT:** `huxreg()` always attempts `broom::glance()` even when `statistics = character(0)`. Fixed by wrapping the entire `huxreg(...)` call in `suppressWarnings()`.

---

**USER:** Fix the positions for all figures and tables when rendering the quarto doc.

**ASSISTANT:** Added two things:
1. `fig-pos: "H"` to the YAML front matter (requires the `float` LaTeX package)
2. `options(huxtable.latex_float = "H")` in the setup chunk

Confirmed `extra_dependencies: ["longtable", "float"]` was already in the YAML output block, so `float` package was available.

---

**USER:** For the heatmap plot, increase the number of top percentages displayed in the squares to 10.

**ASSISTANT:** Changed `slice_max(mean_score, n = 5)` to `slice_max(mean_score, n = 10)` in the `fig-topic-party-heatmap` chunk.

---

**USER:** Think about if it makes sense to remove the Other_1 and Other_2 topics from the regression model. They seem to have a negative effect on populist rhetoric at the moment, but I cannot meaningfully interpret them as I do not know what is talked about here.

**ASSISTANT:** Recommended removal. `Other_1`/`Other_2` are the residual keyATM topics — they capture everything that didn't fit the seed-guided topics. Their coefficients cannot be substantively interpreted, and leaving them in the model creates an arbitrary reference-topic dependency when `agriculture` is the true reference. Removing them is consistent with already excluding the `procedural` topic.

---

**USER:** Implement the fix.

**ASSISTANT:** Added one filter line to `make_subset()` in `6_final_wrangling.r`:

```r
df <- df[!grepl("^Other", df$top_topic_name), ]   # drop residual keyATM topics
```

This sits after the existing `!is.na(df$top_topic)` filter and before the scaling/relevelling steps.

---

**USER:** Think about if fraktionslose members of parliament should be included. They are not from a specific party but their effects seem to be relatively strong.

**ASSISTANT:** Recommended exclusion for three reasons:
1. Fraktionslose MPs are a heterogeneous group — some are expelled AfD members, some independent, some former other parties. A single `Party = "fraktionslos"` coefficient is theoretically uninterpretable.
2. The sample is small, so the coefficient has wide uncertainty.
3. The random intercept already handles individual-level variation, so their exclusion would not bias the speaker-level estimates for other parties.

---

**USER:** No.

**ASSISTANT:** Confirmed — fraktionslose MPs are kept in the model. No code changes made.

---

## Session 3 — 2026-05-08

*Source: verbatim*

---

**ASSISTANT (session open):** Reviewed both key files. Identified a bug in the regression chunk: `set_label()` was being called with the full caption sentence rather than a short LaTeX identifier. In huxtable, `set_label()` sets the `\label{}` for cross-referencing; the visible caption belongs in `set_caption()`. Fixed:

```r
# Before:
set_label("Mixed-Effects Logistic Regression: Predictors of Populist Rhetoric...") |>

# After:
set_caption("Mixed-Effects Logistic Regression: Predictors of Populist Rhetoric...") |>
set_label("tbl-regression") |>
```

Noted that `data/m_ae.rds`, `m_pc.rds`, `m_lw.rds`, `m_rw.rds` are stale and need rerunning from `make_subset` onwards in `6_final_wrangling.r` because three changes accumulated: standardised `date_num`, removed `Other` topics, added `nAGQ = 0`.

---

**USER:** Check my interpretation of the coefficients and tell me what the difference between SPD as a reference category and the intercept is. Also explain to me if I interpreted the topic coefficients correctly and if not, how I should do it. Do not make any changes.

**ASSISTANT:**

**Intercept vs SPD reference category:**
These are two different things. The intercept is the estimated log-odds when *all* predictors simultaneously equal their reference/zero values:
- Party = SPD
- `date_num` = 0 (after standardisation = the *mean* date of the study period, not the start)
- `top_topic` = the reference topic (smallest factor level after filtering)
- Random effect = 0 (average speaker)

The intercept is not "the SPD baseline" in isolation — it is the SPD baseline at the mean date, for the reference topic, for an average speaker.

**Party coefficients:** Mostly correct. The phrasing "increases the log-odds to employ anti-elitist rhetoric" is slightly off — the log-odds are that a single *sentence* is classified as anti-elitist. Keep the unit of analysis (sentence) explicit.

**Topic coefficients — revision needed:**
1. The topic coefficients are relative to the omitted reference topic, not an absolute zero. The reference topic must be named explicitly.
2. Because `Party` is also in the model, topic coefficients are net of party composition. If AfD members disproportionately discuss migration, the migration topic coefficient does *not* capture the AfD effect — the party coefficient absorbs it. The topic coefficient answers: "among speakers of the same party, at the same time, does speaking on this topic associate with more populism than speaking on the reference topic?"

The sentence "Talking about other parties increases the log-odds of a sentence being classified as populist across all dimensions" should be revised to make explicit that this is controlled for party affiliation and time, and is relative to the reference topic.

---

**USER:** How do I know the reference topic?

**ASSISTANT:** The reference topic is whichever `top_topic` number is smallest in the filtered data, because `factor(df$top_topic)` with no levels argument sorts numerically and makes the first level the reference. Three ways to identify it:

1. In R: `levels(ae_d$top_topic)[1]`, then cross-reference with `topic_coef_labels`
2. From `summary(m_ae)`: the topic absent from the `top_topicN` rows is the reference
3. From `topic_coef_labels[1, ]`: the first row of the object sorted by `arrange(top_topic)` is the reference — this can be checked without rerunning anything since the RDS is already saved

---

**USER:** Go over the interpretation again and tell me what is missing and/or incorrect.

**ASSISTANT:** After reading the full interpretation passage (lines 446–465 of `index.qmd`), identified the following:

**Errors:**

1. **SPD listed as having coefficients (line 451):** "The coefficients for the governing parties SPD, Grüne, and FDP naturally were smaller..." — SPD is the reference category and has no coefficient. It is the zero baseline. Remove SPD from this sentence.

2. **CDU/CSU people-centrism significance unclear (line 449):** The paper states only p<0.01 results are discussed, but "The log-odds decreased by 0.332" for CDU/CSU people-centrism has no significance qualifier. Either confirm it is p<0.01 or flag it explicitly as an exception.

3. **"government" coefficient sentence is broken (line 457):** "increases the log-odds of anti-elitist classification by 0.174 and decreases by 5.89 compared to agriculture" — "decreases by 5.89" has no named subject. The dimension must be stated explicitly. Also verify -5.89 against the actual table — that magnitude is unusually large.

**Missing:**

4. **`date_num` coefficient never discussed:** H1 (populism increases over time) is the first hypothesis, and the regression directly tests it with `date_num`. The results section supports H1 only with the descriptive figure. The inferential test from the regression is not mentioned at all.

5. **ICC not interpreted in text:** The ICC values are in the table footnote but never discussed. A sentence on what proportion of residual variance is attributable to the speaker random intercept would strengthen methodological transparency.

6. **Security/migration interpreted for only one dimension (line 459):** "Coefficients for those are positive for people-centrism" — what about anti-elitism, left-wing, right-wing? Selective mention without explanation reads as incomplete.

7. **"party affairs might draw significance" caveat is misplaced (line 455):** Valid methodological observation, but belongs in the limitations section rather than inline immediately after reporting the coefficient.

---

**USER:** Would this sentence be sufficiently sound?
"The intra class correlation for the model is 0.147 for anti-elitism, 0.094 for people-centrism, 0.18 for left-wing host-ideology, and 0.207 for right-wing host-ideology, meaning after controlling for all other variables in the model, 14.7% / 9.4% / 18% / 20.7% of the difference in the outcome remain due to differences in the speakers rather than the party."

**ASSISTANT:** Almost, but two things need fixing:

1. **"difference in the outcome" → variance in the latent propensity:** The ICC for a logistic GLMM is computed on the latent (log-odds) scale, not the observed binary outcome. "Difference" is also vague — "variance" is the precise term.

2. **"rather than the party" is wrong:** Party is already in the model as a fixed effect — its variance has been removed before the ICC is computed. The ICC is the proportion of the *remaining residual variance* that is systematic between-speaker. "Rather than the party" implies a competition between speaker and party as explanations, but party is already gone from the denominator.

Suggested revision:
> "The intraclass correlation coefficients are 0.147 (anti-elitism), 0.094 (people-centrism), 0.180 (left-wing host-ideology), and 0.207 (right-wing host-ideology), indicating that — on the latent scale, after controlling for party affiliation, topic, and time — 14.7%, 9.4%, 18%, and 20.7% of the remaining variance in populist rhetoric is attributable to stable individual differences between speakers."

---

**USER:** Create a new .md file called claude_coding_log.md that includes this complete conversation including those parts that fell out of context. Include both my prompts and your output as thoroughly as possible.

**ASSISTANT:** Created this file.

---

## Key technical reference

### Files modified
- `research paper/6_final_wrangling.r` — wrangling, model fitting, figure data preparation
- `research paper/index.qmd` — Quarto document (all figures, tables, analysis)

### Architecture of `make_subset()` (final version)
```r
make_subset <- function(dim_name) {
  df           <- bt_speeches_long[bt_speeches_long$populist_dimension == dim_name, ]
  df           <- df[!is.na(df$top_topic), ]                         # drop DFM-excluded speeches
  df           <- df[!grepl("^Other", df$top_topic_name), ]          # drop residual keyATM topics
  df$date_num  <- scale(as.numeric(df$Date))[, 1]   # standardise: fixes large-eigenvalue warning
  df$Party     <- relevel(factor(df$Party), ref = "SPD")
  df$top_topic <- factor(df$top_topic)
  df
}
```

### Model specification (final version)
```r
ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
m_ae <- glmer(score ~ date_num + Party + top_topic + (1 | Name), data = ae_d, family = binomial, control = ctrl, nAGQ = 0)
```
- `nAGQ = 0`: PQL approximation — valid for large groups (many sentences per speaker), 3–10× faster than Laplace
- `(1 | Name)`: random intercept by speaker — handles within-speaker correlation without collinearity with `Party`
- `date_num` standardised: fixes large eigenvalue ratio convergence warning from raw day-since-epoch values

### ICC formula for logistic GLMM
```r
icc_fmt <- function(m) {
  v <- as.numeric(VarCorr(m)$Name)
  sprintf("%.3f", v / (v + pi^2 / 3))
}
```
The π²/3 ≈ 3.29 is the fixed variance of the standard logistic distribution (level-1 residual on the latent scale).

### `fe_coeftest()` workaround for huxreg + glmerMod
```r
fe_coeftest <- function(m) {
  cf <- summary(m)$coefficients
  class(cf) <- c("coeftest", "matrix")
  cf
}
```
`lmtest::coeftest.default` calls `coef.merMod` which returns all ~700 conditional modes (speaker BLUPs), causing `diag()` to hit R's long-vector limit. `summary(m)$coefficients` returns fixed-effects only.

### Errors encountered and fixed

| Error | Cause | Fix |
|---|---|---|
| `make_label()` quarto cell labels | Quarto ≥ 1.4 intercepts `tbl-*` chunk labels | Rename chunk to `reg-table`; use `set_label("tbl-regression")` on huxtable object |
| `No tidy() method for glmerMod` | `huxreg` tried to call `broom::tidy()` on raw model | Pass `coeftest` objects, not raw `glmerMod` |
| `diag() long vectors not supported` | `lmtest::coeftest.default` → `coef.merMod` returns all BLUPs | Use `fe_coeftest()` wrapping `summary(m)$coefficients` |
| `Error calling glance on model 1` | `huxreg` attempts `glance()` even with `statistics = character(0)` | Wrap `huxreg(...)` in `suppressWarnings()` |
| Convergence warning `max\|grad\| = 6.40` | `date_num` was raw epoch days (~19,000) → large Hessian eigenvalue ratio | `scale(as.numeric(df$Date))[, 1]` in `make_subset()` |
| Model fitting > 1 hour | Default `nAGQ = 1` (Laplace) expensive with 300k+ rows | Add `nAGQ = 0` (PQL) to all four `glmer()` calls |
