# =============================================================
# StoreMax simulated data — MAR 7628 running dataset
# One generative story used across Decks 1–2 (and the DiD lab)
# =============================================================
# Design goals (known truth, so interpretation can be graded):
#  - price effect flips sign when market_affluence is added (Simpson's)
#  - ad_spend x price_tier interaction exists (Deck 2)
#  - heteroscedasticity: error variance grows with store_size (Deck 2)
#  - DiD extension: loyalty program piloted in 2 regions, true effect +250
# =============================================================

set.seed(7628)
library(tidyverse)

n <- 400

# ---- Cross-sectional data (Decks 1–2) -----------------------
storemax <- tibble(
  store_id = sprintf("S%03d", 1:n),
  region   = sample(c("Southeast", "Northeast", "Midwest", "West"),
                    n, replace = TRUE, prob = c(.3, .25, .25, .2)),
  # Confounder: market affluence (z-score-ish scale)
  market_affluence = rnorm(n, 0, 1),
  # Store size in 1,000 sq ft
  store_size = round(runif(n, 8, 60), 1)
) |>
  mutate(
    # Affluent markets charge more (confounding path 1)
    price = round(20 + 3 * market_affluence + rnorm(n, 0, 1.2), 2),
    # Ad spend ($000/week), mildly related to store size
    ad_spend = round(pmax(1, 10 + 0.15 * store_size + rnorm(n, 0, 3)), 1),
    price_tier = if_else(price > median(price), "Premium", "Value"),
    region = factor(region,
                    levels = c("Southeast", "Northeast", "Midwest", "West")),
    # Region effects (vs Southeast reference)
    region_eff = case_when(
      region == "Southeast" ~ 0,
      region == "Northeast" ~ 1200,
      region == "Midwest"   ~ 400,
      region == "West"      ~ 800
    ),
    # TRUE MODEL:
    #  - true price effect NEGATIVE (-340) once affluence held constant
    #  - affluence boosts sales (+1800): confounding path 2
    #    (strong enough that the bivariate price slope comes out POSITIVE)
    #  - ad spend works (+120/[$1k]), but LESS in premium stores
    #    (interaction -60): planted for Deck 2
    #  - heteroscedastic noise: sd grows with store size
    sales = round(
      9000 +
        120 * ad_spend +
        (-60) * ad_spend * (price_tier == "Premium") +
        (-340) * price +
        1800 * market_affluence +
        35   * store_size +
        region_eff +
        rnorm(n, 0, 40 + 14 * store_size)   # heteroscedasticity
    )
  ) |>
  select(store_id, region, sales, ad_spend, price, price_tier,
         store_size, market_affluence)

write_csv(storemax, "storemax.csv")

# Sanity checks: the Simpson's flip (sign reversal)
coef(lm(sales ~ price, data = storemax))["price"]          # clearly POSITIVE
coef(lm(sales ~ price + market_affluence,
        data = storemax))["price"]                          # ~ -340 (negative)

# ---- DiD extension (Deck 2 example + lab) -------------------
# Two periods; loyalty program launched in NE + West in period 2.
# Parallel trends hold by construction (common +300 time trend).
true_did <- 250

storemax_did <- storemax |>
  select(store_id, region, ad_spend, price, price_tier,
         store_size, market_affluence) |>
  crossing(period = c("Pre", "Post")) |>
  mutate(
    treated = region %in% c("Northeast", "West"),
    post    = period == "Post",
    region_eff = case_when(
      region == "Southeast" ~ 0,
      region == "Northeast" ~ 1200,
      region == "Midwest"   ~ 400,
      region == "West"      ~ 800
    ),
    sales = round(
      9000 +
        120 * ad_spend +
        (-60) * ad_spend * (price_tier == "Premium") +
        (-340) * price +
        1800 * market_affluence +
        35  * store_size +
        region_eff +
        300 * post +                      # common time trend
        true_did * treated * post +       # the causal effect
        rnorm(n(), 0, 200)
    ),
    period = factor(period, levels = c("Pre", "Post"))
  ) |>
  select(store_id, region, period, treated, post, sales,
         ad_spend, price, price_tier, store_size)

write_csv(storemax_did, "storemax_did.csv")

# Sanity check: DiD recovers ~ +250
coef(lm(sales ~ treated * post, data = storemax_did))["treatedTRUE:postTRUE"]
