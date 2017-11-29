library(ggplot2)
library(GGally)
library(trelliscopejs)
library(autocogs)
library(dplyr)
library(tidyr)
library(purrr)
library(housingData)


housing %>% dplyr::group_by(county, state)




housing %>%
  filter(state == "AZ") %>%
  group_by(county, state) ->
az
az_y <- range(az$medListPriceSqft, na.rm = TRUE)
az %>%
  nest() %>%
  mutate(
    panel = map_plot(data, ~ ggplot(.x, mapping = aes(time, medListPriceSqft)) + geom_point() + geom_smooth() + ylim(az_y[1], az_y[2])
    )
  ) %>%
  trelliscope("arizona", "housing", path = "housing_list_price_az", nrow = 3, ncol = 5, state = list(sort = list(sort_spec("county"), sort_spec("state"))))

housing %>%
  filter(state == "TX") %>%
  group_by(county, state) ->
tx
tx_y <- range(tx$medListPriceSqft, na.rm = TRUE)
tx %>%
  nest() %>%
  mutate(
    panel = map_plot(data, ~ ggplot(.x, mapping = aes(time, medListPriceSqft)) + geom_point() + geom_smooth() + ylim(az_y[1], az_y[2])
    )
  ) ->
tx_data
tx_data %>%
  trelliscope("texas", "housing", path = "housing_list_price_tx", nrow = 11, ncol = 23, state = list(sort = list(sort_spec("county"), sort_spec("state"))))

housing_y <- range(housing$medListPriceSqft, na.rm = TRUE)

housing %>%
  group_by(county, state) %>%
  nest() %>%
  filter(map_dbl(data, nrow) > 10) %>%
  mutate(
    loess = lapply(data, function(dt) {
      loess(
        medListPriceSqft ~ as.numeric(time),
        data = filter(dt, !is.na(medListPriceSqft))
      )
    })
  ) %>%
  mutate(
    res_std_err = map_dbl(loess, ~ cog(.$s, "residual standard error")),
    enp = map_dbl(loess, ~ cog(.$enp, "effective number of parameters")),
    mean_med_price = map_dbl(data, ~ cog(mean(.$medListPriceSqft), "mean of median list price")),
    n_obs_list = map_dbl(data, ~ cog(sum(!is.na(.$medListPriceSqft)), "number of observations")),
    zillow_href = map2(county, state, function(county_, state_) {
      cog_href(
        sprintf("http://www.zillow.com/homes/%s-%s_rb/", county_, state_),
        "zillow link"
      )
    }),
    panel = map_plot(data, ~
      ggplot(.x, mapping = aes(time, medListPriceSqft)) +
        geom_point() +
        geom_smooth(method = "loess") +
        ylim(housing_y[1], housing_y[2]))
  ) %>%
  trelliscope(
    "by_hand", "housing", nrow = 3, ncol = 5,
    path = "demo",
    state = list(sort =
      list(sort_spec("county"), sort_spec("state"))))







tx_data %>%
  autocogs::add_panel_cogs()

tx_data %>%
  autocogs::add_panel_cogs()

qplot(x,y,data = tx, geoms = c("points", "smooth")) + facet_trell(auto_cogs = TRUE)








data(tips, package = "reshape")
pm <- ggpairs(tips, c(1,2,6), mapping = aes(color = sex)); pm
pm


ggduo(psychademic, 3:1, 4:8, showStrips = FALSE)




loess_with_cor <- function(data, mapping, ..., method = "pearson") {
  x <- data[[deparse(mapping$x)]]
  y <- data[[deparse(mapping$y)]]
  cor <- cor(x, y, method = method)
  ggally_smooth_loess(data, mapping, ...) +
    ggplot2::geom_label(
      data = data.frame(
        x = min(x, na.rm = TRUE),
        y = max(y, na.rm = TRUE),
        lab = round(cor, digits = 3)
      ),
      mapping = ggplot2::aes(x = x, y = y, label = lab),
      hjust = 0, vjust = 1,
      size = 5, fontface = "bold"
    )
}
ggduo(psychademic, 3:1, 4:8, types = list(continuous = loess_with_cor), showStrips = FALSE)



y_labels <- c("personal consumption\nexpenditures (B)", "total\npopulation (K)", "personal savings\nrate %", "median duration of\nunemployment (week)", "number of\nunemployed (K)")

ggts(economics, "date", 2:6, columnLabelsY = y_labels)

econ <- economics
econ$increasing <- as.factor(c(TRUE, econ$unemploy[-1] - econ$unemploy[-nrow(econ)] >= 0))
ggts(econ, "date", 2:7, mapping = aes(color = increasing))


ggduo(
  economics, 1, 2:6,
  columnLabelsX = "date",
  columnLabelsY = c("personal consumption\nexpenditures (B)", "total\npopulation (K)", "personal savings\nrate %", "median duration of\nunemployment (week)", "number of\nunemployed (K)")
) + theme(axis.title.y = element_text(size = 9))



mod <- step(
  lm(
    formula = head ~ .,
    data = flea
  )
)
mod
# Call:
# lm(formula = head ~ species + tars1 + tars2 + aede1, data = flea)
#
# Coefficients:
#      (Intercept)   speciesHeikert.  speciesHeptapot.             tars1
#          0.63693           1.13651           5.22207           0.06815
#            tars2             aede1
#          0.10160           0.17069
summary(mod)
# Call:
# lm(formula = head ~ species + tars1 + tars2 + aede1, data = flea)
#
# Residuals:
#     Min      1Q  Median      3Q     Max
# -3.0152 -1.2315 -0.0048  1.1490  3.6068
#
# Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)
# (Intercept)       0.63693    6.09162   0.105 0.917034
# speciesHeikert.   1.13651    1.25452   0.906 0.368171
# speciesHeptapot.  5.22207    0.92110   5.669 3.18e-07 ***
# tars1             0.06815    0.01989   3.426 0.001043 **
# tars2             0.10160    0.03297   3.081 0.002974 **
# aede1             0.17069    0.04275   3.993 0.000163 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 1.6 on 68 degrees of freedom
# Multiple R-squared:  0.685,	Adjusted R-squared:  0.6618
# F-statistic: 29.57 on 5 and 68 DF,  p-value: 7.997e-16

ggnostic(mod)

ggnostic(mod, aes(color = species))

ggnostic(
  mod,
  mapping = aes(color = species),
  columnsY = c(
    "head", ".fitted", ".se.fit", ".resid", ".std.resid", ".hat", ".cooksd"
  ),
  continuous = list(
    default = ggally_smooth_lm
  ),
  combo = list(
    default = wrap(ggally_box_no_facet, outlier.shape = 21),
    .fitted = wrap(ggally_box_no_facet, outlier.shape = 21),
    .se.fit = wrap(ggally_nostic_se_fit, outlier.shape = 21),
    .resid = wrap(ggally_nostic_resid, outlier.shape = 21),
    .std.resid = wrap(ggally_nostic_std_resid, outlier.shape = 21),
    .hat = wrap(ggally_nostic_hat, outlier.shape = 21),
    .cooksd = wrap(ggally_nostic_cooksd, outlier.shape = 21)
  )
)



housing_y <- range(housing$medListPriceSqft, na.rm = TRUE)

# set up data
housing %>%
  group_by(county, state) %>%
  nest() %>%
  filter(map_dbl(data, nrow) > 10) %>%

  # visualize data
  mutate(
    panel = map_plot(data, ~
      ggplot(.x, mapping = aes(time, medListPriceSqft)) +
        geom_point() +
        geom_smooth(method = "loess") +
        ylim(housing_y[1], housing_y[2]))) %>%

  # create trelliscope application
  trelliscope(
    "all_states", "housing", nrow = 3, ncol = 5,
    path = "housing_list_price",
    state = list(sort =
      list(sort_spec("county"), sort_spec("state"))))

# # A tibble: 2,945 x 4
#                county  state              data    panel
#                <fctr> <fctr>            <list>   <list>
#  1 Los Angeles County     CA <tibble [92 x 5]> <S3: gg>
#  2        Cook County     IL <tibble [97 x 5]> <S3: gg>
#  3    Maricopa County     AZ <tibble [97 x 5]> <S3: gg>
#  4   San Diego County     CA <tibble [97 x 5]> <S3: gg>
#  5      Orange County     CA <tibble [92 x 5]> <S3: gg>
#  6       Kings County     NY <tibble [97 x 5]> <S3: gg>
#  7  Miami-Dade County     FL <tibble [97 x 5]> <S3: gg>
#  8      Dallas County     TX <tibble [97 x 5]> <S3: gg>
#  9      Queens County     NY <tibble [97 x 5]> <S3: gg>
# 10   Riverside County     CA <tibble [97 x 5]> <S3: gg>
# # ... with 2,935 more rows




# set up data
housing %>%
  group_by(county, state) %>%
  nest() %>%
  filter(map_dbl(data, nrow) > 10) %>%

  # visualize data
  mutate(
    panel = map_plot(data, ~
      ggplot(.x, mapping = aes(as.numeric(time), medListPriceSqft)) +
        geom_point() +
        geom_smooth(method = "loess") +
        ylim(housing_y[1], housing_y[2]))) %>%

  # add automatic cognostics
  add_panel_cogs() %>%

  # create trelliscope application
  trelliscope(
    "all_states_auto", "housing", nrow = 3, ncol = 5,
    path = "housing_list_price_auto",
    state = list(sort =
      list(sort_spec("county"), sort_spec("state"))))

# # A tibble: 2,945 x 11
#                county  state              data    panel    `_scagnostic`
#                <fctr> <fctr>            <list>   <list>           <list>
#  1 Los Angeles County     CA <tibble [92 x 5]> <S3: gg> <tibble [1 x 9]>
#  2        Cook County     IL <tibble [97 x 5]> <S3: gg> <tibble [1 x 9]>
#  3    Maricopa County     AZ <tibble [97 x 5]> <S3: gg> <tibble [1 x 9]>
#  4   San Diego County     CA <tibble [97 x 5]> <S3: gg> <tibble [1 x 9]>
#  5      Orange County     CA <tibble [92 x 5]> <S3: gg> <tibble [1 x 9]>
#  6       Kings County     NY <tibble [97 x 5]> <S3: gg> <tibble [1 x 9]>
#  7  Miami-Dade County     FL <tibble [97 x 5]> <S3: gg> <tibble [1 x 9]>
#  8      Dallas County     TX <tibble [97 x 5]> <S3: gg> <tibble [1 x 9]>
#  9      Queens County     NY <tibble [97 x 5]> <S3: gg> <tibble [1 x 9]>
# 10   Riverside County     CA <tibble [97 x 5]> <S3: gg> <tibble [1 x 9]>
# # ... with 2,935 more rows, and 6 more variables: `_x` <list>, `_y` <list>,
# #   `_bivar` <list>, `_smooth` <list>, `_loess` <list>, `_n` <list>




ggplot(housing, mapping = aes(time, medListPriceSqft)) +
  geom_point() +
  geom_smooth(method = "loess") +
  ylim(housing_y[1], housing_y[2]) +
  facet_trelliscope(~ county + state, auto_cog = TRUE, scales = "free")
