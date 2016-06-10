data('negative')

# Test btscs -------------------------------------------------------------------
neg_df <- btscs(negative, 'y', 'tim', 'group')

test_that('btscs output validity', {
          expect_equal(nrow(neg_df), 1000)
          expect_equal(ncol(neg_df), 7)
          expect_equal(sum(neg_df[['spell']]), 9327)
})

# Test b_sim -------------------------------------------------------------------
set.seed(100)

# Linear model
data('Prestige', package = 'car')
m1 <- lm(prestige ~ education + type, data = Prestige)
m1_sims <- b_sim(m1)

# Survival model (no intercept term)
library(survival)
test1 <- list(time = c(4,3,1,1,2,2,3),
              status = c(1,1,1,0,1,1,0),
              x = c(0,2,1,1,1,0,0),
              sex = c(0,0,0,0,1,1,1))
# Fit a stratified model
m_coxph <- coxph(Surv(time, status) ~ x + strata(sex), test1)
m_coxph <- sim_coxph <- b_sim(m_coxph)

test_that('b_sim output validity', {
        expect_equal(round(sum(m1_sims[['intercept_']])), -2605)
        expect_equal(round(sum(m1_sims[['typeprof']])), 6199)
        expect_match(names(sim_coxph)[[1]], 'x')
})

# Test linear_systematic -------------------------------------------------------
set.seed(100)

# Linear model
fitted_df <- expand.grid(education = 6:16, typewc = 1)
ls_lm <- linear_systematic(b_sims = m1_sims, newdata = fitted_df)

# Survival model
ls_coxph <- linear_systematic(sim_coxph, newdata = data.frame(x = 1))

test_that('linear_systematic output validity', {
    expect_equal(round(sum(ls_lm)), 492460)
    expect_equal(round(sum(ls_coxph)), 815)
})

# Test qi_builder -------------------------------------------------------
qi_linear <- qi_builder(b_sims = m1_sims, newdata = fitted_df)

test_that('qi_builder output validity', {
    expect_equal(round(sum(qi_linear)), 492460)
})

