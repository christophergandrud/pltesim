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
data('Prestige', package = 'car')
m1 <- lm(prestige ~ education + type, data = Prestige)
ps <- b_sim(m1)

test_that('b_sim output validity', {
        expect_equal(round(sum(ps[['intercept_']])), -2605)
        expect_equal(round(sum(ps[['typeprof']])), 6199)
})

# Test linear_systematic -------------------------------------------------------
set.seed(100)
m1 <- lm(prestige ~ education + type, data = Prestige)
fitted_df <- expand.grid(education = 6:16, typewc = 1)
m1_sims <- b_sim(m1, nsim = 1000)
ls <- linear_systematic(b_sims = m1_sims, newdata = fitted_df)

test_that('linear_systematic output validity', {
    expect_equal(round(sum(ls)), 463809)
})

# Test qi_builder -------------------------------------------------------
qi_linear <- qi_builder(b_sims = m1_sims, newdata = fitted_df)

test_that('qi_builder output validity', {
    expect_equal(round(sum(qi_linear)), 463809)
})

