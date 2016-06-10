data('negative')

# Test btscs -------------------------------------------------------------------
neg_df <- btscs(negative, 'y', 'tim', 'group')

test_that('btscs output validity', {
          expect_equal(nrow(neg_df), 1000)
          expect_equal(ncol(neg_df), 7)
          expect_equal(sum(neg_df[['spell']]), 9327)
})
