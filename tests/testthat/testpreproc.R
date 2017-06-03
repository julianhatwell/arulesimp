
find_char <- function(pat, dt) {
  any(!(is.na(sapply(dt, match, x = pat))))
}

test_that("char_to_na returns correctly converted data"
          , { expect_false(
                find_char(""
                  , char_to_na(
                    data.frame(
                      x = c("foo", "")
                      , b = c("", "bar")))))
              expect_false(
                find_char("_"
                  , char_to_na(char_string = "_",
                    data.frame(
                      x = c("foo", "_")
                      , b = c("", "bar")))))
          })


test_that("all_factor returns only factors"
          , { expect_true(
            all(sapply(
              all_factor(
                data.frame(
                  x = c("foo", "bar")
                  , y = c(10, 20)))
              , class) == "factor")
          )
          })

test_that("missing_values finds missing values"
          , { expect_equal(2
            , sum(
              missing_values(
              data.frame(
                x = c("foo", NA)
                , y = c(NA, "bar")))))
              expect_equal(3
              , sum(
                missing_values(
                data.frame(
                  x = c("foo", NA)
                  , y = c(NA, NA)))))
          })
