context("Tab-delimited concordance")

test_that("concordance with tabs", {

  expect_equal(
    concord("hello", "e"),
    data.frame(PRE_CONTEXT = "h", MATCH = "e", POST_CONTEXT = "llo")
  )

  expect_equal(
    concord("some sentence here", "sentence", to_return = "char"),
    "some\tsentence\there"
  )

  expect_equal(
    concord("some sentence here", "e[nr]"),
    data.frame(PRE_CONTEXT = c("some s", "some sent", "some sentence h"),
               MATCH = c("en", "en", "er"),
               POST_CONTEXT = c("tence here", "ce here", "e")
               )
  )

  expect_equal(
    concord("some sentence here", "e(?=r)"),
    data.frame(PRE_CONTEXT = "some sentence h", MATCH = "e", POST_CONTEXT = "re")
  )

  expect_equal(
    concord("some sentence here", "e(?!n)", to_return = "char"),
    c("som\te\tsentence here", "some sentenc\te\there", "some sentence h\te\tre", "some sentence her\te\t")
  )

  expect_equal(
    concord(c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"), "(eight|nine|ten)", to_return = "char", extra_context = 2),
    c("six seven\teight\tnine ten", "seven eight\tnine\tten", "eight nine\tten\t")
  )

})
