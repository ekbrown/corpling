context("Tab-delimited concordance")

test_that("concordance with tabs", {

  expect_equal(
    concord("hello", "e"),
    data.frame(PRE_CONTEXT = "h", MATCH = "e", POST_CONTEXT = "llo")
  )

  expect_equal(
    concord("some sentence here", "sentence"),
    data.frame(PRE_CONTEXT = "some", MATCH = "sentence", POST_CONTEXT = "here")
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
    concord("some sentence here", "e(?!n)"),
    data.frame(
      PRE_CONTEXT = c("som", "some sentenc", "some sentence h", "some sentence her"),
      MATCH = c("e", "e", "e", "e"),
      POST_CONTEXT = c("sentence here", "here", "re", "")
    )
  )

  expect_equal(
    concord(c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"), "(eight|nine|ten)", extra_context = 2),
    data.frame(
      PRE_CONTEXT = c("six seven", "seven eight", "eight nine"),
      MATCH = c("eight", "nine", "ten"),
      POST_CONTEXT = c("nine ten", "ten", "")
    )
  )

  expect_equal(
    concord("some sentence here", "sentence", shorten = 2),
    data.frame(PRE_CONTEXT = "me", MATCH = "sentence", POST_CONTEXT = "he")
  )

})
