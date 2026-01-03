# choice_vector_string ( Internal )
test_that("choice_vector_string works!", {
  # returns NA for empty / missing input
  expect_equal(choice_vector_string(NULL), NA)
  expect_equal(choice_vector_string(character(0)), NA)
  # single element
  expect_equal(choice_vector_string("a"), "1, a")
  # multiple elements
  expect_equal(
    choice_vector_string(c("one", "two", "three")),
    "1, one | 2, two | 3, three"
  )
  # numeric values get coerced to character
  expect_equal(choice_vector_string(c(10, 20)), "1, 10 | 2, 20")
})
