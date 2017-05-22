library(DBI)
library(odbc)
library(testthat)
# library(RODBCDBI)


# Prepare test data -----------------------------------------------------------------------------------------------

test.data <- list()

# data.frames with different encodings
test.data$ASCII  <- data.frame(col1 = c("aaa", "123", "all ASCII"), stringsAsFactors = FALSE)
test.data$latin1 <- data.frame(col1 = iconv(enc2utf8(c("äöü as latin1", "Voilà", "c'ést ça")), "UTF-8", "latin1"), stringsAsFactors = FALSE)
test.data$UTF8   <- data.frame(col1 = enc2utf8(c("äöü as UTF-8", "Voilà", "c'ést")), stringsAsFactors = FALSE)
test.data$mixed  <- data.frame(col1 = c(iconv(enc2utf8("äöüß as latin1"), "UTF-8", "latin1"), enc2utf8("äöüß as UTF-8"), "aaa as unknown"), stringsAsFactors = FALSE)

# Ensure correct test set-up
stopifnot(Encoding(test.data$ASCII$col1)  == c(rep("unknown", 3)))
stopifnot(Encoding(test.data$latin1$col1) == c(rep("latin1", 3)))
stopifnot(Encoding(test.data$UTF8$col1)   == c(rep("UTF-8", 3)))
stopifnot(Encoding(test.data$mixed$col1)  == c("latin1", "UTF-8", "unknown"))



# Loop tests over all databases configurations to be tested -------------------------------------------------------

# con <- dbConnect(odbc::odbc(), dsn = "PostgreSQL")
con <- dbConnect(odbc::odbc(), dsn = "PostgreSQL", encoding = "UTF-8")
# con <- dbConnect(odbc::odbc(), dsn = "PostgreSQL", encoding = "latin1")
# con <- dbConnect(odbc::odbc(), dsn = "PostgreSQL", encoding = "")



# Test each test data.frame ---------------------------------------------------------------------------------------

for (i in 1:(length(test.data))) {
print(i)
  data <- test.data[[i]]
  test.data.name <- names(test.data)[i]

  context(paste("Encoding test data:", test.data.name))

  # Prepare data base -----------------------------------------------------------------------------------------------

  table.name <- "encoding_test"

  if (dbExistsTable(con, table.name)) { dbRemoveTable(con, table.name) }

  sql <- paste0("CREATE TABLE ", table.name, "(col1 nvarchar(255) NULL)")
  dbGetQuery(con, sql)



  # Perform the tests -----------------------------------------------------------------------------------------------

  # TODO
  #  Error in result_insert_dataframe(rs@ptr, values) :
  #  Invalid multibyte sequence
  # at Table.R#73
  dbWriteTable(con, table.name, data, append = TRUE)

  result <- dbReadTable(con, table.name)

  try(test_that(paste("writing encoded", test.data.name, "data and reading it again keeps the strings unchanged"), {

    # expect_equal(result, data)

    # Compare the cells 1:1 to get a values that differ in case of discrepancies
    expect_equal(result$col1[1], data$col1[1])
    expect_equal(result$col1[2], data$col1[2])
    expect_equal(result$col1[3], data$col1[3])

  }))

}



# Clean-up --------------------------------------------------------------------------------------------------------

dbDisconnect(con)



