test_that("count functions return expected numbers", {
    # test the arcticreport package
    # baseline values were calculated from query results generated on 3/31/21

      objs <- query_objects()

      dset_count <- count_new_datasets(objs, from = "2019-01-01", to = "2020-01-01")
      expect_equal(dset_count, 493)

      obj_count <- count_data_objects(objs, from = "2019-01-01", to = "2020-01-01")
      expect_equal(obj_count, 23236)

      cre_count <- count_creators(objs, from = "2019-01-01", to = "2020-01-01")
      expect_equal(cre_count, 205)

      changed_count <- count_new_and_changed_datasets(objs, from = "2019-01-01", to = "2020-01-01")
      expect_equal(changed_count, 850)
})

test_that("count functions add up as expected", {
    # test that the count of new objects over individual time periods adds up
    # to the same count taken over the entire range of those periods

    objs <- query_objects()

    quarters_file <- system.file("extdata", "quarters.csv", package="arcticreport")
    quarters <- suppressMessages(readr::read_csv(quarters_file, progress = FALSE))

    ys <- dplyr::filter(quarters, period %in% c("y2", "y3", "y4"))

    yr_break_new <- c(); yr_break_changed <- c(); yr_break_objs <- c()
    for (i in 1:nrow(ys)){
        yr_break_new[i] <- count_new_datasets(objs, from = ys$from[i], to = ys$to[i])
        yr_break_changed[i] <- count_new_and_changed_datasets(objs, from = ys$from[i], to = ys$to[i])
        yr_break_objs[i] <- count_data_objects(objs, from = ys$from[i], to = ys$to[i])
    }

    yr_total_new <- count_new_datasets(objs, from = min(ys$from), to = max(ys$to))
    yr_total_objs <- count_data_objects(objs, from = min(ys$from), to = max(ys$to))

    expect_equal(sum(yr_break_new), yr_total_new)
    expect_equal(sum(yr_break_objs), yr_total_objs)




})
