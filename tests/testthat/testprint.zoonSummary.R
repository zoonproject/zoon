context('print.zoonSummary')

work1 <- workflow(occurrence = UKAnophelesPlumbeus,
                  covariate = UKAir,
                  process = OneHundredBackground,
                  model = LogisticRegression,
                  output = SameTimePlaceMap)

test_that('print.zoonSummary tests', {
  
  sum_out <- capture.output(summary(work1))
  
  expect_identical(sum_out,
                   c("Data summaries", "==============", "[[1]]", "   longitude          latitude         value  ", 
                     " Min.   :-9.5639   Min.   :49.96   Min.   :1  ", " 1st Qu.:-3.7344   1st Qu.:51.63   1st Qu.:1  ", 
                     " Median :-2.5024   Median :52.48   Median :1  ", " Mean   :-2.3544   Mean   :52.78   Mean   :1  ", 
                     " 3rd Qu.:-0.2057   3rd Qu.:53.43   3rd Qu.:1  ", " Max.   : 1.2824   Max.   :57.93   Max.   :1  ", 
                     "     type                fold  ", " Length:188         Min.   :1  ", 
                     " Class :character   1st Qu.:1  ", " Mode  :character   Median :1  ", 
                     "                    Mean   :1  ", "                    3rd Qu.:1  ", 
                     "                    Max.   :1  ", "", "", "", "Model summaries", 
                     "===============", "", "[[1]]", "         Length Class  Mode     ", 
                     "model    30     glm    list     ", "code      1     -none- character", 
                     "packages  1     -none- character", ""))
})

