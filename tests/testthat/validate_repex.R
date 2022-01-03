# Reference data in an environment
e <- new.env()
e$test <- women
v <- validator(weight == test$weight)
ans <- confront(women, v, e)
summary(ans  )

# Reference data in an environment
df1 <- data.frame(names = c("bat", "baf", "caf"))
e <- new.env()
e$test <- list(names = data.frame(names = c("bat", "cat")))
v <- validate::validator(names %in% test[["names"]][["names"]])

e$test <- data.frame(names = c("bat", "cat"))
v <- validate::validator(names %in% db$names)

ans <- validate::confront(df1, v, list(db = data.frame(names = c("bat", "cat"))))
validate::summary(ans  )
