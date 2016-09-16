# Use iris for unit testing
data(iris)

context("Input testing")

test_that("Erroneous input returns error", {
    expect_error(aml_k_means(iris, -1))
    expect_error(aml_k_means(iris, 4.1))
    expect_error(aml_k_means(5, 1))
    expect_error(aml_k_means(data.frame(), 1))
    expect_error(aml_k_means(matrix(1:5), 1))
    expect_error(aml_k_means(data.frame(letters), 1))
    expect_error(aml_k_means(data.frame(letters[1:5], 1:5), 1))
})

context("Internal algorithm testing")

test_that("Initial assignments are returning integers", {
    all_initial_assignment_tests = sapply(1:20, function(k){
        initial_assignments_big = .find_initial_assignments(100, k)
        initial_assignments_small = .find_initial_assignments(1, k)
        (max(initial_assignments_small) <= k) &
        (min(initial_assignments_small) >= 1) &
        (max(initial_assignments_big) <= k) &
        (min(initial_assignments_big) >= 1)
    })
    expect_true(all(all_initial_assignment_tests))
})

test_that("Seeding initial assignment function returns reproducible results", {
    k = 3
    n = 100
    set.seed(1337)
    seeded_numbers = sample(1:k, n, replace = TRUE)
    expect_true(all(seeded_numbers == .find_initial_assignments(n, k)))
    set.seed(8675309)
    seeded_numbers = sample(1:k, n, replace = TRUE)
    expect_true(all(seeded_numbers == 
                    .find_initial_assignments(n, k, seed = 8675309)))
})

test_that("Centroid calculation returns expected means", {
    expect_true(all(.calculate_centroids(data.frame(c(1, 1)), c(1, 2)) == 
                    c(1, 1)))
    expect_true(all(.calculate_centroids(data.frame(c(1, 1)), c(1, 1)) == 1))
    expect_true(all(.calculate_centroids(data.frame(rep(10, 5)), 1:5) == 
                    rep(10, 5)))
    expect_true(all(.calculate_centroids(iris[, 1:4], 
                                         as.numeric(iris$Species)) == 
                    data.frame(Sepal.Length = c(5.006, 5.936, 6.588),
                               Sepal.Width  = c(3.428, 2.770, 2.974),
                               Petal.Length = c(1.462, 4.260, 5.552),
                               Petal.Width  = c(0.246, 1.326, 2.026))))
    expect_true(.calculate_centroids(data.frame(c(500)), c(1)) == 500)
})

test_that("Distances from centroids are correctly calculated", {
    t1 = data.frame(a = c(1, 2, 3))
    c1 = data.frame(a = c(1))
    d1 = .calculate_distances_from_centroids(t1, c1)
    d1_expected = list(c(0, 1, 2))
    expect_true(class(d1) == "list")
    expect_true(all(d1[[1]] == d1_expected[[1]]))

    data = iris[, 1:4]
    roids = .calculate_centroids(data, rep(1:3, 50))
    d2 = .calculate_distances_from_centroids(data, roids)
    expect_true(length(d2) == 3)
    round_digits = 6

    for(k in 1:3){
        coldiffs = lapply(1:ncol(data), function(i){
            (data[, i] - roids[k, i])^2
        })

        coldiff_matrix = do.call(cbind, coldiffs)

        expect_true(
            all(round(sqrt(apply(coldiff_matrix, 1, sum)), round_digits) == 
                    round(d2[[k]], round_digits))
        )
    }
})

test_that("New label calculations based on distance are correct", {
    l1 = list(c(1, 2, 3), c(3, 1, 1))
    expect_true(all(.calculate_new_labels(l1) == c(1, 2, 2)))
    l2 = list(c(1, 1, 1), c(1, 1, 1))
    expect_true(all(.calculate_new_labels(l2) == c(1, 1, 1)))
    data = iris[1:12, 1:4]
    roids = .calculate_centroids(data, rep(1:3, 4))
    d2 = .calculate_distances_from_centroids(data, roids)
    expect_true(
        all(.calculate_new_labels(d2) == as.integer(c(2, 1, 1, 1, 2, 2, 3, 2, 1,
                                                      1, 2, 3)))
    )
})


context("K-means output testing")

test_that("K-means algorithm outputs appropriately", {
    vect = aml_k_means(iris[, 1:4], 3)
    expect_true(all(vect %in% 1:3))
    expect_true(length(vect) == nrow(iris))
    expect_true(all(!is.null(vect)))
})


