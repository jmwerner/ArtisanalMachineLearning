# Use iris for unit testing
data(iris)

################################################################################
context("k-means input testing")
################################################################################

test_that("Erroneous k-means input returns error", {
    expect_error(aml_k_means(iris, -1))
    expect_error(aml_k_means(iris, 4.1))
    expect_error(aml_k_means(5, 1))
    expect_error(aml_k_means(data.frame(), 1))
    expect_error(aml_k_means(matrix(1:5), 1))
    expect_error(aml_k_means(data.frame(letters), 1))
    expect_error(aml_k_means(data.frame(letters[1:5], 1:5), 1))
})

################################################################################
context("k-means internal algorithm testing")
################################################################################

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

################################################################################
context("k-means output testing")
################################################################################

test_that("k-means algorithm outputs appropriately", {
    k_means_execution = aml_k_means(iris[, 1:4], 3)

    vect = k_means_execution$labels
    expect_true(all(vect %in% 1:3))
    expect_true(length(vect) == nrow(iris))
    expect_true(all(!is.null(vect)))

    expect_true(identical(k_means_execution$data, iris[, 1:4]))
    expect_true(k_means_execution$iter > 0)
    expect_true(all(!is.na(k_means_execution$centroids)))
    expect_true(k_means_execution$k > 0)
    expect_true(!is.na(k_means_execution$k))

    expect_true(sum("aml_k_means" == class(k_means_execution)) == 1)

    expect_warning(aml_k_means(iris[,1:3], 10, maximum_iterations = 1))

    expect_true(is.data.frame(k_means_execution$data))
})

test_that("k-means plotting functionality works", {
    # Test 2 column input plotting first
    k_means = aml_k_means(iris[, 1:2], 4)
    ggp = .make_ggplot_object(k_means)

    expect_true(all(is.na(ggp) == FALSE))
    expect_true(all(c("ggplot", "gg") %in% class(ggp)))
    expect_true(ncol(ggp$data) == (ncol(k_means$data) + 1))
    expect_true(identical(ggp$data[,1:2], k_means$data))
    expect_true(all(as.numeric(ggp$data$Labels) == k_means$labels))
    expect_true(names(k_means$data)[1] == ggp$labels$x)
    expect_true(names(k_means$data)[2] == ggp$labels$y)
    expect_true(all(k_means$labels %in% 1:4))
    expect_true(ggp$labels$colour == "Labels" & ggp$labels$shape == "Labels")

    # Test .convert_to_ggname
    expect_identical(.convert_to_ggname(1:5), 1:5)
    expect_true(.convert_to_ggname("hello") == "`hello`")
    expect_true(.convert_to_ggname('hello, i like "cheese"') == '`hello, i like \"cheese\"`')

    # Test alm_k_means with dimensionality reduction in plotting
    k_means_2 = aml_k_means(iris[, 1:4], 5)
    ggp_2 = .make_ggplot_object(k_means_2)

    expect_true(all(is.na(ggp_2) == FALSE))
    expect_true(all(c("ggplot", "gg") %in% class(ggp_2)))
    expect_true(ncol(ggp_2$data) == 3)

    prcomp_object = prcomp(iris[, 1:4], center = TRUE, scale = TRUE)
    test_frame = data.frame(prcomp_object$x[, 1:2])
    names(test_frame) = c("Principal Component I", "Principal Component II")

    expect_true(identical(ggp_2$data[, 1:2], test_frame))

    expect_true(all(as.numeric(ggp_2$data$Labels) == k_means_2$labels))
    expect_true(names(ggp_2$data)[1] == "Principal Component I")
    expect_true(names(ggp_2$data)[2] == "Principal Component II")
    expect_true(all(k_means$labels %in% 1:5))
    expect_true(ggp_2$labels$colour == "Labels" & ggp_2$labels$shape == "Labels")

    ggp_2 = .make_ggplot_object(k_means_2, plot_centroids = TRUE)

    expect_true(all(is.na(ggp_2) == FALSE))
    expect_true(all(c("ggplot", "gg") %in% class(ggp_2)))
    expect_true(ncol(ggp_2$data) == 3)

    prcomp_object = prcomp(iris[, 1:4], center = TRUE, scale = TRUE)
    test_frame = data.frame(prcomp_object$x[, 1:2])
    names(test_frame) = c("Principal Component I", "Principal Component II")

    expect_true(identical(ggp_2$data[, 1:2], test_frame))

    expect_true(all(as.numeric(ggp_2$data$Labels) == k_means_2$labels))
    expect_true(names(ggp_2$data)[1] == "Principal Component I")
    expect_true(names(ggp_2$data)[2] == "Principal Component II")
    expect_true(all(k_means$labels %in% 1:5))
    expect_true(ggp_2$labels$colour == "Labels" & ggp_2$labels$shape == "Labels")

    png("test1.png")
        plot(k_means)
    dev.off()

    png("test2.png")
        plot(k_means_2)
    dev.off()

    png("test3.png")
        plot(k_means_2, tsne_reduction = TRUE)
    dev.off()

    expect_true(file.exists("test1.png"))
    file.remove("test1.png")

    expect_true(file.exists("test2.png"))
    file.remove("test2.png")

    expect_true(file.exists("test3.png"))
    file.remove("test3.png")

    png("test1.png")
        plot(k_means, plot_centroids = TRUE)
    dev.off()

    png("test2.png")
        plot(k_means_2, plot_centroids = TRUE)
    dev.off()

    png("test3.png")
        plot(k_means_2, plot_centroids = TRUE, tsne_reduction = TRUE)
    dev.off()

    expect_true(file.exists("test1.png"))
    file.remove("test1.png")

    expect_true(file.exists("test2.png"))
    file.remove("test2.png")

    expect_true(file.exists("test3.png"))
    file.remove("test3.png")
})


