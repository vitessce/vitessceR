library(vitessceR)

test_that("CsvWrapper", {
  w <- CsvWrapper$new(
    csv_path = "tests/data/test.csv",
    data_type = "obsEmbedding",
    options = obj_list(
        obsIndex = "index",
        obsEmbedding = c("UMAP_1", "UMAP_2")
    ),
    coordination_values = obj_list(
        embeddingType = "UMAP"
    )
  )
  w$local_csv_uid <- "some_uuid.csv"

  file_def_creator <- w$make_file_def_creator("A", "0")
  file_def <- file_def_creator("http://localhost:8000")

  expect_equal(file_def, list(
    fileType = "obsEmbedding.csv",
    url = "http://localhost:8000/A/0/some_uuid.csv",
    options = obj_list(
        obsIndex = "index",
        obsEmbedding = c("UMAP_1", "UMAP_2")
    ),
    coordinationValues = obj_list(
        embeddingType = "UMAP"
    )
  ))
})

test_that("CsvWrapper with base_dir", {
  w <- CsvWrapper$new(
    csv_path = "test.csv",
    data_type = "obsEmbedding",
    options = obj_list(
        obsIndex = "index",
        obsEmbedding = c("UMAP_1", "UMAP_2")
    ),
    coordination_values = obj_list(
        embeddingType = "UMAP"
    )
  )
  w$base_dir <- "tests/data"
  w$local_csv_uid <- "some_uuid.csv"

  file_def_creator <- w$make_file_def_creator("A", "0")
  file_def <- file_def_creator("http://localhost:8000")

  expect_equal(file_def, list(
    fileType = "obsEmbedding.csv",
    url = "http://localhost:8000/test.csv",
    options = obj_list(
        obsIndex = "index",
        obsEmbedding = c("UMAP_1", "UMAP_2")
    ),
    coordinationValues = obj_list(
        embeddingType = "UMAP"
    )
  ))
})
