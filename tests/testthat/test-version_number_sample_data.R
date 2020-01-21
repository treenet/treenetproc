test_that("sample dataset has the same version number as package, i.e. is updated", {
  package_version <- utils::packageDescription("treenetproc",
                                               fields = "Version", drop = TRUE)
  dendro_data_L1_version <- dendro_data_L1$version[1]
  dendro_data_L2_version <- dendro_data_L2$version[1]
  temp_data_L1_version <- temp_data_L1$version[1]

  expect_equal(package_version, dendro_data_L1_version)
  expect_equal(package_version, dendro_data_L2_version)
  expect_equal(package_version, temp_data_L1_version)
})
