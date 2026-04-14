context("storage provider contract")

library(testthat)

test_that(
  "
  Given a file provider,
  When I use it with object storage,
  Then it satisfies the provider contract",
  {
    base.dir <- file.path(tempdir(), "memofunc-provider-contract-file")
    storage <- storage.init(storage.object.class, provider = "file", base.dir = base.dir)

    assert_storage_contract(storage)
  }
)

if (nzchar(Sys.getenv("MEMOFUNC_RUN_AZURE_TESTS"))) {
  test_that(
    "
    Given Azure Blob credentials,
    When I initialize the Azure provider,
    Then it satisfies the provider contract",
    {
      skip_if_not_installed("AzureStor")

      account <- Sys.getenv("AZURE_STORAGE_ACCOUNT")
      container <- Sys.getenv("AZURE_STORAGE_CONTAINER")
      key <- Sys.getenv("AZURE_STORAGE_KEY")
      token <- Sys.getenv("AZURE_STORAGE_TOKEN")

      if (account == "" || container == "") {
        skip("Azure Blob credentials not configured")
      }

      if (key == "" && token == "") {
        skip("Azure Blob credentials not configured")
      }

      key_arg <- if (key == "") NULL else key
      token_arg <- if (token == "") NULL else token
      prefix <- paste0("memofunc-test-", as.integer(Sys.time()))

      storage <- storage.init(
        storage.object.class,
        provider = "azure.blob",
        account = account,
        container = container,
        key = key_arg,
        token = token_arg,
        prefix = prefix
      )

      assert_storage_contract(storage)
    }
  )
}
