.AsMP <- function(f) { class(f) <- 'mp'; f }

# defines `Objs` in the global environment (as if created at the console), runs `code`, then removes them
.WithGlobals <- function(Objs, code) {
  Objs <- lapply(Objs, \(x) { if (is.function(x)) environment(x) <- globalenv(); x })
  list2env(Objs, globalenv())
  on.exit(rm(list = names(Objs), envir = globalenv()))
  force(code)
}

test_that(".MakeSelfContained bundles global helpers, nested helpers and global variables", {
  MP <- .WithGlobals(
    list(
      hA = function(x) x * 2,
      hC2 = function(x) x + scaleD,
      hC1 = function(x) hC2(x),
      scaleB = 10,
      scaleD = 100,
      hE = function(x) x - 1,
      MyMP = .AsMP(function(Data) c(hA(Data), Data * scaleB, hC1(Data), sapply(Data, hE)))
    ),
    .MakeSelfContained(get('MyMP', globalenv()))
  )
  expect_false(any(c('hA', 'hC1', 'hC2', 'scaleB', 'scaleD', 'hE') %in% ls(globalenv())))
  expect_equal(MP(1), c(2, 10, 101, 0))
  expect_setequal(ls(environment(MP)), c('hA', 'hC1', 'hC2', 'scaleB', 'scaleD', 'hE'))
  expect_identical(environment(environment(MP)$hC1), environment(MP))
})

test_that(".MakeSelfContained keeps variables captured by closure-factory MPs", {
  Make <- function(s) { force(s); .AsMP(function(Data) Data * s) }
  MP1 <- .MakeSelfContained(Make(0.5))
  MP2 <- .MakeSelfContained(Make(3))
  expect_equal(MP1(2), 1)
  expect_equal(MP2(2), 6)
})

test_that(".MakeSelfContained does not copy MSEtool, base or default-package objects", {
  MP <- .MakeSelfContained(.AsMP(function(Data) {
    a <- CurrentCatch(Data)
    stats::median(c(pi, length(a)))
  }))
  expect_length(ls(environment(MP)), 0)
  expect_identical(parent.env(environment(MP)), asNamespace('MSEtool'))
})

test_that(".MakeSelfContained keeps SetMPArgs values and wrapped legacy MPs working", {
  MP <- .WithGlobals(
    list(BaseMP = .AsMP(function(Data, scale = 1) Data * scale)),
    .MakeSelfContained(SetMPArgs(get('BaseMP', globalenv()), scale = 4))
  )
  expect_equal(MP(2), 8)

  Legacy <- .MakeSelfContained(.WrapLegacyMP(structure(function(x, Data, reps, ...) 1, class = 'MP')))
  expect_length(ls(environment(Legacy)), 0)
})

test_that(".MakeSelfContained warns when bundled global objects are large", {
  expect_warning(
    .WithGlobals(
      list(BigObj = rnorm(1e4), MyMP = .AsMP(function(Data) Data + length(BigObj))),
      .MakeSelfContained(get('MyMP', globalenv()), MaxSize = 1000)
    ),
    'BigObj'
  )
})

test_that("Project accepts a named-list `parallel` for hist objects and validates it", {
  skip_on_cran()
  om <- SingleStockOM
  om@nSim <- 3
  hist <- Simulate(om, silent = TRUE)
  expect_error(Project(hist, MPs = 'NoFishing', parallel = 'yes', silent = TRUE), 'TRUE')
  ref <- Project(hist, MPs = 'NoFishing', silent = TRUE)
  expect_message(
    mse <- Project(hist, MPs = 'NoFishing', parallel = list(NoFishing = FALSE)),
    'Per-MP'
  )
  expect_equal(Landings(mse), Landings(ref))
})

test_that("custom MPs with global dependencies project identically in parallel", {
  skip_on_cran()
  skip_if_not_installed('future')
  om <- SingleStockOM
  om@nSim <- 4
  hist <- Simulate(om, silent = TRUE)

  .WithGlobals(
    list(
      hC2 = function(x) x * scaleD,
      hC1 = function(x) hC2(x),
      scaleD = 0.5,
      MyMP = .AsMP(function(Data, ...) {
        a <- CurrentCatch(Data, ...)
        a@TAC <- hC1(a@TAC)
        a
      })
    ),
    {
      seq <- Project(hist, MPs = 'MyMP', silent = TRUE)
      SetupParallel(workers = 2, backend = 'multisession', silent = TRUE)
      par <- try(Project(hist, MPs = 'MyMP', parallel = TRUE, silent = TRUE))
      suppressMessages(DisableParallel())
    }
  )
  expect_null(par@Log$error)
  expect_equal(Landings(par), Landings(seq))
})
