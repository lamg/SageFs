namespace SageFs.Features.LiveTesting

open System
open System.Diagnostics
open System.Reflection

// --- Executor Types (IO side — functions that actually run tests) ---

/// Tier 1: Attribute-based test executor.
/// Core discovers tests via tree-sitter + reflection, executor just runs them.
type AttributeTestExecutor = {
  Description: AttributeProviderDescription
  Execute: MethodInfo -> Async<TestResult>
}

/// Tier 2: Custom test executor.
/// Provider handles its own discovery (e.g., Expecto value-based tests).
type CustomTestExecutor = {
  Description: CustomProviderDescription
  Discover: Assembly -> TestCase list
  Execute: TestCase -> Async<TestResult>
}

[<RequireQualifiedAccess>]
type TestExecutor =
  | AttributeBased of AttributeTestExecutor
  | Custom of CustomTestExecutor

module TestExecutor =
  let description (executor: TestExecutor) : ProviderDescription =
    match executor with
    | TestExecutor.AttributeBased ap -> ProviderDescription.AttributeBased ap.Description
    | TestExecutor.Custom cp -> ProviderDescription.Custom cp.Description

// --- Attribute-based discovery ---

module AttributeDiscovery =

  let private hasTestAttribute (attrs: string list) (mi: MethodInfo) : bool =
    mi.GetCustomAttributes(true)
    |> Array.exists (fun attr ->
      let attrName = attr.GetType().Name
      attrs
      |> List.exists (fun testAttr ->
        attrName = testAttr || attrName = sprintf "%sAttribute" testAttr))

  let private toTestCase (framework: string) (category: TestCategory) (mi: MethodInfo) : TestCase =
    let fullName = sprintf "%s.%s" mi.DeclaringType.FullName mi.Name
    { Id = TestId.create fullName framework
      FullName = fullName
      DisplayName = mi.Name
      Origin = TestOrigin.ReflectionOnly
      Labels = []
      Framework = framework
      Category = category }

  let discoverInAssembly
    (desc: AttributeProviderDescription)
    (category: TestCategory)
    (asm: Assembly)
    : TestCase list =
    try
      asm.GetExportedTypes()
      |> Array.collect (fun t ->
        t.GetMethods(BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.Static)
        |> Array.filter (hasTestAttribute desc.TestAttributes))
      |> Array.map (toTestCase desc.Name category)
      |> Array.toList
    with
    | :? ReflectionTypeLoadException -> []
    | :? TypeLoadException -> []

// --- Reflection-based execution ---

module ReflectionExecutor =

  let executeMethod (mi: MethodInfo) : Async<TestResult> =
    async {
      let sw = Stopwatch.StartNew()
      try
        let instance =
          if mi.IsStatic then null
          else Activator.CreateInstance(mi.DeclaringType)
        let result = mi.Invoke(instance, [||])
        match result with
        | :? Threading.Tasks.Task as task ->
          do! Async.AwaitTask task
        | _ -> ()
        sw.Stop()
        return TestResult.Passed sw.Elapsed
      with
      | :? TargetInvocationException as tie ->
        sw.Stop()
        let inner = if tie.InnerException <> null then tie.InnerException else tie :> exn
        let isAssertion =
          inner.GetType().Name.Contains("Assert")
          || inner.GetType().Name.Contains("Expect")
        if isAssertion then
          return TestResult.Failed (TestFailure.AssertionFailed inner.Message, sw.Elapsed)
        else
          return TestResult.Failed (TestFailure.ExceptionThrown (inner.Message, inner.StackTrace), sw.Elapsed)
      | ex ->
        sw.Stop()
        return TestResult.Failed (TestFailure.ExceptionThrown (ex.Message, ex.StackTrace), sw.Elapsed)
    }

// --- Built-in framework executors ---

module BuiltInExecutors =

  let xunit : TestExecutor =
    TestExecutor.AttributeBased {
      Description = {
        Name = "xunit"
        TestAttributes = ["Fact"; "Theory"]
        AssemblyMarker = "xunit.core"
      }
      Execute = ReflectionExecutor.executeMethod
    }

  let nunit : TestExecutor =
    TestExecutor.AttributeBased {
      Description = {
        Name = "nunit"
        TestAttributes = ["Test"; "TestCase"; "TestCaseSource"]
        AssemblyMarker = "nunit.framework"
      }
      Execute = ReflectionExecutor.executeMethod
    }

  let mstest : TestExecutor =
    TestExecutor.AttributeBased {
      Description = {
        Name = "mstest"
        TestAttributes = ["TestMethod"; "DataTestMethod"]
        AssemblyMarker = "Microsoft.VisualStudio.TestPlatform.TestFramework"
      }
      Execute = ReflectionExecutor.executeMethod
    }

  let tunit : TestExecutor =
    TestExecutor.AttributeBased {
      Description = {
        Name = "tunit"
        TestAttributes = ["Test"]
        AssemblyMarker = "TUnit.Core"
      }
      Execute = ReflectionExecutor.executeMethod
    }

  let expecto : TestExecutor =
    TestExecutor.Custom {
      Description = {
        Name = "expecto"
        AssemblyMarker = "Expecto"
      }
      Discover = fun asm ->
        try
          asm.GetExportedTypes()
          |> Array.collect (fun t ->
            t.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
            |> Array.filter (fun pi ->
              pi.GetCustomAttributes(true)
              |> Array.exists (fun attr ->
                attr.GetType().Name = "TestsAttribute"))
            |> Array.map (fun pi ->
              let fullName = sprintf "%s.%s" t.FullName pi.Name
              { Id = TestId.create fullName "expecto"
                FullName = fullName
                DisplayName = pi.Name
                Origin = TestOrigin.ReflectionOnly
                Labels = []
                Framework = "expecto"
                Category = TestCategory.Unit }))
          |> Array.toList
        with
        | :? ReflectionTypeLoadException -> []
        | :? TypeLoadException -> []
      Execute = fun _testCase ->
        async { return TestResult.NotRun }
    }

  let builtIn = [ xunit; nunit; mstest; tunit; expecto ]

  let descriptions : ProviderDescription list =
    builtIn |> List.map TestExecutor.description

// --- Test orchestration ---

module TestOrchestrator =

  let discoverTests
    (executors: TestExecutor list)
    (asm: Assembly)
    : TestCase list =
    executors
    |> List.collect (fun executor ->
      match executor with
      | TestExecutor.AttributeBased ae ->
        AttributeDiscovery.discoverInAssembly ae.Description TestCategory.Unit asm
      | TestExecutor.Custom ce ->
        ce.Discover asm)

  let findExecutor (executors: TestExecutor list) (framework: string) : TestExecutor option =
    executors
    |> List.tryFind (fun e ->
      match e with
      | TestExecutor.AttributeBased ae -> ae.Description.Name = framework
      | TestExecutor.Custom ce -> ce.Description.Name = framework)

  let executeOne
    (executors: TestExecutor list)
    (testCase: TestCase)
    : Async<TestRunResult> =
    async {
      let! result =
        match findExecutor executors testCase.Framework with
        | Some (TestExecutor.Custom ce) ->
          ce.Execute testCase
        | Some (TestExecutor.AttributeBased _) ->
          async { return TestResult.NotRun }
        | None ->
          async { return TestResult.NotRun }
      return {
        TestId = testCase.Id
        TestName = testCase.DisplayName
        Result = result
        Timestamp = DateTimeOffset.UtcNow
      }
    }

  let executeFiltered
    (executors: TestExecutor list)
    (maxParallelism: int)
    (tests: TestCase array)
    : Async<TestRunResult array> =
    async {
      let semaphore = new Threading.SemaphoreSlim(maxParallelism)
      let! results =
        tests
        |> Array.map (fun tc ->
          async {
            do! Async.AwaitTask (semaphore.WaitAsync())
            try
              return! executeOne executors tc
            finally
              semaphore.Release() |> ignore
          })
        |> Async.Parallel
      return results
    }
