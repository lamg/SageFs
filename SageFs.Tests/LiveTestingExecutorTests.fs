module SageFs.Tests.LiveTestingExecutorTests

open System.Reflection
open Expecto
open Expecto.Flip
open SageFs.Features.LiveTesting

let executorDescriptionTests = testList "TestExecutor.description" [
  test "extracts AttributeBased description" {
    let desc = TestExecutor.description BuiltInExecutors.xunit
    match desc with
    | ProviderDescription.AttributeBased d ->
      d.Name |> Expect.equal "should be xunit" "xunit"
    | _ -> failtest "Expected AttributeBased"
  }

  test "extracts Custom description" {
    let desc = TestExecutor.description BuiltInExecutors.expecto
    match desc with
    | ProviderDescription.Custom d ->
      d.Name |> Expect.equal "should be expecto" "expecto"
    | _ -> failtest "Expected Custom"
  }
]

let builtInDescriptionTests = testList "BuiltInExecutors.descriptions" [
  test "has 5 built-in providers" {
    BuiltInExecutors.descriptions
    |> List.length
    |> Expect.equal "should have 5 providers" 5
  }

  test "includes all framework names" {
    let names =
      BuiltInExecutors.descriptions
      |> List.map (fun d ->
        match d with
        | ProviderDescription.AttributeBased a -> a.Name
        | ProviderDescription.Custom c -> c.Name)
      |> Set.ofList
    Set.count names |> Expect.equal "should have 5 unique names" 5
  }

  test "descriptions match TestProviderDescriptions.builtInDescriptions" {
    let executorDescs = BuiltInExecutors.descriptions |> List.length
    let typeDescs = TestProviderDescriptions.builtInDescriptions |> List.length
    executorDescs |> Expect.equal "both should have same count" typeDescs
  }
]

let attributeDiscoveryTests = testList "AttributeDiscovery" [
  test "discovers Expecto [Tests] properties via custom executor" {
    let testsAsm =
      System.AppDomain.CurrentDomain.GetAssemblies()
      |> Array.tryFind (fun a -> a.GetName().Name = "SageFs.Tests")
    match testsAsm with
    | Some asm ->
      match BuiltInExecutors.expecto with
      | TestExecutor.Custom ce ->
        let discovered = ce.Discover asm
        (List.length discovered > 0)
        |> Expect.isTrue "should find at least 1 expecto test property"
      | _ -> failtest "Expected Custom"
    | None ->
      skiptest "SageFs.Tests assembly not loaded"
  }

  test "attribute discovery finds nothing in assembly without test attributes" {
    let desc = {
      Name = "xunit"
      TestAttributes = ["Fact"; "Theory"]
      AssemblyMarker = "xunit.core"
    }
    let coreAsm = typeof<TestId>.Assembly
    let discovered = AttributeDiscovery.discoverInAssembly desc TestCategory.Unit coreAsm
    List.length discovered |> Expect.equal "should find 0 tests" 0
  }
]

let reflectionExecutorTests = testList "ReflectionExecutor" [
  test "executeMethod handles parameter mismatch gracefully" {
    let mi = typeof<string>.GetMethod("IsNullOrEmpty", [| typeof<string> |])
    let result = ReflectionExecutor.executeMethod mi |> Async.RunSynchronously
    match result with
    | TestResult.Failed _ -> ()
    | TestResult.Passed _ -> ()
    | other -> failtestf "Expected Passed or Failed, got %A" other
  }
]

let findExecutorTests = testList "TestOrchestrator.findExecutor" [
  test "finds xunit executor by name" {
    TestOrchestrator.findExecutor BuiltInExecutors.builtIn "xunit"
    |> Option.isSome
    |> Expect.isTrue "should find xunit"
  }

  test "finds expecto executor by name" {
    TestOrchestrator.findExecutor BuiltInExecutors.builtIn "expecto"
    |> Option.isSome
    |> Expect.isTrue "should find expecto"
  }

  test "returns None for unknown framework" {
    TestOrchestrator.findExecutor BuiltInExecutors.builtIn "jest"
    |> Option.isNone
    |> Expect.isTrue "should not find jest"
  }
]

let discoverTests = testList "TestOrchestrator.discoverTests" [
  test "discovers expecto tests from test assembly" {
    let testsAsm =
      System.AppDomain.CurrentDomain.GetAssemblies()
      |> Array.tryFind (fun a -> a.GetName().Name = "SageFs.Tests")
    match testsAsm with
    | Some asm ->
      let discovered = TestOrchestrator.discoverTests BuiltInExecutors.builtIn asm
      (List.length discovered > 0)
      |> Expect.isTrue "should discover tests"
    | None ->
      skiptest "SageFs.Tests assembly not loaded"
  }

  test "all discovered tests have framework=expecto" {
    let testsAsm =
      System.AppDomain.CurrentDomain.GetAssemblies()
      |> Array.tryFind (fun a -> a.GetName().Name = "SageFs.Tests")
    match testsAsm with
    | Some asm ->
      let discovered = TestOrchestrator.discoverTests BuiltInExecutors.builtIn asm
      discovered
      |> List.iter (fun tc ->
        tc.Framework |> Expect.equal "framework should be expecto" "expecto")
    | None ->
      skiptest "SageFs.Tests assembly not loaded"
  }
]

[<Tests>]
let allExecutorTests = testList "Provider Executors" [
  executorDescriptionTests
  builtInDescriptionTests
  attributeDiscoveryTests
  reflectionExecutorTests
  findExecutorTests
  discoverTests
]
