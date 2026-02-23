namespace SageFs.VisualStudio.Services;

using System.Threading;
using System.Threading.Tasks;
using Microsoft.VisualStudio.Extensibility;
using Microsoft.VisualStudio.Extensibility.Editor;

#pragma warning disable VSEXTPREVIEW_CODELENS
#pragma warning disable VSEXTPREVIEW_OUTPUTWINDOW

/// <summary>
/// Provides "▶ Eval" CodeLens on F# functions and methods.
/// Clicking evaluates the code element's body via SageFs daemon.
/// </summary>
[VisualStudioContribution]
internal class EvalCodeLensProvider : ExtensionPart, ICodeLensProvider
{
  public TextViewExtensionConfiguration TextViewExtensionConfiguration => new()
  {
    AppliesTo =
    [
      DocumentFilter.FromGlobPattern("**/*.fs", true),
      DocumentFilter.FromGlobPattern("**/*.fsx", true),
    ],
  };

  public CodeLensProviderConfiguration CodeLensProviderConfiguration =>
    new("%SageFs.CodeLens.DisplayName%")
    {
      Priority = 200,
    };

  public Task<CodeLens?> TryCreateCodeLensAsync(
    CodeElement codeElement,
    CodeElementContext codeElementContext,
    CancellationToken token)
  {
    if (codeElement.Kind == CodeElementKind.KnownValues.Function
        || codeElement.Kind == CodeElementKind.KnownValues.Method
        || codeElement.Kind == CodeElementKind.KnownValues.Type
        || codeElement.Kind == CodeElementKind.KnownValues.Module)
    {
      return Task.FromResult<CodeLens?>(
        new EvalCodeLens(codeElement, Extensibility));
    }

    return Task.FromResult<CodeLens?>(null);
  }
}

/// <summary>
/// An invokable CodeLens that evaluates the code element in SageFs.
/// </summary>
internal class EvalCodeLens : InvokableCodeLens
{
  private readonly CodeElement codeElement;
  private readonly VisualStudioExtensibility extensibility;
  private string lastResult = "";

  public EvalCodeLens(CodeElement codeElement, VisualStudioExtensibility extensibility)
  {
    this.codeElement = codeElement;
    this.extensibility = extensibility;
  }

  public override void Dispose() { }

  public override Task<CodeLensLabel> GetLabelAsync(
    CodeElementContext codeElementContext, CancellationToken token)
  {
    var text = string.IsNullOrEmpty(lastResult)
      ? $"▶ Eval {codeElement.Description}"
      : $"✓ {lastResult}";

    return Task.FromResult(new CodeLensLabel
    {
      Text = text,
      Tooltip = "Evaluate this code element in SageFs",
    });
  }

  public override async Task ExecuteAsync(
    CodeElementContext codeElementContext,
    IClientContext clientContext,
    CancellationToken cancelToken)
  {
    var range = codeElementContext.Range;
    var code = range.CopyToString();

    if (string.IsNullOrWhiteSpace(code)) return;

    // Append ;; if not present
    if (!code.TrimEnd().EndsWith(";;"))
      code += ";;";

    var client = new Core.SageFsClient();
    var result = await client.EvalAsync(code, cancelToken);
    lastResult = result.ExitCode == 0
      ? (result.Output.Length > 60 ? result.Output[..60] + "…" : result.Output)
      : $"✗ Exit {result.ExitCode}";

    Invalidate();
  }
}

#pragma warning restore VSEXTPREVIEW_OUTPUTWINDOW
#pragma warning restore VSEXTPREVIEW_CODELENS
