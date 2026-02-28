namespace SageFs.VisualStudio.ToolWindows;

using System;
using System.Runtime.Serialization;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.VisualStudio.Extensibility;
using Microsoft.VisualStudio.Extensibility.UI;

[DataContract]
internal class LiveTestingData : NotifyPropertyChangedObject, IDisposable
{
  private readonly VisualStudioExtensibility extensibility;
  private readonly Core.SageFsClient client;
  private readonly Core.LiveTestingSubscriber subscriber;

  private string enabledStatus = "⟳ Checking...";
  private string summaryText = "";
  private string testResultsText = "";
  private string recentEventsText = "";
  private string filterLabel = "All";
  private bool isEnabled;
  private Core.TestStatusFilter currentFilter = Core.TestStatusFilter.All;
  private string searchQuery = "";

  public LiveTestingData(
    VisualStudioExtensibility extensibility,
    Core.SageFsClient client,
    Core.LiveTestingSubscriber subscriber)
  {
    this.extensibility = extensibility;
    this.client = client;
    this.subscriber = subscriber;
    this.RefreshCommand = new AsyncCommand(this.RefreshAsync);
    this.ToggleCommand = new AsyncCommand(this.ToggleAsync);
    this.RunAllCommand = new AsyncCommand(this.RunAllAsync);
    this.CycleFilterCommand = new AsyncCommand(this.CycleFilterAsync);
    this.ClearSearchCommand = new AsyncCommand(this.ClearSearchAsync);

    subscriber.StateChanged += OnStateChanged;
    subscriber.SummaryChanged += OnSummaryChanged;

    _ = RefreshAsync(null, CancellationToken.None);
  }

  [DataMember] public IAsyncCommand RefreshCommand { get; }
  [DataMember] public IAsyncCommand ToggleCommand { get; }
  [DataMember] public IAsyncCommand RunAllCommand { get; }
  [DataMember] public IAsyncCommand CycleFilterCommand { get; }
  [DataMember] public IAsyncCommand ClearSearchCommand { get; }

  [DataMember]
  public string EnabledStatus
  {
    get => enabledStatus;
    set => SetProperty(ref enabledStatus, value);
  }

  [DataMember]
  public string SummaryText
  {
    get => summaryText;
    set => SetProperty(ref summaryText, value);
  }

  [DataMember]
  public string TestResultsText
  {
    get => testResultsText;
    set => SetProperty(ref testResultsText, value);
  }

  [DataMember]
  public string RecentEventsText
  {
    get => recentEventsText;
    set => SetProperty(ref recentEventsText, value);
  }

  [DataMember]
  public bool IsEnabled
  {
    get => isEnabled;
    set => SetProperty(ref isEnabled, value);
  }

  [DataMember]
  public string FilterLabel
  {
    get => filterLabel;
    set => SetProperty(ref filterLabel, value);
  }

  [DataMember]
  public string SearchQuery
  {
    get => searchQuery;
    set
    {
      if (SetProperty(ref searchQuery, value))
        UpdateTestResults();
    }
  }

  private void OnStateChanged(object? sender, Core.LiveTestState state)
  {
    UpdateFromState(state);
  }

  private void OnSummaryChanged(object? sender, Core.TestSummary summary)
  {
    var icon = summary.Failed > 0 ? "✗" : "✓";
    SummaryText = $"{icon} {summary.Passed}/{summary.Total} passed, {summary.Failed} failed";
    if (summary.Running > 0)
      SummaryText += $", {summary.Running} running";
    if (summary.Stale > 0)
      SummaryText += $", {summary.Stale} stale";
  }

  private Core.LiveTestState? lastState;

  private void UpdateFromState(Core.LiveTestState state)
  {
    lastState = state;
    IsEnabled = state.Enabled.IsOn;
    EnabledStatus = IsEnabled ? "● Live Testing ON" : "○ Live Testing OFF";

    if (state.LastSummary != null)
      OnSummaryChanged(null, state.LastSummary.Value);

    UpdateTestResults();
  }

  private void UpdateTestResults()
  {
    if (lastState == null) return;
    TestResultsText = Core.TestTreeViewModel.formatGroupedOutput(
      currentFilter, searchQuery, lastState);
  }

  private async Task RefreshAsync(object? parameter, CancellationToken ct)
  {
    try
    {
      var state = subscriber.CurrentState;
      IsEnabled = state.Enabled.IsOn;
      EnabledStatus = IsEnabled ? "● Live Testing ON" : "○ Live Testing OFF";
      if (state.LastSummary != null)
        OnSummaryChanged(null, state.LastSummary.Value);

      var eventsJson = await client.GetRecentEventsAsync(10, ct);
      if (!string.IsNullOrEmpty(eventsJson) && eventsJson != "[]")
      {
        var truncated = eventsJson.Length > 500 ? eventsJson[..500] + "..." : eventsJson;
        RecentEventsText = $"Recent Events:\n  {truncated}";
      }
      else
      {
        RecentEventsText = "No recent events.";
      }
    }
    catch (Exception ex)
    {
      EnabledStatus = "✗ Error";
      SummaryText = $"Error: {ex.Message}";
    }
  }

  private async Task ToggleAsync(object? parameter, CancellationToken ct)
  {
    try
    {
      if (IsEnabled)
        await client.DisableLiveTestingAsync(ct);
      else
        await client.EnableLiveTestingAsync(ct);
      IsEnabled = !IsEnabled;
      EnabledStatus = IsEnabled ? "● Live Testing ON" : "○ Live Testing OFF";
    }
    catch { /* best effort */ }
  }

  private async Task RunAllAsync(object? parameter, CancellationToken ct)
  {
    try
    {
      SummaryText = "⟳ Running all tests...";
      await client.RunTestsAsync("", ct);
    }
    catch { /* best effort */ }
  }

  private Task CycleFilterAsync(object? parameter, CancellationToken ct)
  {
    currentFilter = Core.TestTreeViewModel.nextFilter(currentFilter);
    FilterLabel = Core.TestTreeViewModel.filterLabel(currentFilter);
    UpdateTestResults();
    return Task.CompletedTask;
  }

  private Task ClearSearchAsync(object? parameter, CancellationToken ct)
  {
    SearchQuery = "";
    return Task.CompletedTask;
  }

  public void Dispose()
  {
    subscriber.StateChanged -= OnStateChanged;
    subscriber.SummaryChanged -= OnSummaryChanged;
  }
}
