module SageFs.HotReloadState

/// Per-session state tracking which files are opted-in for hot-reload.
/// Default: no files watched. Users explicitly opt files in.
type T = {
  Watched: Set<string>
}

let private normalize (path: string) =
  path.Replace('\\', '/').ToLowerInvariant()

let empty : T = { Watched = Set.empty }

let watch (path: string) (state: T) : T =
  { state with Watched = state.Watched.Add(normalize path) }

let unwatch (path: string) (state: T) : T =
  { state with Watched = state.Watched.Remove(normalize path) }

let isWatched (path: string) (state: T) : bool =
  state.Watched.Contains(normalize path)

let watchMany (paths: string seq) (state: T) : T =
  { state with Watched = Set.union state.Watched (paths |> Seq.map normalize |> Set.ofSeq) }

let unwatchAll (_state: T) : T = empty

let watchAll (paths: string seq) (_state: T) : T =
  { Watched = paths |> Seq.map normalize |> Set.ofSeq }

let toggle (path: string) (state: T) : T =
  let p = normalize path
  if state.Watched.Contains(p) then
    { state with Watched = state.Watched.Remove(p) }
  else
    { state with Watched = state.Watched.Add(p) }

let watchedCount (state: T) : int =
  state.Watched.Count
