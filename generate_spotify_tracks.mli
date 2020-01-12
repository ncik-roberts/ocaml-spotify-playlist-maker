open Core
open Async

val tracks
  :  Npr.Song.t Or_error.t Pipe.Reader.t
  -> access_token:_ Spotify.Access_token.t
  -> [ `Found of Npr.Song.t * Spotify.Track.t | `Skipping of Npr.Song.t ]
       Or_error.t
       Pipe.Reader.t
