module Decode exposing
  ( users
  , User
  , clips
  , hosts
  )

import Persist exposing (Clip, UserId)

import Twitch.Helix.User as User
import Twitch.Helix.Clip as Clip
import Twitch.Kraken.Host as Host

import Json.Decode exposing (..)

type alias User =
  { id : UserId
  , login : String
  , displayName : String
  }

users : Decoder (List User)
users = User.response user

user : Decoder User
user =
  map3 User
    User.id
    User.login
    User.displayName

clips : Decoder (List Clip)
clips = Clip.response clip

clip : Decoder Clip
clip =
  succeed Clip
    |> map2 (|>) Clip.id
    |> map2 (|>) Clip.url
    |> map2 (|>) Clip.embedUrl
    |> map2 (|>) Clip.broadcasterId
    |> map2 (|>) (Clip.broadcasterName |> map Just)
    |> map2 (|>) duration
    |> map2 (|>) thumbnail
    |> map2 (|>) (Clip.createdAt |> map Just)

duration : Decoder (Maybe Int)
duration =
  float
    |> map (\x -> round(x * 1000))
    |> field "duration"
    |> maybe

thumbnail : Decoder (Maybe String)
thumbnail =
  Clip.thumbnailUrl
    |> map (
      --Debug.log "thumnail"
      String.replace "-preview-480x272.jpg" ".mp4"
      >> Just
      -->> Debug.log "video url"
    )

hosts : Decoder (List UserId)
hosts = Host.response Host.hostId
