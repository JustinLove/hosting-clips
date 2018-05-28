module Twitch.Helix.Decode exposing
  ( Token
  , User
  , LiveStream
  , Game
  , Follow
  , Video
  , VideoType(..)
  , Viewable(..)
  , Clip
  , token
  , users
  , liveStreams
  , games
  , follows
  , videos
  , clips
  , sampleToken
  , sampleUser
  , sampleLiveStream
  , sampleGame
  , sampleFollow
  , sampleVideo
  , sampleClip
  )

import Twitch.Parse as Parse

import Json.Decode exposing (..)
import Parser
import Date
import Time exposing (Time)

sampleToken = """{ sub = "12345678", iss = "https://api.twitch.tv/api", aud = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx", exp = 1511110246, iat = 1511109346 }"""

type alias Token =
  { sub : String
  , iss : String
  , aud : String
  , exp : Int
  , iat : Int
  }

token : Decoder Token
token =
  map5 Token
    (field "sub" string)
    (field "iss" string)
    (field "aud" string)
    (field "exp" int)
    (field "iat" int)

sampleUser = """
{"data":[{
   "id":"44322889",
   "login":"dallas",
   "display_name":"dallas",
   "type":"staff",
   "broadcaster_type":"",
   "description":"Just a gamer playing games and chatting. :)",
   "profile_image_url":"https://static-cdn.jtvnw.net/jtv_user_pictures/dallas-profile_image-1a2c906ee2c35f12-300x300.png",
   "offline_image_url":"https://static-cdn.jtvnw.net/jtv_user_pictures/dallas-channel_offline_image-1a2c906ee2c35f12-1920x1080.png",
   "view_count":191836881,
   "email":"login@provider.com"
}]}
"""

type alias User =
  { id : String
  , login : String
  , displayName : String
  , userType : String
  , broadcasterType : String
  , description : String
  , profileImageUrl : String
  , offlineImageUrl : String
  , viewCount : Int
  , email : Maybe String
  }

users : Decoder (List User)
users =
  field "data" (list user)

user : Decoder User
user =
  succeed User
    |> map2 (|>) (field "id" string)
    |> map2 (|>) (field "login" string)
    |> map2 (|>) (field "display_name" string)
    |> map2 (|>) (field "type" string)
    |> map2 (|>) (field "broadcaster_type" string)
    |> map2 (|>) (field "description" string)
    |> map2 (|>) (field "profile_image_url" string)
    |> map2 (|>) (field "offline_image_url" string)
    |> map2 (|>) (field "view_count" int)
    |> map2 (|>) (maybe (field "email" string))

sampleLiveStream = """
{"data":
   [
      {
         "id":"26007494656",
         "user_id":"23161357",
         "game_id":"417752",
         "community_ids":[
            "5181e78f-2280-42a6-873d-758e25a7c313",
            "848d95be-90b3-44a5-b143-6e373754c382",
            "fd0eab99-832a-4d7e-8cc0-04d73deb2e54"
         ],
         "type":"live",
         "title":"Hey Guys, It's Monday - Twitter: @Lirik",
         "viewer_count":32575,
         "started_at":"2017-08-14T16:08:32Z",
         "language":"en",
         "thumbnail_url":"https://static-cdn.jtvnw.net/previews-ttv/live_user_lirik-{width}x{height}.jpg"
      }, 
   ], 
   "pagination":{"cursor":"eyJiIjpudWxsLCJhIjp7Ik9mZnNldCI6MjB9fQ=="}
}
"""

type alias LiveStream =
  { channelId : String
  , userId : String
  , gameId : String
  , title : String
  , viewerCount : Int
  , thumbnailUrl : String
  }

liveStreams : Decoder (List LiveStream)
liveStreams =
  field "data" (list stream)

stream : Decoder LiveStream
stream =
  map6 LiveStream
    (field "id" string)
    (field "user_id" string)
    (field "game_id" string)
    (field "title" string)
    (field "viewer_count" int)
    (field "thumbnail_url" string)

sampleGame = """
{"data":
   [
      {
         "id":"493057",
         "name":"PLAYERUNKNOWN'S BATTLEGROUNDS",
         "box_art_url":"https://static-cdn.jtvnw.net/ttv-boxart/PLAYERUNKNOWN%27S%20BATTLEGROUNDS-{width}x{height}.jpg"
      }
   ]
}
"""

type alias Game =
  { id : String
  , name : String
  , boxArtUrl : String
  }

games : Decoder (List Game)
games =
  field "data" (list game)

game : Decoder Game
game =
  map3 Game
    (field "id" string)
    (field "name" string)
    (field "box_art_url" string)

sampleFollow = """
{"data":
   [
      {
         "from_id":"171003792",
         "to_id":"23161357",
         "followed_at":"2017-08-22T22:55:24Z"
      },
      {
         "from_id":"113627897",
         "to_id":"23161357",
         "followed_at":"2017-08-22T22:55:04Z"
      },
   ],
   "pagination":{"cursor":"eyJiIjpudWxsLCJhIjoiMTUwMzQ0MTc3NjQyNDQyMjAwMCJ9"}
}
"""

type alias Follow =
  { from_id : String
  , to_id : String
  }

follows : Decoder (List Follow)
follows =
  field "data" (list follow)

follow : Decoder Follow
follow =
  map2 Follow
    (field "from_id" string)
    (field "to_id" string)

sampleVideo = """
{
  "data": [{
    "id": "234482848",
    "user_id": "67955580",
    "title": "-",
    "description": "",
    "created_at": "2018-03-02T20:53:41Z",
    "published_at": "2018-03-02T20:53:41Z",
    "url": "https://www.twitch.tv/videos/234482848",
    "thumbnail_url": "https://static-cdn.jtvnw.net/s3_vods/bebc8cba2926d1967418_chewiemelodies_27786761696_805342775/thumb/thumb0-%{width}x%{height}.jpg",
    "viewable": "public",
    "view_count": 142,
    "language": "en",
    "type": "archive",
    "duration": "3h8m33s"
  }],
  "pagination":{"cursor":"eyJiIjpudWxsLCJhIjoiMTUwMzQ0MTc3NjQyNDQyMjAwMCJ9"}
}"""

type alias Video =
  { id : String
  , userId : String
  , title : String
  , description : String
  , createdAt : Time
  , publishedAt : Time
  , url : String
  , thumbnailUrl : String
  , viewable : Viewable
  , viewCount : Int
  , language : String
  , videoType : VideoType
  , duration : Time
  }

videos : Decoder (List Video)
videos =
  field "data" (list video)

video : Decoder Video
video =
  succeed Video
    |> map2 (|>) (field "id" string)
    |> map2 (|>) (field "user_id" string)
    |> map2 (|>) (field "title" string)
    |> map2 (|>) (field "description" string)
    |> map2 (|>) (field "created_at" timeStamp)
    |> map2 (|>) (field "published_at" timeStamp)
    |> map2 (|>) (field "url" string)
    |> map2 (|>) (field "thumbnail_url" string)
    |> map2 (|>) (field "viewable" viewable)
    |> map2 (|>) (field "view_count" int)
    |> map2 (|>) (field "language" string)
    |> map2 (|>) (field "type" videoType)
    |> map2 (|>) (field "duration" duration)

type Viewable
  = Public
  | Private

viewable : Decoder Viewable
viewable =
  string
    |> map (\s -> case s of
      "public" -> Public
      "private" -> Private
      _ -> Private
    )

type VideoType
  = Upload
  | Archive
  | Highlight
  | Other String

videoType : Decoder VideoType
videoType =
  string
    |> map (\s -> case s of
      "upload" -> Upload
      "archive" -> Archive
      "highlight" -> Highlight
      _ -> Other s
    )


sampleClip = """
{
  "data":
  [{
    "id": "AwkwardHelplessSalamanderSwiftRage",
    "url": "https://clips.twitch.tv/AwkwardHelplessSalamanderSwiftRage",
    "embed_url": "https://clips.twitch.tv/embed?clip=AwkwardHelplessSalamanderSwiftRage",
    "broadcaster_id": "67955580",
    "creator_id": "53834192",
    "video_id": "205586603",
    "game_id": "488191",
    "language": "en",
    "title": "babymetal",
    "view_count": 10,
    "created_at": "2017-11-30T22:34:18Z",
    "thumbnail_url": "https://clips-media-assets.twitch.tv/157589949-preview-480x272.jpg"
  }]
}
"""

type alias Clip =
  { id : String
  , url : String
  , embedUrl : String
  , broadcasterId : String
  , creatorId : String
  , videoId : String
  , gameId : String
  , language : String
  , title : String
  , viewCount : Int
  , createdAt : Time
  , thumbnailUrl : String
  }

clips : Decoder (List Clip)
clips =
  field "data" (list clip)

clip : Decoder Clip
clip =
  succeed Clip
    |> map2 (|>) (field "id" string)
    |> map2 (|>) (field "url" string)
    |> map2 (|>) (field "embed_url" string)
    |> map2 (|>) (field "broadcaster_id" string)
    |> map2 (|>) (field "creator_id" string)
    |> map2 (|>) (field "video_id" string)
    |> map2 (|>) (field "game_id" string)
    |> map2 (|>) (field "language" string)
    |> map2 (|>) (field "title" string)
    |> map2 (|>) (field "view_count" int)
    |> map2 (|>) (field "created_at" timeStamp)
    |> map2 (|>) (field "thumbnail_url" string)

duration : Decoder Time
duration =
  string
    |> map (\s -> case Parser.run Parse.duration s of
      Ok d -> d
      Err err ->
        let _ = Debug.log "duration parse error" err in 0
    )

timeStamp : Decoder Time
timeStamp =
  string
    |> andThen (\s -> case Date.fromString s of
      Ok d -> succeed (Date.toTime d)
      Err err -> fail err
    )

sampleHost = """
{"hosts":[{"host_id":69626522,"target_id":56623426,"host_login":"stormyiceleopard","target_login":"wondible","host_display_name":"StormyIceLeopard","target_display_name":"wondible"},{"host_id":36610797,"target_id":56623426,"host_login":"doginlake","target_login":"wondible","host_display_name":"DogInLake","target_display_name":"wondible"},{"host_id":59692084,"target_id":56623426,"host_login":"katnox","target_login":"wondible","host_display_name":"katnox","target_display_name":"wondible"},{"host_id":25503934,"target_id":56623426,"host_login":"haskentv","target_login":"wondible","host_display_name":"haskentv","target_display_name":"wondible"},{"host_id":138565742,"target_id":56623426,"host_login":"2laughor2cry","target_login":"wondible","host_display_name":"2LaughOr2Cry","target_display_name":"wondible"},{"host_id":176430307,"target_id":56623426,"host_login":"ossorakgaming","target_login":"wondible","host_display_name":"OssorakGaming","target_display_name":"wondible"},{"host_id":59716837,"target_id":56623426,"host_login":"joeyschefferap","target_login":"wondible","host_display_name":"joeyschefferap","target_display_name":"wondible"}]}
"""

type alias Host =
  { hostId : String
  , targetId : String
  , hostLogin : String
  , targetLogin : String
  , hostDisplayName : String
  , targetDisplayName : String
  }

hosts : Decoder (List Host)
hosts =
  field "hosts" (list host)

host : Decoder Host
host =
  map6 Host
    ((field "host_id" int) |> map toString)
    ((field "target_id" int) |> map toString)
    (field "host_login" string)
    (field "target_login" string)
    (field "host_display_name" string)
    (field "target_display_name" string)