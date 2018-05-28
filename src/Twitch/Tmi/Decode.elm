module Twitch.Tmi.Decode exposing
  ( Host
  , hosts
  , sampleHost
  )

import Json.Decode exposing (..)

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
