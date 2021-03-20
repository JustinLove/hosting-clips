
- may need: https://api.twitch.tv/kraken/channels/CHANNELID/hosts
  - name is now in clip
    - clips are cached, so we need to update storage an handle case where not present
  - fetching names for other users is delayed
- https://obsproject.com/tools/browser-drag-and-drop
- mature flag?

## CORS proxy (obsolete)

http://tmi.twitch.tv/hosts?include_logins=1&target=56623426
http://www.whateverorigin.org/get?url=http%3A%2F%2Ftmi.twitch.tv%2Fhosts%3Finclude_logins%3D1%26target%3D56623426
http://www.whateverorigin.org/
https://cors-proxy.htmldriven.com/?url=http%3A%2F%2Ftmi.twitch.tv%2Fhosts%3Finclude_logins%3D1%26target%3D56623426
