# Hosting Clips

Displays random clips from people hosting you on Twitch.

## URL Parameters

- login : Twitch username of the channel to check hosts for. (Exactly one of login or userId is required.)
- userId : Twitch user id of the channel to check hosts for.
- selfRate : Tweak the amount the channel's own clips are shown. 2 is twice as much, 0.5 is half and so on. Always relative to number of hosts.

### Debugging Parameters

- showClip : "true"/"false" Does not display clip embed.
- hostLimit : Reduce the number of channels displayed and which have clips fetched.

## Limitations

- Currently have no way to determine channel mature flag
- The hosting API is not part of the official Twitch API and requires a CORS proxy. Currently using [https://cors-proxy.htmldriven.com/]

## Compiling

Built using [Elm](http://elm-lang.org/)

A [Twitch Client-ID](https://dev.twitch.tv/docs/authentication#registration) is required to make API calls. This is defined in `src/TwitchId.elm`. This file is not part of of the repo, but `src/TwitchId.elm.example` can be copied and edited to provide your client id.

My build command:

> `elm-make src/HostingClips.elm --output public/hosting-clips.js`

`bin/monitor.bat` has a command using the [watch](https://www.npmjs.com/package/watch) CLI

Once built, `public/index.html` can be opened locally from the filesystem, or set up for a local or internet server as you wish.

## Credits

Icons: [IcoMoon - Free](https://icomoon.io/#icons-icomoon) ([CC BY 4.0](http://creativecommons.org/licenses/by/4.0/))
