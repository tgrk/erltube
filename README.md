erltube
=======

An Erlang library for [Google Youtube API][1] v3.

## Fetch dependencies and compile

Project depends on [jiffy][2] library for JSON parsing.
```
$ rebar get-deps compile
```

## Quick start

### Authentication
The Youtube API v3 uses implementation of oAuth 2.0 for authentiaction and access validation.
This library provide helper functions to [authorize][6] your application. Fist you neeed to [register][3] your application to get `client_id`, `redirect_uri` and `client_secrect`.

```
TODO
```

### General

#### Channels
Returns a collection of zero or more [channel][4] resources that match the request params:
```erlang
TODO
```

[Updates][5] a channel's metadata:
```erlang
TODO
```

#### Playlists
```
TODO
```

#### Upload
```
TODO
```

## Example
```
TODO
```


[1]: https://developers.google.com/youtube/v3/
[2]: https://github.com/davisp/jiffy
[3]: https://developers.google.com/youtube/registering_an_application
[4]: https://developers.google.com/youtube/v3/docs/channels/list
[5]: https://developers.google.com/youtube/v3/docs/channels/update
[6]: https://developers.google.com/youtube/v3/guides/authentication