erltube
=======

An Erlang library for [Google Youtube API][1] v3.

## Fetch dependencies and compile

Project depends on [jiffy][2] library for JSON parsing.
```
$ rebar get-deps compile
```

## Quick start

### Authentication using oAuth2
The Youtube API v3 uses implementation of oAuth 2.0 for authentiaction and access validation.
This library provide helper functions to authorize your application.

```
TODO
```

#### General

Validate API params:
```erlang
true = erltube:is_valid_param(channel, [{id, "Foobar"}]).
```

## Example
```
TODO
```


[1]: https://developers.google.com/youtube/v3/
[2]: https://github.com/davisp/jiffy
[3]: https://developers.google.com/youtube/registering_an_application