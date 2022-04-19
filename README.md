# ansi

ANSI escape code library for Erlang.

## Features

* **Smart** :brain:

  Terminal color support is detected automatically on load, based on various factors such as environment variables and `tput` output (if available).

* **Respectful** :pray:

  Respects [`NO_COLOR`](https://no-color.org) for disabling colorized output if set by the user. This can be overridden with `ansi:set_mode(Mode)`.

* **Fast** :zap:

  The terminal color mode is detected and stored as a persistent term once on module load. When used, the library only produces deep IO lists with no unnecessary flattening.

* **Composable** :wrench:

  _ansi_ does not assume printing to the console. The user can send the resulting IO data wherever they want (console, file, log etc.) using the appropriate library functions in OTP (or elsewhere).

## Color Tool

_ansi_ includes a color helper CLI tool that can be used to enumerate colors.

### Usage

```console
$ _build/tool/bin/colortool --help
usage: colortool [-b BACKGROUNDS] [-f FOREGROUNDS] [-t TEXT]

Optional arguments:
  -b, --backgrounds The set of background colors to enumerate ('0..16')
  -f, --foregrounds The set of foreground colors to enumerate ('0..16')
  -t, --text        The example text to display
```

The color ranges should be a comma separated list of either individual color
codes or a range in the format `FROM..TO`. Each color code must be between `0` and `255`. Example:

```
0..8,16,232..255
```

### Build

```console
$ rebar3 as tool escriptize
```

This produces a binary at `_build/tool/bin/colortool`.

