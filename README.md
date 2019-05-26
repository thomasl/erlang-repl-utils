Simple utilities for interactive erlang development.

Run `make` to build the files, then take a look at `.erlang` for
how to load the beam files on starting erlang. Or have a look below.

1. `user_default`: provide this in your `.erlang`

```
code:load_abs("$PATH/user_default").
```

See also `http://erlang.org/doc/man/shell_default.html`

The `user_default`  module here and there uses `user_default_config`, which
keeps track of persistent configuration variables. `user_default:mk/0`
uses this, for instance. 

In particular, the keys `mkdir` and `ebin` are currently used if available.

See `.econf` for a configuration example and `.erlang` for how to
start it. You can read and write configuration keys and config files. See
`user_default` for how this is used by our system and
`user_default_config` for how it is implemented.

(NOTE: Writing and saving configuration files is currently not functioning
properly.)

2. `.emacs`

OK, this one is probably mostly useful to me. Some standard key
bindings, mostly.
