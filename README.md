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
uses this, for instance. See the macro `?home_defaults` in `user_default_config` for a
config table location variable you may want to update.

2. `.emacs`

OK, this one is probably mostly useful to me. Some standard key bindings, mostly.
