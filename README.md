# erlang-repl-utils

Simple utilities for interactive Erlang development, plus a personal
Emacs configuration.

Requirements: Erlang/OTP 27 or later (tested with OTP 29) and
GNU Emacs 29 or later (tested with Emacs 31).

| File                      | Contents                                         |
|---------------------------|--------------------------------------------------|
| `user_default.erl`        | Extra commands for the Erlang shell              |
| `user_default_config.erl` | Key-value configuration used by `user_default`   |
| `.erlang`                 | Example `~/.erlang` that loads the two modules   |
| `.econf`                  | Example configuration file                       |
| `.emacs`                  | Emacs init file                                  |
| `buffer-stash.el`         | Emacs library: stash all buffers at exit, restore them at startup |

## Erlang shell commands

Run `make` to build the beam files. Then copy `.erlang` to your home
directory and set `Dir` in that file to the directory of the beam
files.

The Erlang shell looks in the module
[`user_default`](https://www.erlang.org/doc/apps/stdlib/shell_default.html)
for commands that it does not know. Thus you can call these functions
without a module name:

| Command          | Function                                                    |
|------------------|-------------------------------------------------------------|
| `mk()`           | Run make in the configured directories, load the changed modules, run their EUnit tests |
| `mk(Dir)`        | The same, for the directory `Dir`                           |
| `mk0()`          | Run make in the current directory                           |
| `mkl()`          | Run make in the current directory, load the changed modules |
| `l()`            | Load all changed modules on the current node                |
| `nl()`           | Load all changed modules on all connected nodes             |
| `mm()`           | List the changed modules                                    |
| `mm(Node)`       | List the changed modules on `Node`                          |
| `u()`            | Load all changed modules and run their EUnit tests          |
| `u(M)`           | Run the EUnit tests of module `M`                           |
| `p(X)`           | Print the full value of `X`                                 |
| `s(Str)`         | Print `Str` as text                                         |
| `mc(M, F, Args)` | Call `M:F(Args)` on this node and all connected nodes       |
| `dbgon(M)`, `dbgon(M, FunOrFile)` | Start the dbg tracer for module `M`        |
| `dbgadd(M)`, `dbgadd(M, F)`       | Add call trace                             |
| `dbgdel(M)`, `dbgdel(M, F)`       | Remove call trace                          |
| `dbgoff()`       | Stop the dbg tracer                                         |
| `dbgtc(File)`    | Print the trace data in `File`                              |
| `help()`         | Show the shell help and these commands                      |

The functions have documentation attributes, so `h(user_default).` and
`h(user_default, mk).` show the documentation in the shell.

### Configuration

`user_default` reads the file `.econf` in the directory where Erlang
is started. The file contains `{Key, Value}.` terms. These keys are
used:

- `mkdir`: the directories in which `mk()` runs make
- `ebin`: the directories that are added to the code path at startup

See `.econf` for an example. You can also read and write keys from the
shell:

```erlang
user_default_config:get_all_keys().       % list all keys and values
user_default_config:get(Key).             % {found, Value} | not_found
user_default_config:get(Key, Default).
user_default_config:put(Key, Value).
user_default_config:save_config().        % write to the loaded file
user_default_config:save_config(File).    % write to File
```

Note: a save writes the terms in sorted order. Comments in the
original file are not kept.

## Emacs

`.emacs` is a personal init file with some standard key bindings. It
loads the Erlang mode from the installed Erlang/OTP (`lib/tools-*/emacs`),
so the mode always agrees with the OTP version. If no OTP installation
is found, it uses the MELPA package `erlang`.

`buffer-stash.el` is a general library. `.emacs` uses it to save all
buffers when Emacs stops and to restore them at the next start. To
install it, copy the file to `~/.emacs.d/lisp/`. The commentary at the
top of the file gives the details.
