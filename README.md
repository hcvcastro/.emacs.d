# Collabora Configure UI

An Emacs Lisp configuration file that provides a widget-based UI for configuring
two related projects: **Collabora Online (COOL)** and **Collabora Office (CO)**.
The UI parses `configure.ac`, reads `config.status` for current values, and
generates the `./configure` or `./autogen.sh` command to run.

## Recommended workspace layout

Use `git worktree` to keep each workspace isolated. This lets you have several
branches checked out simultaneously without cloning the repo multiple times,
and each worktree becomes its own independent workspace that the UI can target.

Typical setup:

```
~/develop/online/
  main/        ← git worktree: master branch
  bin/         ← git worktree: working branch (active)
  fix-123/     ← git worktree: bugfix branch
```

Create additional worktrees with:

```sh
cd ~/develop/online/main
git worktree add ../bin <branch-name>
```

**Build output is out of tree.** The UI assumes builds go under `~/build/...`,
mirroring the workspace name under `~/develop/...`:

```
~/develop/online/bin/    ← source (git worktree)
~/build/online/bin/      ← build output (COOL)
~/build/online/bin/engine/  ← build output (Collabora Office)
```

Advantages of keeping builds out of tree:

- `git status` stays clean — no generated files cluttering the worktree.
- You can nuke `~/build/online/bin/` and start fresh without touching source.
- Multiple worktrees can each have their own build dir without collisions.
- Switching branches doesn't invalidate unrelated build artifacts.

The variables `hcv-cool-workspace-dir` and the derived build paths encode this
layout. If you deviate from it, adjust the defvars at the top of `config.el`.

## Architecture

### Two parallel project setups

Each project has its own set of variables, mirroring each other:

| Concept            | COOL                           | Collabora Office                 |
| ------------------ | ------------------------------ | -------------------------------- |
| Project name       | `hcv-cool-project-name`        | `hcv-co-project-name`            |
| Workspace dir      | `hcv-cool-workspace-dir`       | (derived, under COOL + `engine/`) |
| Build dir          | `hcv-cool-default-build-dir`   | `hcv-co-default-build-dir`       |
| configure.ac path  | `hcv-cool-configure-ac`        | `hcv-co-configure-ac`            |
| Default options    | `hcv-cool-config-default-ac`   | `hcv-co-config-default-ac`       |
| config.status path | `hcv-cool-config-status`       | `hcv-co-config-status`           |
| Configure script   | `hcv-cool-configure-file`      | `hcv-co-configure-file`          |
| Option list        | `hcv-cool-config-list`         | `hcv-co-config-list`             |
| Config buffer name | `hcv-cool-config-buffer`       | `hcv-co-config-buffer`           |

COOL uses `./configure`; Office uses `./autogen.sh`. Office lives in the
`engine/` subdirectory inside the COOL workspace, and its build dir is derived
from COOL's: `<cool-build>/engine/`.

### Transient menu structure

Entry point: `C-c c` → `hcv-main-commands`.

```
C-c c
  Buffers & Navigation
    b — Buffer List
    r — Recent Files
    m — Bookmarks
    g — Registers
    y — Yank Menu
  Projects
    o — Collabora Online…
    w — Collabora Office…
```

Each project submenu is identical in shape:

```
  c — Configure…      (hcv-configure-<proj>)
  m — Make            (hcv-compile-<proj>)
  r — Run             (hcv-run-<proj>)
  L — Log (head)      (hcv-head-config-<proj>)
  t — Tags: build     (hcv-tags-build-<proj>)
  T — Tags: load      (hcv-tags-load-<proj>)
```

Keys are consistent across projects: `c` always configures, `m` always makes,
etc. Lowercase for frequent actions, uppercase (`L`, `T`) for the less common.

## Key flow summary

At load time (if `command-line-default-directory` is inside COOL workspace):

1. Build paths derived: `hcv-cool-default-build-dir`, `hcv-co-default-build-dir`.
2. For each project: parse configure.ac, overlay env-vars, then try to read
   `config.status`. If that fails, fall back to the defaults file.
3. Emacs defaults set: `compile-command`, `tags-table-list` (COOL's).
4. If `./configure` doesn't exist, run `autogen.sh` to create it.

At interactive time:

1. User presses `C-c c` → menu appears.
2. User picks `o` (Online) or `w` (Writer/Office).
3. User picks `c` (Configure) → widget buffer opens with current options.
4. User ticks boxes, edits fields.
5. User presses "Copy" button → command placed in clipboard to paste in shell.
   Or "Configure" button → command runs via `async-shell-command`.

## Files

- `config.el` — the configuration itself.
- `~/cool-config.default` — COOL option defaults.
- `~/co-config.default` — Office option defaults.

### Example `~/cool-config.default`

```
CXXFLAGS="-g -O0 -Werror -Wno-error=deprecated-declarations"
--enable-debug
--enable-silent-rules
--with-poco-includes=/opt/poco/include
--with-poco-libs=/opt/poco/lib
--with-lokit-path=/home/user/collaboraoffice/include
--with-lo-path=/home/user/collaboraoffice/instdir
--with-logfile=/tmp/coolwsd.log
```

### Example `~/co-config.default`

```
CXXFLAGS="-g -O0 -Werror -Wno-error=deprecated-declarations"
--enable-dbgutil
--enable-debug
--enable-silent-rules
--without-java
--with-jdk-home=/usr/lib/jvm/default-java
--with-external-tar=/home/user/lo-externals
--without-help
--without-myspell-dicts
```

Notes:

- One option per line.
- Blank lines and lines starting with `#` are ignored.
- Values wrapped in double quotes (like `CXXFLAGS="..."`) get the surrounding
  quotes stripped at read time — shell quoting is re-applied when the command
  is built, so don't pre-escape.
- These files are consulted only when `config.status` is absent. Once you run
  `./configure`, subsequent widget openings read from `config.status` instead.

## Keybindings quick reference

| Key       | Action                       |
| --------- | ---------------------------- |
| `C-c c`   | Open main commands menu      |
| `C-o`     | Open next line (vi-style)    |
| `M-o`     | Open previous line (vi-style) |

Inside `C-c c`:

| Key       | Action                       |
| --------- | ---------------------------- |
| `b`       | Buffer List                  |
| `r`       | Recent Files                 |
| `m`       | Bookmarks                    |
| `g`       | Registers                    |
| `y`       | Yank Menu                    |
| `o`       | Online submenu               |
| `w`       | Writer / Office submenu      |

Inside `o` or `w`:

| Key       | Action                       |
| --------- | ---------------------------- |
| `c`       | Configure (widget buffer)    |
| `m`       | Make                         |
| `r`       | Run                          |
| `L`       | Log (head of config.log)     |
| `t`       | Tags: build (`make tags`)    |
| `T`       | Tags: load (set tags-table-list) |
