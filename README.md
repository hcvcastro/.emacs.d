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

## Running coda-qt (the Qt app)

When the current COOL build is configured or built with `--enable-qtapp`, the
Online submenu (`C-c c o`) gains **`R` — Run coda-qt…**. It opens a widget
buffer (like the configure UI) with the run parameters, then launches
`<build>/qt/coda-qt` and streams its output — with the exact command echoed on
the first line — into a `*coda-qt …*` async buffer.

```
coda-qt — run parameters.

Display:  :99
Wayland:  wayland-0
Document: <worktree>/test/data/hello.odt
Chromium: --disable-gpu --disable-gpu-compositing --disable-software-rasterizer --no-sandbox

[Run Wayland]  [Run X11]  [Cancel]

(W: Run Wayland   X: Run X11   q: Cancel)
```

- **Run Wayland** (`W`) — runs natively under Wayland: sets `WAYLAND_DISPLAY`
  (the *Wayland* field), `QT_QPA_PLATFORM=wayland` and `XDG_RUNTIME_DIR`, and
  aborts if that Wayland socket isn't present (start the Wayland session first).
  The *Wayland* field **auto-detects** the running compositor's `wayland-N`
  socket (wlroots auto-picks the name), preferring one other than Emacs's own.
- **Run X11** (`X`) — sets `DISPLAY` to the *Display* field and first checks the
  display is reachable (`xdpyinfo`/`xset`), aborting otherwise.
- Both force software rendering (no GPU) and export the editable
  `QTWEBENGINE_CHROMIUM_FLAGS`. The single-key `W`/`X`/`q` shortcuts work outside
  the fields (inside a field those letters type normally).
- The *Display*, *Wayland* and Chromium-flags values persist across sessions via
  Customize (only when changed); the document stays per-worktree.

Customize: `hcv-coda-qt-display`, `hcv-coda-qt-wayland-display`,
`hcv-coda-qt-chromium-flags`. The binary lives out-of-tree under
`hcv-cool-default-build-dir` (`<build>/qt/coda-qt`).

## Running Collabora Office (soffice)

The Office submenu (`C-c c w`) entry **`r` — Run…** opens the same kind of
buffer for the desktop `soffice.bin` (shown when the build has a `soffice.bin`):

```
Collabora Office — run parameters.

Display:  :99
Wayland:  wayland-0
Args:     --norestore --nologo
Document: <worktree>/test/data/hello.odt

[Run Wayland]  [Run X11]  [Cancel]

(W: Run Wayland   X: Run X11   q: Cancel)
```

- **Run Wayland** (`W`) / **Run X11** (`X`) — same display/socket checks and
  Wayland auto-detection as coda-qt, but soffice selects its backend through the
  GTK VCL plugin: Wayland sets `WAYLAND_DISPLAY` + `GDK_BACKEND=wayland` +
  `XDG_RUNTIME_DIR`; X11 sets `DISPLAY` + `GDK_BACKEND=x11`. Both export
  `SAL_USE_VCLPLUGIN=gtk3` and force software rendering, then run
  `<co-build>/instdir/program/soffice.bin <Args> <Document>`.
- *Document* has the same example radios as coda-qt (shared
  `hcv-coda-qt--examples`); the default *Args* carry no module flag, so soffice
  auto-detects the module from the selected document.
- Persists the session display/Wayland, the args and the document (only when
  changed; the document is shared with coda-qt via `hcv-coda-qt-document`).

Customize: `hcv-co-run-args` (and the shared `hcv-coda-qt-display` /
`hcv-coda-qt-wayland-display` / `hcv-coda-qt-document`).

**Note:** an online/LOKit build (`libmergedlo.so`, driven headless by
`coolwsd`) ships **no VCL plugin**, so its `soffice.bin` cannot open a desktop
window — "no muestra nada". Use a GUI build (e.g. `--enable-gtk3`) for this, or
coda-qt for the online build.

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
| `R`       | Run coda-qt… (COOL, if `--enable-qtapp`) |
| `L`       | Log (head of config.log)     |
| `t`       | Tags: build (`make tags`)    |
| `T`       | Tags: load (set tags-table-list) |
