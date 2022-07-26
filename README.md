# .emacs.d
My Emacs configuration to develop Collabora Online

Hi, This is my emacs settings of the Collabora Online development.
I run a text terminal (gnome-terminal with TrueColor feature)
for coding and I do not like my workspace folder be contaminated with
the generated build output.

Assign values to your preference workspace folders:
```elisp
;; Collabora Online workspace directory
(setq hcv-cool-workspace-dir "/path/to/your/cool/workspace/"))

;; Collabora Online output build directory
(setq hcv-cool-build-dir "/path/to/your/cool/build/"))

;; Libre Office workspace directory
(setq hcv-lo-workspace-dir "/path/to/your/lo/worspace/"))

;; Libre Office output build directory
(setq hcv-lo-build-dir "/path/to/your/lo/build/"))
```

Check out a local branch to your workspace folder:
```bash
~$ git worktree add /path/to/your/cool/workspace/your-cool-branch your-cool-branch

~$ git worktree add /path/to/your/lo/workspace/your-lo-branch your-lo-branch

~$ cd /path/to/your/cool/workspace/test

~$ emacs
```

Run configure command
```
‘M-x hcv-configure’
```

Edit your config parameters into interactive custom buffer
"*Collabora Online Configure*"

Push the button "Configure"

(Optional) You can define your default config values
in a file.
```elisp
(setq hcv-config-default "~/config.default")
```

For example, I like to build Libre Office:
```
--with-doxygen=no
--with-external-tar=/path/to/external-tar
```

Compile
```
‘M-x compile’ or C-c c
```

Run
```
C-c x
```

Stop process
```
‘M-x comint-interrupt-subjob’ or C-c C-c
```

Happy hacking


TODO
* Custom buffer to read Makefile targets
* Custom variables
* Package if it is useful