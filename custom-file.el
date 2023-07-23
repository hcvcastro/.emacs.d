(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(company-backends
   '(company-semantic company-clang company-cmake company-capf company-files
		      (company-dabbrev-code company-gtags company-etags company-keywords)
		      company-oddmuse company-dabbrev))
 '(custom-enabled-themes '(hcv-gruvbox-light-hard))
 '(custom-safe-themes
   '("e88b46709b84985cd53ccd4fd5a8ec8d9e59560aa415f302e535785288d48361" "603eb1c9a1b883a9737cdcd55adf011758271433bffa441cb648519f5b276cea" "70b92719d97e07f87dad761a7c5d63066f0b73f60b0b220bed7e358ffeb9ea60" "e8b34b270a993bf1ae96a50069b9f8a584aafb073ef96e7ced6f0e0b49a4fe08" "2de50b6adabc94975a8e00b9fe7d9644f7650ed559e82743cad3b51d417fb17e" "b37d228717168b22f4742c56208a98b038183c5d42ec9a6dd87f2159827bcece" "f505943c04b8bdbd57b9f4dc171ae12142e3677787c8914337e3724580e39848" "1f2efc6f00633019a93625dc324244f61ca28e18e44c4cc4a2c830760a7bad74" "c35265861fc17135b981aad1766186e77ff0248dda5f5fc29fd9a2716988cb37" "f71b2c1e477b2314210bf8cde387605f7bd4402a5436d17cc7c9150e0e2df9c7" "1891abe3f4fa5553e0a23a5d1c20dc72043989352b8596b279b26ca19e896950" "0a5f6c4010867808b861ed2b73b10784abc1bafb5ffab8f543819e9a54880a9d" "40a44d761e790c3bcfb323147ba91d830139a3a725c81494a16ffe86efc27169" "354f5a9ce3e0dfe7ec00e0832b915339dea8d812f13ffb5be599e6e05f5c4948" "42fb14472db5719aab5d5e57973e5028ae89d99ac1f7f46774bef6873222475a" "5fcc271a05e77b04e330fd9c87b6e2cc69cf5f21715a11e64f967c5747ad524d" "94412a87b330ebd8122f23f4c1f415d8e8a2dd057e5aca92ede589e42416c008" "2a5991c649f28bfd4a8b82b407bd6219f651ce31a6f76dc7738518c58b02c998" "82907c772a1d32a2bda1644c69b89cb41d207ec05a7a98b74ed25e46a4b9fb4a" "b78f23b46c30f40dd468b585db87b30378ff2cb9a141291c3f9e531012d11ac2" "8b1e1cd63466301012559fbffafef6339b176bb7d3704c0d691cf0b37d3deaab" "ac7410cbc4aa4f39f27888755b44323ba257088ebc679c22dc7f1b5717dac8d8" "ce75e00a39b65cd386be9a2c5f70a8a6f92bf02e696d7bfaae6fe5d25328d917" "01be9c4aeb6e0d992dc24a3dae6b8c5399279d9b6ecfcc8052ad2def8bc6cb13" "1566b14ffbd49c29cb8c537863f1f64bbdf35f3c2c7502aeb2533da57e2bbd32" "0f9d8106678720ff1cbcce50c9fc063e4a2ccd1304cf5ee290b715b922b11b43" "a583e070d0f81b13765c5238a3521390282682c23f9e9477b1c8f42a25eda78f" "a50ef4c50b60777f938826bfc7a89d406a2a154b560544ddbd745393dbe9d8d3" "302319ca75e53715a9481b5f21a9bfefb5537796cb60a84d69f8a47b8342c5ba" "88f3ff3c4d761d070492c9611d38a1242ca9558a4350a916591c86f9e7581d92" "1c6fb1223b4f5f24718124a814a5c81bc0a6a8929ce8fe484f483c2379c90fc3" "00d7337ecdb908b668853d201f99b07c845471de659e9cedc4ccc2f9883676ca" "1424f5dcb6ad3fc4fe5572a7f4c1db127b3030ede293452e4a8ae56545f31559" "eaaf5a77ea1ed5e97eb449a4b4588c4198a5d3de45631d329b0538fb8f22b2db" "cee048037faa2c5b079cca82787ae210ac5dd264fe4d70ba71b4619e2acb0edb" "105ccf66475bb574ad6cc3972a60f7d9d2dfa07b45f085140a12304b2cb4c4a1" "584ca36f7f8134ff95c9d27eb564474b6a3dfdd68510fa55bed0cdfffb60a3bf" "f34cf2880094c8fe2f9a89db847d376336644951cb55283bcc557173ea3e323c" "d6a56d403310297708bb6ea3ce3ea2a8faf41f7250459800339788cd7e35a6c9" "543f71a0a501911423a5a2c0e8430be22d2c97c838ac90623be4edcbc24469f4" "b6cef542ce1bd3d713c9759e210be7e24b20ac789301333b31fdcd92359dd6ab" "939ea070fb0141cd035608b2baabc4bd50d8ecc86af8528df9d41f4d83664c6a" "a06658a45f043cd95549d6845454ad1c1d6e24a99271676ae56157619952394a" "123a8dabd1a0eff6e0c48a03dc6fb2c5e03ebc7062ba531543dfbce587e86f2a" "e1d09f1b2afc2fed6feb1d672be5ec6ae61f84e058cb757689edb669be926896" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default))
 '(electric-pair-mode t)
 '(global-display-line-numbers-mode t)
 '(grep-command "grep --color -nRIH -e ")
 '(grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "po"))
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#3a81c3")
     ("OKAY" . "#3a81c3")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#42ae2c")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(lsp-enable-file-watchers nil)
 '(menu-bar-mode nil)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(which-key lsp-java magit-annex magit-circleci magit-delta magit-diff-flycheck magit-filenotify magit-find-file magit-gerrit magit-gh-pulls magit-gitflow magit-imerge magit-lfs magit-org-todos magit-p4 magit-patch-changelog magit-popup magit-rbr magit-reviewboard magit-section magit-stgit magit-svn magit-tbdiff magit-todos magit-topgit magit-vcsh magithub transient magit php-mode xclip geben typescript-mode pdf-tools company go-mode tabbar session pod-mode muttrc-mode mutt-alias markdown-mode initsplit htmlize graphviz-dot-mode folding eproject diminish csv-mode browse-kill-ring boxquote bm bar-cursor apache-mode spacemacs-theme solarized-theme monokai-theme moe-theme material-theme leuven-theme gruvbox-theme doom-themes color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized))
 '(pdf-view-midnight-colors '("#655370" . "#fbf8ef"))
 '(recentf-mode t)
 '(xclip-mode t)
 '(yank-menu-length 60))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
