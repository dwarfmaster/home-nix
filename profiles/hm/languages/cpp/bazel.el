
(load! "+nix.el")
(map!
 :map bazel-mode-map
 :after bazel
 :localleader
 "b" 'bazel-build
 "c" 'bazel-coverage
 "t" 'bazel-test
 "r" 'bazel-run)
(map!
 :map bazel-workspace-mode-map
 :after bazel
 :localleader
 "h" 'bazel-insert-http-archive)
(after! bazel
  (setq bazel-command *nix/bazel*)
  (setq bazel-buildifier-command *nix/buildifier*)
  (setq bazel-buildozer-command *nix/buildozer*))
