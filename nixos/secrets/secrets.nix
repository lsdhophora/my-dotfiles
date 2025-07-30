let
  user = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGEBH8xixYoAZUiEu+Y9n/b7WJSDYlHxO4RnGGbJRlOL github-ssh"; # 替换为实际公钥
in {
  "nix-access-tokens-github.age".publicKeys = [ user ];
  "config.dae.age".publicKeys = [ user ];
}
