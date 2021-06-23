# Nix configuration for testing QueueSheet against all supported GHC versions
#
# Usage:
#
#     $ nix-build test-all.nix

{
  queue-sheet-ghc-822 = import ./default.nix { compiler = "ghc822"; };
  queue-sheet-ghc-844 = import ./default.nix { compiler = "ghc844"; };
  queue-sheet-ghc-865 = import ./default.nix { compiler = "ghc865"; };
  queue-sheet-ghc-884 = import ./default.nix { compiler = "ghc884"; };
  queue-sheet-ghc-8104 = import ./default.nix { compiler = "ghc8104"; };
  queue-sheet-ghc-901 = import ./default.nix { compiler = "ghc901"; };
}
