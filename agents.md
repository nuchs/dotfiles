# sn-agents.md

## Purpose
This repository manages personal **dotfiles** and configuration for the developer environment.
All files are symlinked into their respective locations on the system — changes here directly
affect the live workflow. Therefore, any modification must be treated as **critical**.

## Change Policy
- **Never commit directly to main.**  
  All edits must occur on a feature branch and be merged only through a Pull Request.
- **Every change must be reviewed and tested locally** before merge.
- **Do not rename, move, or delete files** unless explicitly requested.
  Many are symlinked and required for shell startup, editor configuration, or tooling.

## Repository Structure
- `/nvim/` — Neovim configuration (init.lua, Lua modules, plugin settings).  
- `/scripts/` — Executables and shell utilities. These are symlinked to `~/.local/bin/`.  
- Root files — Shell, tmux, and general configuration (named without a leading dot).

Each file corresponds to its hidden counterpart in `$HOME`.  
Example: `bashrc` here maps to `~/.bashrc`.

## Agent Behavior
- Treat every file as **live configuration** — edits must preserve existing semantics.
- **No experimental edits or speculative changes.**
- Keep configurations lean and purposeful; remove unused items rather than commenting them out.
- Use descriptive names for functions and scripts. Shorten through aliases, not abbreviations.
- Maintain readability: clear structure, minimal comments, logical grouping.
- Use POSIX-compatible syntax unless explicitly Bash, Zsh, Lua, or Go.
- Avoid unnecessary dependencies or plugin bloat.
- When updating Neovim config, keep plugin lists minimal and document reasoning inline.

## Security
- This repository is **public**.  
  Absolutely **no secrets, API keys, access tokens, SSH configs, or private paths** should appear.
- Remove any personal data, system identifiers, or sensitive paths before committing.
- Avoid revealing internal domain names or hostnames.
- Never include credentials even in comments or sample configs.
- Prefer environment variables or external secret stores for anything private.
- When in doubt: assume visibility by everyone.

## Scripts
- Executables belong in `/scripts/`.
- Names must be descriptive and self-explanatory.
- Output should be deterministic, no hidden side effects.
- Ensure scripts are `chmod +x` and portable between Linux systems.
- Scripts should fail safely: check inputs, avoid destructive defaults.

## Environment Notes
- Symlinks managed manually or via a bootstrap script (e.g. `stow` or custom linker).
- Target environment: Linux (Arch/WSL2).
- Default shell: Bash.  
  Terminal multiplexer: tmux.  
  Primary editor: Neovim.
- All paths must respect `$HOME` expansion; no absolute `/home/<user>` paths.

## Safe Testing Procedure
1. **Clone** the branch locally; do *not* test directly from main.
2. **Unlink** or isolate affected dotfiles from `$HOME` using a staging directory:
   ```bash
   mkdir -p ~/dotfiles-test && cd ~/dotfiles-test
   ln -s /path/to/repo/<file> ~/<file>
   ```
3. Test interactively:
    * Start a new shell session for shell or tmux changes.
    * Launch Neovim and run :checkhealth for plugin or Lua changes.
    * For scripts, run them with --help or --dry-run before live use.
4. Once verified, open a Pull Request for review and merge.

## Example Workflow
### To add a new script
1. Create the script under /scripts/ with a descriptive name.
2. Set correct permissions and validate portability.
3. Test locally via its symlink in ~/.local/bin/.
4. Commit to a branch and open a PR for review.

### To modify Neovim config
1. Edit nvim/init.lua or supporting Lua modules.
1. Confirm syntax and plugin load order with :checkhealth.
1. Test interactively before PR.

## Exclusions
* Do not modify or auto-format:
* Symlink metadata
* Generated lockfiles
* Machine-specific or private files (SSH, GPG, Vault, etc.)

## Philosophy
This repo emphasizes clarity over cleverness, utility over ornament, and removal over accumulation.
Git preserves history — clutter does not.


