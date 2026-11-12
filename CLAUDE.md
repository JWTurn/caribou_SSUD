# caribou_SSUD -- repository conventions

Guidance for contributors and coding agents working in this repo. This is a SpaDES module that
turns iSSA (integrated step-selection analysis) model coefficients into a steady-state utilization
distribution (SSUD; Potts & Schlaegel 2020) for caribou. The repo is **public** on GitHub
(`JWTurn/caribou_SSUD`).

## People / authorship

- **JWT** = Julie W. Turner -- module author and maintainer.

## Hard rules (non-negotiable)

- **Raw caribou locations are restricted.** Raw collar locations (and any table that still holds
  them, e.g. used steps in iSSA data, mortality sites) and herd-range details not already public
  never go into tracked files, commit messages, PR/issue text, or rendered docs. Model coefficients
  and derived maps are fine. Keep data in `data/` (gitignored). When unsure, ASK JWT.
- **No host names, IP addresses or restricted terms in published content.** Enforced by
  `scripts/git-hooks/check-restricted-names` (lists in the gitignored `_restricted_terms.txt` and
  `.claude/r-project-policy.json`).
- **Secrets stay outside the repo.** Login tokens and keys (Google Drive, GitHub) live outside the
  repo (e.g. `~/.config/`, `~/.Renviron`); never read, print or copy them.
- **Never fabricate citations, references, or DOIs.** Verify against authoritative sources; ASK JWT
  if unsure.
- **Respect the existing code style.** Do not reformat code you were not asked to change.

## Checks before pushing

- Once per clone, install the pre-push check:
  `install -m 755 scripts/git-hooks/pre-push "$(git rev-parse --git-common-dir)/hooks/pre-push"`.
- Run it by hand on the whole repo: `scripts/git-hooks/check-restricted-names --tree`.
