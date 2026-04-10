# REL1_43 upgrade: `resources/src/mediawiki.util/util.js` conflict

## Reproduction branch
- `repro/REL1_43-Fandom-1.43.8-utiljs-conflict`

## How conflict was reproduced
Starting from `REL1_43-Fandom`, apply:
1. `git revert -m 1 d4025058bd4`
2. `git revert a71cd7a5445`
3. `git revert -m 1 0bd9be8182d`
4. `git revert -m 1 773318bf32d`
5. `git merge --no-ff --no-commit 1.43.8`

Result: one content conflict in `resources/src/mediawiki.util/util.js` (`addSubtitle`).

## Conflict sides
- Ours (Fandom):
  - Uses Fandom subtitle selector:
    - `.page-header__page-subtitle, .page-header__subtitle`
- Theirs (upstream 1.43.8):
  - Uses core selector:
    - `#mw-content-subtitle`
  - Adds explicit return contract:
    - returns `false` when subtitle element is missing
    - returns `true` after update

## Resolution decision
Keep Fandom selector and keep upstream return-value behavior.

Resolved function behavior:
- find subtitle with Fandom selector
- if subtitle is missing -> `return false`
- append string/node to subtitle
- `return true`

## Rationale
- Preserves Fandom skin compatibility.
- Preserves upstream API semantics (`boolean` result) used by callers.
- Minimal, targeted customization against upstream code.
