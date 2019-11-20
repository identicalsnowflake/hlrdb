# New in 0.3
- Generalized path declarations to any `Store` from `IsIdentifier`.

# New in 0.2

- Core dependency bumped to 0.1.1
- Commands related to key expiration have been moved to the core module, assigned their corresponding names in Redis, and depend on types from the `time` package.
- `time-exts` has been dropped in favor of `time` for handling timestamps in identifiers, though the semantics should be the same and data should be backwards compatible.

