# `fixtures/symbol_server/`

Microsoft operates a [public symbol server](https://docs.microsoft.com/en-us/windows-hardware/drivers/debugger/microsoft-public-symbols).
This allows `tests/` to download PDBs at runtime instead of including them in this repository.

As an optimization, `tests/` that download PDBs will cache them in this folder.
