# QML JavaScript checks

Run the Network preset data checks from the repository root with Node.js:

```sh
node tests/qml/test-network-presets.js
node tests/qml/test-network-actions.js
```

No packages need to be installed. The checks load the actual QML JavaScript
helper, remove its QML-only `.pragma library` line, and stub `qsTranslate`.
They cover preset round trips, input validation, exclusion of patient ratings,
scale boundaries, and independence of returned objects. They do not instantiate
JASP controls or verify the live QML interface. Run these checks separately from
the R `testthat` suite.

The action checks extract the actual Form functions from `Network.qml` and use
small JASP API fakes (`columnsNames`, `getRowControl`, `addItem`, `removeItem`,
and field properties). They verify that applying presets and clearing an
assessment affect the intended data, preserve CSV export state, and fail before
clearing ratings when controls are unavailable, uninitialized, or have empty
generated connection sources. Failed growth restores added rows without
clearing existing ratings. The fakes preserve retained row
identities during removal, matching JASP's `ListModel` implementation. They do
not reproduce native model notifications or replace a JASP integration test.
The same script checks the actual `Upgrades.qml` migration: existing severity
values and rated flags are preserved, missing legacy flags are filled, and
unrelated options remain unchanged.
