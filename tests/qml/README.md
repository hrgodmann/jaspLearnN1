# QML checks

Run the Network preset data checks from the repository root with Node.js:

```sh
node tests/qml/test-network-presets.js
node tests/qml/test-network-actions.js
node tests/qml/test-network-rating.js
node tests/qml/test-forecasting-actions.js
```

No packages need to be installed. The checks load the actual QML JavaScript
helper, remove its QML-only `.pragma library` line, and evaluate the prompt-list functions from `NetworkPresetLists.qml` with a stub `qsTr`. The prompt text stays in QML because JASP's translation workflow does not extract JavaScript files.
Both R and Node checks read `tests/fixtures/network-preset-normalization.json`, which fixes the Unicode trimming contract. They cover preset round trips, input validation, exclusion of patient ratings,
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

The rating checks execute `NetworkRating.qml` functions and numeric-input
handlers. They distinguish untouched focus loss, deliberate edits, explicit
acceptance, invalid-input restoration and programmatic synchronization, including
signed magnitude entry. They use small field fakes and do not exercise Qt's event
delivery or JASP's locale conversion; those need a component or Desktop check.

The Forecasting checks execute its actual export initialization and click
expressions with small control fakes. They cover restored request values,
fresh sessions, button transitions and enabling conditions. These checks also
run in CI on QML/JavaScript changes, but do not replace native JASP reopen,
duplicate and undo/redo acceptance checks.

## Treatment phase selector bindings

With an existing Python environment containing PySide6, run:

```sh
QT_QPA_PLATFORM=offscreen QT_QUICK_CONTROLS_STYLE=Basic python3 tests/qml/test-treatment-phase-selectors.py
```

This check extracts both selectors' current QML bindings, their name helpers,
and the label-reorder connection. Real Qt evaluates those bindings against
small JASP interface models. The models reproduce JASP 0.98.1's alphabetical
control setup and persistent `useSourceLevels` flag, with exact source revision
and locations recorded in the test. A retained old-binding case must reproduce
the initial empty first selector and the loss of both lists after loading data
and switching back to simulation. The current bindings must populate both
selectors, track phase edits and loaded-label changes, respond to the native
reorder-only signal, and preserve a shared explicit selection across modes.
The test also completes normal Qt object cleanup.

PySide6 is separate from the Node checks and is not installed by this command.
The CI job `qml-treatment-bindings` runs this check with Python 3.11 and pinned
PySide6 6.8.3, using the offscreen platform and Basic style with QML disk caching
disabled.
These interface models are not native JASP controls. Fresh Desktop forms,
actual dataset edits, and saved-analysis reopen remain separate acceptance
checks; this regression does not certify those paths.

## Network slider geometry and interaction

With an existing PySide6 environment, run both supported test styles:

```sh
QT_QPA_PLATFORM=offscreen QT_QUICK_CONTROLS_STYLE=Basic python3 tests/qml/test-network-rating-layout.py
QT_QPA_PLATFORM=offscreen QT_QUICK_CONTROLS_STYLE=Fusion python3 tests/qml/test-network-rating-layout.py
```

This regression loads the actual `NetworkRating.qml` component inside the three
parent rows extracted from `Network.qml`: shared severity, manual connections,
and all-pairs connections. Qt evaluates the production layouts and supplies the
real Slider, handle, and mouse events. Small presentation substitutes replace
JASP fields, labels, buttons and selectors; the test does not emulate native
JASP option binding or locale parsing.

The checks compare tick-label centers, track endpoints and midpoint markers
against the actual handle travel, and require equal track lengths across all
three placements. They cover narrow/wide parent widths, UI scales 1/1.5/2,
maximum values 1/10/1000, signed/magnitude input, larger fonts, and mirrored
layout. They check label containment and overlap, parent-card bounds, and
vertical alignment of the numeric input, direction selector and reset button.
Real mouse drags exercise both endpoints and the midpoint, including negative
magnitude ratings. Resizing or changing presentation must preserve canonical
ratings and emit no `recorded` signal. A retained former-layout fixture must
reproduce its large tick/track mismatch before the production checks run.

The geometric tolerances account for Qt's pixel rounding and mouse coordinates;
they do not rely on the component's own calculated track properties. Both
styles run in the existing `qml-treatment-bindings` CI job with pinned PySide6
6.8.3. No dependency is installed by the local commands above. To retain
combined three-row screenshots and measured geometry, set
`LEARNN1_QML_ARTIFACT_DIR` to an output directory. Native JASP Desktop rendering,
actual native field sizing, and saved-analysis behavior remain separate checks.
