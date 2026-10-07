"""Treatment selector regression: real Qt bindings, modeled JASP lifecycle.

Run with an existing PySide6 environment; no JASP installation is required.
This is not a compiled native JASP-control or saved-file test.

Native provenance: jasp-stats/jasp-desktop@077322e8269c1ace3bd14b6dfe7059f606fc0bc6
  QMLComponents/analysisform.h:237 and analysisform.cpp:680-687: alphabetical
  QMap setup precedes dependency sorting; radio group setUp chooses its default.
  QMLComponents/controls/sourceitem.cpp:66-67: map sources inherit a persistent
  useSourceLevels flag, unlike the direct-values constructor at78-84.
  QMLComponents/models/listmodel.cpp:628-640: levels filter resolves each term
  as a dataset column; controls/comboboxbase.cpp:160-190 preserves value or index.

The old binding is deliberately retained as a failure control: omitting either
the setup ordering or sticky flag must fail the old-behavior assertions.
"""
from pathlib import Path
import re, os, sys

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from PySide6.QtCore import (
    QObject,
    Property,
    Signal,
    Slot,
    QUrl,
    QCoreApplication,
    QEvent,
    qInstallMessageHandler,
)
from PySide6.QtGui import QGuiApplication
from PySide6.QtQml import QQmlEngine, QQmlComponent, qmlRegisterType

app = QGuiApplication(sys.argv)
messages = []
qInstallMessageHandler(lambda kind, ctx, msg: messages.append(msg))


class Field(QObject):
    valueChanged = Signal()

    def __init__(self, v):
        super().__init__()
        self.v = v

    value = Property(str, lambda self: self.v, notify=valueChanged)

    def set(self, v):
        self.v = v
        self.valueChanged.emit()


class Rows(QObject):
    countChanged = Signal()
    columnsNamesChanged = Signal()

    def __init__(self):
        super().__init__()
        self.fields = {}

    count = Property(int, lambda self: len(self.fields), notify=countChanged)
    columnsNames = Property(
        "QStringList", lambda self: list(self.fields), notify=columnsNamesChanged
    )

    @Slot(str, str, result=QObject)
    def getRowControl(self, k, n):
        return self.fields.get(k)

    def add(self, k, v):
        self.fields[k] = Field(v)
        self.fields[k].setParent(self)
        self.countChanged.emit()
        self.columnsNamesChanged.emit()

    def remove(self, k):
        self.fields.pop(k)
        self.countChanged.emit()
        self.columnsNamesChanged.emit()


class PhaseModel(QObject):
    labelsReordered = Signal(str)


class Phase(QObject):
    levelsChanged = Signal()

    def __init__(self):
        super().__init__()
        self.assigned = False
        self.labels = []
        self.model_ = PhaseModel(self)

    model = Property(QObject, lambda self: self.model_, constant=True)
    levels = Property(
        "QStringList",
        lambda self: self.labels if self.assigned else [],
        notify=levelsChanged,
    )

    def assign(self, labels):
        self.assigned = True
        self.labels = labels
        self.levelsChanged.emit()

    def clear(self):
        self.assigned = False
        self.levelsChanged.emit()

    def reorder(self, labels):
        self.labels = labels
        self.model_.labelsReordered.emit("phaseColumn")

    def labelsOf(self, name):
        return self.labels if name == "phaseColumn" else []


class Dropdown(QObject):
    sourceChanged = Signal()

    def __init__(self, parent=None):
        super().__init__(parent)
        self.v = []
        self.s = []
        self.setup = False
        self.sticky = False
        self.terms = [""]
        self.current = ""
        self.index = 0
        self.resets = []

    def list(self, x):
        return x.toVariant() if hasattr(x, "toVariant") else x

    def setValues(self, x):
        x = self.list(x)
        if self.v != x:
            self.v = x
            if self.setup:
                self.reset()
            self.sourceChanged.emit()

    def setSource(self, x):
        x = self.list(x)
        if self.s != x:
            self.s = x
            if self.setup:
                self.reset()
            self.sourceChanged.emit()

    values = Property(
        "QVariant", lambda self: self.v, setValues, notify=sourceChanged
    )
    source = Property(
        "QVariant", lambda self: self.s, setSource, notify=sourceChanged
    )

    def nativeSetUp(self, phase):
        self.phase = phase
        self.setup = True
        self.reset()
        phase.levelsChanged.connect(self.phaseChanged)

    def phaseChanged(self):
        if any(s.get("name") == "phase" for s in self.s):
            self.reset()

    def reset(self):
        # SourceItem::readAllSources direct values overload does not inherit
        # useSourceLevels.
        result = list(self.v)
        for source in self.s:
            # SourceItem map ctor 0.98.1 lines66–67: _setupSources does not
            # reset this flag.
            filters = [source["use"]] if source.get("use") else []
            if "levels" in filters:
                self.sticky = True
            if self.sticky and "levels" not in filters:
                filters.append("levels")
            terms = source.get(
                "values", (["phaseColumn"] if self.phase.assigned else [])
            )
            # SourceItem::_readAllTerms ->
            # ListModel::termsEx/filterTerms/allLevels.
            if "levels" in filters:
                terms = [
                    label
                    for term in terms
                    for label in self.phase.labelsOf(term)
                ]
            result.extend(terms)

        self.terms = list(dict.fromkeys([""] + result))
        # ComboBoxBase::termsChangedHandler: old value, then index fallback.
        if self.current in self.terms:
            self.index = self.terms.index(self.current)
        if self.index < 0 or self.index >= len(self.terms):
            self.index = 0
        self.current = self.terms[self.index]
        self.resets.append((self.sticky, list(self.terms), self.current))


qmlRegisterType(Dropdown, "Probe", 1, 0, "NativeLogicDropdown")

actual = (
    Path(__file__).resolve().parents[2] / "inst/qml/Treatment.qml"
).read_text()


def extract_function(name):
    return re.search(
        r"\tfunction " + name + r"\(\)\n\t\{.*?\n\t\}\n", actual, re.S
    ).group(0)


fn = extract_function("simulationPhaseNames") + extract_function("loadedPhaseNames")
bridge = re.search(
    r"\tproperty int phaseLevelsRevision:.*?\n\t\}", actual, re.S
).group(0)
oldexpr = (
    'inputType.value == "simulateData" '
    '? [{values: treatmentForm.simulationPhaseNames()}] '
    ': [{name: "phase", use: "levels"}]'
)
names = ["comparisonPhase", "referencePhase"]
expressions = []
for name in names:
    body = re.search(r'name: "' + name + r'"(.*?)\n\s*}', actual, re.S).group(1)
    assert not re.search(r"^\s*source\s*:", body, re.M), name
    assert 'depends: [inputType, simPhaseEffects, "phase"]' in body, name
    expressions.append(
        re.search(r"values:\s*(.*?)\n\s*addEmptyValue:", body, re.S).group(1)
    )
assert expressions[0] == expressions[1]
newexpr = expressions[0]
print(
    "Using both actual current production values bindings and reorder connection",
    flush=True,
)

KEEP = []
phases = ["Pre-treatment", "Treatment", "Post-treatment"]


def run(label, prop, expr):
    rows = Rows()
    phase = Phase()
    engine = QQmlEngine()
    engine.rootContext().setContextProperty("fakeRows", rows)
    engine.rootContext().setContextProperty("fakePhase", phase)
    qml = '''import QtQuick
import Probe 1.0
Item {
 id: treatmentForm
 property var simPhaseEffects: fakeRows
 property var phaseVariable: fakePhase
 property QtObject inputType: QtObject { property string value: "" }
 %s
 property QtObject comparisonPhase: NativeLogicDropdown { %s: %s }
 property QtObject referencePhase: NativeLogicDropdown { %s: %s }
}''' % (fn + bridge, prop, expr, prop, expr)
    comp = QQmlComponent(engine)
    comp.setData(qml.encode(), QUrl(label + ".qml"))
    form = comp.create()
    assert form, [e.toString() for e in comp.errors()]
    comparison = form.property("comparisonPhase")
    reference = form.property("referencePhase")
    mode = form.property("inputType")

    # Exact Form QMap order: comparisonPhase setup, inputType setup sets its
    # default, then referencePhase setup.
    comparison.nativeSetUp(phase)
    mode.setProperty("value", "simulateData")
    reference.nativeSetUp(phase)
    for key, value in zip(["r0", "r1", "r2"], phases):
        rows.add(key, value)
    print(
        label, "INITIAL", comparison.terms, reference.terms,
        "sticky", comparison.sticky, reference.sticky, flush=True,
    )
    if label == "old-v2":
        assert comparison.terms == [""] and reference.terms == [""] + phases
    else:
        assert comparison.terms == reference.terms == [""] + phases

    phase.assign(phases)
    mode.setProperty("value", "loadData")
    assert comparison.terms == reference.terms == [""] + phases
    for d in [comparison, reference]:
        d.current = "Post-treatment"
        d.index = 3
        d.resets = []
    mode.setProperty("value", "simulateData")
    print(
        label, "AFTER LOAD-SIM", comparison.terms, reference.terms,
        "selection", comparison.current, reference.current, flush=True,
    )
    if label == "old-v2":
        assert comparison.terms == reference.terms == [""]
    else:
        assert comparison.terms == reference.terms == [""] + phases
        assert comparison.current == reference.current == "Post-treatment"
        # Equal QVariant lists cause no setter reset; changed lists cause one.
        assert len(comparison.resets) <= 1 and len(reference.resets) <= 1

        rows.fields["r1"].set("Therapy")
        assert comparison.terms == reference.terms == [
            "", "Pre-treatment", "Therapy", "Post-treatment"
        ]
        rows.add("r3", "Phase 4")
        assert comparison.terms == reference.terms == [
            "", "Pre-treatment", "Therapy", "Post-treatment", "Phase 4"
        ]
        rows.remove("r3")
        assert len(comparison.terms) == len(reference.terms) == 4
        rows.fields["r1"].set("Treatment")

        for d in [comparison, reference]:
            d.resets = []
        mode.setProperty("value", "loadData")
        assert comparison.current == reference.current == "Post-treatment"
        assert len(comparison.resets) <= 1 and len(reference.resets) <= 1

        phase.assign(["Baseline", "Therapy", "Follow-up"])
        assert comparison.terms == reference.terms == [
            "", "Baseline", "Therapy", "Follow-up"
        ]
        phase.reorder(["Follow-up", "Baseline", "Therapy"])
        assert comparison.terms == reference.terms == [
            "", "Follow-up", "Baseline", "Therapy"
        ]
        phase.clear()
        assert comparison.terms == reference.terms == [""]
        print(
            label,
            "PASS simulation mutations, both mode directions, levels notification, "
            "reorder-only signal and clear",
            flush=True,
        )
    KEEP.extend([rows, phase, engine, comp, form, comparison, reference, mode])


run("old-v2", "source", oldexpr)
run("new-values", "values", newexpr)
assert not messages, messages
print(
    "PASS native-logic regression assertions; real Qt bindings, "
    "modeled native lifecycle only",
    flush=True,
)

# Disconnect modeled native listeners before destroying QML controls, as native
# AnalysisForm::cleanUpForm does. Keep context objects alive through destruction.
for rows, phase, engine, comp, form, comparison, reference, mode in zip(
    *[iter(KEEP)] * 8
):
    phase.levelsChanged.disconnect(comparison.phaseChanged)
    phase.levelsChanged.disconnect(reference.phaseChanged)
    form.deleteLater()
QCoreApplication.sendPostedEvents(None, QEvent.DeferredDelete)
app.processEvents()
for rows, phase, engine, comp, form, comparison, reference, mode in zip(
    *[iter(KEEP)] * 8
):
    engine.deleteLater()
QCoreApplication.sendPostedEvents(None, QEvent.DeferredDelete)
assert not messages, messages
print("PASS normal explicit Qt teardown", flush=True)
