"""Network rating geometry with real Qt layouts, Slider, and mouse events.

Loads the production shared component and extracts its three actual parent rows.
Only JASP controls are substituted; this is not native JASP/Desktop validation.
Requires an existing PySide6 environment (CI pins 6.8.3). Optional screenshots
and measured geometry: LEARNN1_QML_ARTIFACT_DIR=/path/to/output.
"""

from contextlib import contextmanager
import json
from itertools import product
import os
from pathlib import Path
import re
import sys
import tempfile

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")
os.environ.setdefault("QT_QUICK_CONTROLS_STYLE", "Basic")
os.environ.setdefault("QML_DISABLE_DISK_CACHE", "1")

from PySide6.QtCore import QCoreApplication, QEvent, QObject, QPoint, QPointF, Qt, QUrl, qVersion
from PySide6.QtGui import QGuiApplication
from PySide6.QtQml import QQmlComponent, QQmlEngine, QQmlExpression
from PySide6.QtQuick import QQuickItem
from PySide6.QtTest import QTest


ROOT = Path(__file__).resolve().parents[2]
OUTPUT = os.environ.get("LEARNN1_QML_ARTIFACT_DIR")
CHECKS = 0
GEOMETRY = []


def check(condition, detail):
    global CHECKS
    CHECKS += 1
    assert condition, detail


def extract_row(source, identifier):
    """Find a rowComponent, counting braces outside strings and comments."""
    marker = re.search(r"\bid:\s*" + re.escape(identifier) + r"\b", source)
    assert marker, identifier
    match = re.search(r"\browComponent:\s*(\w+\s*\{)", source[marker.end():])
    assert match, identifier
    start = marker.end() + match.start(1)
    opening = source.index("{", start)
    depth = 0
    quote = None
    escaped = False
    comment = None
    index = opening
    while index < len(source):
        char = source[index]
        pair = source[index:index + 2]
        if comment == "line":
            if char == "\n":
                comment = None
        elif comment == "block":
            if pair == "*/":
                comment = None
                index += 1
        elif quote:
            if escaped:
                escaped = False
            elif char == "\\":
                escaped = True
            elif char == quote:
                quote = None
        elif pair in ("//", "/*"):
            comment = "line" if pair == "//" else "block"
            index += 1
        elif char in ('"', "'"):
            quote = char
        elif char == "{":
            depth += 1
        elif char == "}":
            depth -= 1
            if depth == 0:
                return source[start:index + 1]
        index += 1
    raise AssertionError("Unclosed rowComponent for " + identifier)


def write_substitutes(folder):
    """Small presentation substitutes; numeric validation is tested separately."""
    controls = folder / "JASP/Controls"
    controls.mkdir(parents=True)
    (folder / "JASP/qmldir").write_text("module JASP\nDummy 1.0 Dummy.qml\n")
    (folder / "JASP/Dummy.qml").write_text("import QtQuick\nQtObject {}\n")
    types = {
        "TextField": '''import QtQuick
import QtQuick.Controls as C
C.TextField {
    id: field
    property string name: ""
    objectName: name
    property var value: ""
    property real defaultValue: 0
    property string info: ""
    property real fieldWidth: 150 * preferencesModel.uiScale
    property bool initialized: true
    property bool isBound: true
    property alias displayValue: field.text
    property var control: field
    implicitWidth: fieldWidth
    font.pixelSize: 14 * preferencesModel.uiScale * fontMultiplier
    text: String(value)
}''',
        "DoubleField": '''import QtQuick
TextField {
    property real min: 0
    property real max: 1
    property bool negativeValues: false
    property int decimals: 2
    value: defaultValue
}''',
        "DropDown": '''import QtQuick
import QtQuick.Controls as C
C.ComboBox {
    property string name: ""
    objectName: name
    property var source: []
    property var values: []
    property bool addEmptyValue: false
    property string info: ""
    property bool isBound: true
    property bool initialized: true
    font.pixelSize: 14 * preferencesModel.uiScale * fontMultiplier
    implicitWidth: (values.length ? 130 : 210) * preferencesModel.uiScale
    model: values.length ? values : ["Trouble falling asleep", "Waking at night"]
    textRole: values.length ? "label" : ""
    currentIndex: name === "connectionTo" ? 1 : 0
}''',
        "CheckBox": '''import QtQuick
import QtQuick.Controls as C
C.CheckBox {
    id: box
    property string name: ""
    objectName: name
    property alias label: box.text
    property string info: ""
    font.pixelSize: 14 * preferencesModel.uiScale * fontMultiplier
}''',
        "Label": '''import QtQuick
import QtQuick.Controls as C
C.Label {
    objectName: "jasp-label"
    font.pixelSize: 14 * preferencesModel.uiScale * fontMultiplier
}''',
        "Button": '''import QtQuick
import QtQuick.Controls as C
C.Button {
    id: button
    property alias label: button.text
    font.pixelSize: 14 * preferencesModel.uiScale * fontMultiplier
}''',
    }
    (controls / "qmldir").write_text(
        "module JASP.Controls\n"
        + "".join(f"{name} 1.0 {name}.qml\n" for name in types)
    )
    for name, source in types.items():
        (controls / (name + ".qml")).write_text(source)


def settle():
    QCoreApplication.processEvents()
    QTest.qWait(10)
    QCoreApplication.processEvents()


@contextmanager
def window(folder, source, name):
    engine = QQmlEngine()
    engine.addImportPath(str(folder))
    warnings = []
    engine.warnings.connect(lambda errors: warnings.extend(e.toString() for e in errors))
    component = QQmlComponent(engine)
    component.setData(source.encode(), QUrl.fromLocalFile(str(folder / (name + ".qml"))))
    obj = component.create() if component.isReady() else None
    assert obj is not None, [error.toString() for error in component.errors()]
    try:
        settle()
        yield engine, obj, warnings
        check(not warnings, warnings)
    finally:
        obj.close()
        obj.deleteLater()
        QCoreApplication.sendPostedEvents(None, QEvent.DeferredDelete)
        component.deleteLater()
        engine.deleteLater()
        QCoreApplication.sendPostedEvents(None, QEvent.DeferredDelete)


def evaluate(engine, obj, expression):
    script = QQmlExpression(engine.rootContext(), obj, expression)
    result = script.evaluate()
    assert not script.hasError(), script.error().toString()
    return result[0] if isinstance(result, tuple) else result


def has_property(obj, name):
    return obj.metaObject().indexOfProperty(name) >= 0


def one(objects, predicate, description):
    matches = [obj for obj in objects if predicate(obj)]
    assert len(matches) == 1, (description, len(matches))
    return matches[0]


def center(item, relative):
    return item.mapToItem(relative, QPointF(item.width() / 2, item.height() / 2))


def parts(row):
    rating = one(row.findChildren(QObject), lambda obj: has_property(obj, "ratingLabel"), "rating")
    slider = one(
        rating.findChildren(QQuickItem),
        lambda obj: has_property(obj, "visualPosition") and has_property(obj, "handle"),
        "real Qt Slider",
    )
    labels = [obj for obj in rating.findChildren(QQuickItem) if obj.objectName() == "jasp-label"]
    check(len(labels) == 3, ("three tick labels", len(labels)))
    labels.sort(key=lambda label: center(label, slider).x())
    return rating, slider, labels


def geometry(slider, labels):
    labels = sorted(labels, key=lambda label: center(label, slider).x())
    handle = slider.property("handle")
    background = slider.property("background")
    left = float(slider.property("leftPadding")) + handle.width() / 2
    right = left + float(slider.property("availableWidth")) - handle.width()
    start = background.mapToItem(slider, QPointF(0, 0)).x()
    end = background.mapToItem(slider, QPointF(background.width(), 0)).x()
    markers = [item for item in background.childItems() if item.height() > background.height()]
    return {
        "travel": [left, (left + right) / 2, right],
        "labels": [center(label, slider).x() for label in labels],
        "track": [start, end],
        "handle": center(handle, slider).x(),
        "width": slider.width(),
        "text": [str(label.property("text")) for label in labels],
        "labelBounds": [[label.mapToItem(slider, QPointF(0, 0)).x(),
                         label.mapToItem(slider, QPointF(label.width(), 0)).x()]
                        for label in labels],
        "labelWidths": [[label.width(), float(label.property("implicitWidth"))]
                        for label in labels],
        "midpointMarker": center(markers[0], slider).x() if len(markers) == 1 else None,
    }


def assert_geometry(measurement, detail):
    check(measurement["travel"][2] > measurement["travel"][0], detail)
    for actual, expected in zip(measurement["labels"], measurement["travel"]):
        check(abs(actual - expected) < 0.1, (detail, measurement))
    for actual, expected in zip(measurement["track"], measurement["travel"][::2]):
        check(abs(actual - expected) < 0.1, (detail, measurement))
    check(measurement["midpointMarker"] is not None
          and abs(measurement["midpointMarker"] - measurement["travel"][1]) <= 0.51,
          (detail, "midpoint marker", measurement))
    for left, right in measurement["labelBounds"]:
        check(left >= -0.1 and right <= measurement["width"] + 0.1,
              (detail, "clipped label", measurement))
    for previous, following in zip(measurement["labelBounds"], measurement["labelBounds"][1:]):
        check(previous[1] <= following[0], (detail, "overlapping labels", measurement))
    for width, implicit_width in measurement["labelWidths"]:
        check(width >= implicit_width - 0.1, (detail, "compressed label", measurement))


def assert_auxiliary_alignment(rating, slider, detail):
    controls = [item for item in rating.childItems()
                if item.isVisible() and (has_property(item, "isBound") or has_property(item, "label"))]
    expected_count = 3 if rating.property("signedRating") and rating.property("magnitudeMode") else 2
    check(len(controls) == expected_count, (detail, "numeric/direction/button controls", len(controls)))
    track_center = center(slider.property("background"), rating).y()
    bounds = []
    for item in controls:
        check(abs(center(item, rating).y() - track_center) <= 1,
              (detail, "input/control vertical alignment", item.metaObject().className(),
               center(item, rating).y(), track_center))
        left = item.mapToItem(rating, QPointF(0, 0)).x()
        bounds.append((left, left + item.width()))
    slider_left = slider.mapToItem(rating, QPointF(0, 0)).x()
    bounds.append((slider_left, slider_left + slider.width()))
    bounds.sort()
    for previous, following in zip(bounds, bounds[1:]):
        check(previous[1] <= following[0] + 0.1, (detail, "overlapping controls", bounds))


def assert_parent_layout(row, rating, detail):
    padding = float(row.property("padding"))
    start = rating.mapToItem(row, QPointF(0, 0))
    check(start.x() >= padding - 0.1 and start.x() + rating.width() <= row.width() - padding + 0.1,
          (detail, "rating exceeds card's horizontal padding", start.x(), rating.width(), row.width(), padding))
    check(start.y() >= padding - 0.1 and start.y() + rating.height() <= row.height() - padding + 0.1,
          (detail, "rating exceeds card's vertical padding", start.y(), rating.height(), row.height(), padding))
    # The actual row's content layout places the rating column beside its name
    # or selectors. Check those columns, not just the isolated slider itself.
    content = rating.parentItem().parentItem()
    siblings = [item for item in content.childItems() if item.isVisible()]
    bounds = sorted((item.x(), item.x() + item.width()) for item in siblings)
    for previous, following in zip(bounds, bounds[1:]):
        check(previous[1] <= following[0] + 0.1, (detail, "overlapping parent columns", bounds))


def drag(window_obj, slider, position, endpoint):
    handle = slider.property("handle")
    point = center(handle, window_obj.contentItem())
    # Endpoint centers can lie between physical mouse pixels. Drag two pixels
    # past the travel boundary so Qt clamps to the exact endpoint value.
    offset = -2 if endpoint == 0 else 2 if endpoint == 2 else 0
    target = slider.mapToItem(window_obj.contentItem(), QPointF(position + offset, slider.height() / 2))
    QTest.mousePress(window_obj, Qt.LeftButton, Qt.NoModifier, QPoint(round(point.x()), round(point.y())))
    QTest.mouseMove(window_obj, QPoint(round(target.x()), round(target.y())), 2)
    QTest.mouseRelease(window_obj, Qt.LeftButton, Qt.NoModifier, QPoint(round(target.x()), round(target.y())))
    settle()


def production_window(source):
    rows = []
    for identifier in ("problems", "connections", "targetConnections"):
        row = extract_row(source, identifier)
        row = row.replace("{", '''{
            objectName: "row-IDENTIFIER"
'''.replace("IDENTIFIER", identifier), 1)
        rows.append(row)
    return '''import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import JASP.Controls
import "Common" as Common
ApplicationWindow {
    id: window
    visible: true
    color: "#eeeeee"
    width: Math.max(900 * preferencesModel.uiScale, rows.width + 40 * preferencesModel.uiScale)
    height: rows.height + 40 * preferencesModel.uiScale
    property real paneWidth: 550
    property real fontMultiplier: 1
    property bool mirrored: false
    property int rowIndex: 0
    property string rowValue: "Waking at night"
    LayoutMirroring.enabled: mirrored
    LayoutMirroring.childrenInherit: true
    property QtObject preferencesModel: QtObject { property real uiScale: 1 }
    property QtObject jaspTheme: QtObject {
        property color sliderPartOff: "#dddddd"
        property color sliderPartOn: "#555555"
        property color textEnabled: "black"
        property color analysisBackgroundColor: "#eeeeee"
        property color borderColor: "#cccccc"
        property real borderRadius: 4 * preferencesModel.uiScale
        property real iconSize: 16 * preferencesModel.uiScale
    }
    property QtObject fromGroup: QtObject { property string fromName: "Trouble falling asleep" }
    property QtObject problems: QtObject {
        property real availableWidth: paneWidth * preferencesModel.uiScale
        property real columnSpacing: 10 * preferencesModel.uiScale
    }
    property QtObject connections: QtObject {
        property real availableWidth: paneWidth * preferencesModel.uiScale
        property real columnSpacing: 10 * preferencesModel.uiScale
    }
    property QtObject targetConnections: QtObject {
        property real availableWidth: paneWidth * preferencesModel.uiScale
    }
    property QtObject networkSeverityMaximum: QtObject { property real value: 1 }
    property QtObject networkConnectionMaximum: QtObject { property real value: 1 }
    property QtObject networkConnectionInput: QtObject { property string currentValue: "signed" }
    Column {
        id: rows
        x: 20 * preferencesModel.uiScale
        y: 20 * preferencesModel.uiScale
        spacing: 20 * preferencesModel.uiScale
        ROWS
    }
}
'''.replace("ROWS", "\n".join(rows))


def failure_control(folder):
    # Retained former layout: the 190px slider and expanding label row do not
    # share geometry. This must reproduce a mismatch, independently of the fix.
    source = '''import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
ApplicationWindow {
    visible: true
    width: 700
    height: 120
    ColumnLayout {
        width: 600
        Slider { objectName: "old-slider"; Layout.preferredWidth: 190 }
        RowLayout {
            Layout.fillWidth: true
            Label { objectName: "old-label"; text: "0" }
            Item { Layout.fillWidth: true }
            Label { objectName: "old-label"; text: "0.5" }
            Item { Layout.fillWidth: true }
            Label { objectName: "old-label"; text: "1" }
        }
    }
}'''
    with window(folder, source, "OldLayout") as (_, obj, _):
        slider = obj.findChild(QQuickItem, "old-slider")
        labels = obj.findChildren(QQuickItem, "old-label")
        labels.sort(key=lambda label: center(label, slider).x())
        measurement = geometry(slider, labels)
        check(max(abs(a - b) for a, b in zip(measurement["labels"], measurement["travel"])) > 100,
              ("retained old layout must reproduce the reported alignment failure", measurement))
        print("PASS negative control: former slider/tick layout has >100px endpoint mismatch")


def run(folder):
    write_substitutes(folder)
    common = folder / "Common"
    common.mkdir()
    (common / "NetworkRating.qml").write_text((ROOT / "inst/qml/common/NetworkRating.qml").read_text())
    failure_control(folder)
    source = production_window((ROOT / "inst/qml/Network.qml").read_text())
    with window(folder, source, "ProductionRows") as (engine, obj, _):
        rows = {name: obj.findChild(QQuickItem, "row-" + name)
                for name in ("problems", "connections", "targetConnections")}
        controls = {name: parts(row) for name, row in rows.items()}
        recorded = {name: [] for name in rows}
        for name, (rating, _, _) in controls.items():
            rating.recorded.connect(lambda name=name: recorded[name].append(1))
        fields = {name: rating.property("boundField") for name, (rating, _, _) in controls.items()}
        widths = []
        normalized_travel = []
        cases = [(*case, False) for case in product(
            (1, 1.5, 2), (550, 1400), (1, 10, 1000), ("signed", "magnitude"), (1, 1.5)
        )]
        cases += [(1.5, 1400, 1000, mode, 1.5, True) for mode in ("signed", "magnitude")]
        for scale, pane, maximum, mode, font_size, mirrored in cases:
            before = {name: float(field.property("value")) for name, field in fields.items()}
            events = {name: len(count) for name, count in recorded.items()}
            evaluate(engine, obj, f"preferencesModel.uiScale = {scale}; paneWidth = {pane}; fontMultiplier = {font_size}; "
                     f"mirrored = {str(mirrored).lower()}; "
                     f"networkSeverityMaximum.value = {maximum}; networkConnectionMaximum.value = {maximum}; "
                     f"networkConnectionInput.currentValue = '{mode}'")
            settle()
            check(before == {name: float(field.property("value")) for name, field in fields.items()},
                  ("presentation/resize changed canonical ratings", scale, pane, maximum, mode))
            check(events == {name: len(count) for name, count in recorded.items()},
                  ("presentation/resize recorded a rating", scale, pane, maximum, mode))
            lengths = []
            for name, (rating, slider, labels) in controls.items():
                detail = (name, scale, pane, maximum, mode, font_size, mirrored)
                measurement = geometry(slider, labels)
                assert_geometry(measurement, detail)
                assert_auxiliary_alignment(rating, slider, detail)
                assert_parent_layout(rows[name], rating, detail)
                lengths.append(measurement["travel"][2] - measurement["travel"][0])
                normalized_travel.append(lengths[-1] / scale)
                widths.append((scale, font_size, measurement["width"]))
                expected_ticks = [float(slider.property("from")) * maximum,
                                  (float(slider.property("from")) + 1) * maximum / 2, maximum]
                if mirrored:
                    expected_ticks.reverse()
                check([float(text) for text in measurement["text"]] == expected_ticks,
                      (detail, "tick values", measurement))
                GEOMETRY.append({"case": detail, **measurement})
                for index, position in enumerate(measurement["travel"]):
                    # Set a distinct starting value without recording, then drag
                    # the real handle with actual Qt mouse press/move/release.
                    field = fields[name]
                    starting = -0.37 if name != "problems" else 0.37
                    field.setProperty("value", starting)
                    settle()
                    events_before = len(recorded[name])
                    drag(obj, slider, position, index)
                    after = geometry(slider, labels)
                    value_index = 2 - index if mirrored else index
                    expected = float(slider.property("from")) + value_index * (1 - float(slider.property("from"))) / 2
                    if mode == "magnitude" and name != "problems":
                        expected *= -1
                    # The midpoint can fall between physical mouse pixels;
                    # allow only that coordinate rounding plus half one step.
                    step = float(slider.property("stepSize"))
                    tolerance = (1 - float(slider.property("from"))) * 0.51 / lengths[-1] + step / 2 + 1e-7
                    if index in (0, 2):
                        tolerance = 1e-7
                    check(abs(float(field.property("value")) - expected) <= tolerance,
                          (detail, index, field.property("value"), expected, tolerance))
                    check(len(recorded[name]) > events_before, (detail, "mouse drag must record", index))
                    # QTest rounds the mouse coordinate, then Slider snaps its
                    # value. Qt 6.8.3 Fusion/Slider.qml separately Math.rounds
                    # the handle's x offset; Basic leaves it fractional.
                    # Account for these independent errors, rather than using
                    # the value tolerance alone for the rendered handle.
                    mouse_rounding = 0.51 if index == 1 else 0
                    handle_rounding = 0.51 if os.environ["QT_QUICK_CONTROLS_STYLE"] == "Fusion" else 0
                    snap_rounding = (step * lengths[-1] / (1 - float(slider.property("from"))) / 2
                                     if index == 1 else 0)
                    handle_tolerance = mouse_rounding + handle_rounding + snap_rounding + 1e-7
                    check(abs(after["handle"] - position) <= handle_tolerance,
                          (detail, index, "thumb center", after))
            check(max(lengths) - min(lengths) < 0.1,
                  ("shared severity/manual/all-pairs track length", scale, pane, maximum, mode, lengths))
            if OUTPUT and (maximum == 1000 or (maximum == 1 and scale == 1 and pane == 550 and font_size == 1)):
                destination = Path(OUTPUT)
                destination.mkdir(parents=True, exist_ok=True)
                filename = (f"rating-scale-{scale}-pane-{pane}-max-{maximum}-"
                            f"{mode}-font-{font_size}-rtl-{mirrored}.png")
                check(obj.grabWindow().save(str(destination / filename)), filename)
        for scale, font_size in product((1, 1.5, 2), (1, 1.5)):
            same_scale = [width for at_scale, at_font, width in widths
                          if at_scale == scale and at_font == font_size]
            check(max(same_scale) - min(same_scale) < 0.1,
                  ("slider width changed with parent, maximum or mode", scale, font_size, same_scale))
        # Qt layouts round allocated widths to pixels while font metrics and
        # endpoint padding can be fractional. Allow one logical pixel here;
        # each case still requires all three actual tracks to agree at 0.1px.
        check(max(normalized_travel) - min(normalized_travel) <= 1,
              ("travel changes only with UI scale, not font size or other presentation", normalized_travel))
        for name, (rating, _, _) in controls.items():
            rating.recorded.disconnect()
    if OUTPUT:
        (Path(OUTPUT) / "geometry.json").write_text(json.dumps(GEOMETRY, indent=2) + "\n")


if __name__ == "__main__":
    app = QGuiApplication(sys.argv)
    with tempfile.TemporaryDirectory(prefix="learnn1-rating-layout-") as temporary:
        run(Path(temporary))
    print(f"PASS {CHECKS} checks; Qt {qVersion()}; actual Network rows/shared slider, JASP presentation substitutes")
    print("Native JASP control integration and Desktop rendering remain separate acceptance checks.")
