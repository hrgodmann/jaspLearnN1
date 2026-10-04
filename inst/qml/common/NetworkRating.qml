import QtQuick
import QtQuick.Controls as QtControls
import QtQuick.Layouts
import JASP
import JASP.Controls

// The hidden bound field stores the original canonical value. Presentation
// changes never write it; only an explicit edit records a new rating.
RowLayout
{
    id: rating
    property var boundField
    property real maximum: 1
    property bool signedRating: false
    property bool magnitudeMode: false
    property string ratingLabel: qsTr("Rating")
    property bool synchronizing: false
    signal recorded()
    spacing: 8 * preferencesModel.uiScale

    function synchronize() {
        if (!boundField || !slider || !display || !direction || !isFinite(Number(boundField.value))) return
        synchronizing = true
        var number = Number(boundField.value)
        var shown = signedRating && magnitudeMode ? Math.abs(number) : number
        slider.value = shown
        display.value = shown * maximum
        // Zero has no sign. Preserve a direction chosen for the next nonzero
        // magnitude when changing the scale or the input mode.
        if (number !== 0)
            direction.currentIndex = number < 0 ? 1 : 0
        synchronizing = false
    }

    function recordDisplayed(number) {
        if (!boundField || synchronizing || !isFinite(number) ||
                !isFinite(maximum) || maximum < 1 || maximum > 1000 || Math.floor(maximum) !== maximum) return
        var minimum = signedRating && !magnitudeMode ? -maximum : 0
        if (number < minimum || number > maximum) return
        var canonical = number / maximum
        if (signedRating && magnitudeMode && direction.currentIndex === 1)
            canonical = -canonical
        boundField.value = canonical
        synchronize()
        recorded()
    }

    onMaximumChanged: synchronize()
    onMagnitudeModeChanged: synchronize()
    onSignedRatingChanged: synchronize()
    onBoundFieldChanged: synchronize()
    Component.onCompleted: synchronize()
    Connections {
        target: rating.boundField
        function onValueChanged() { rating.synchronize() }
        function onInitializedChanged() { rating.synchronize() }
    }

    ColumnLayout {
        spacing: 0
        QtControls.Slider {
            id: slider
            from: rating.signedRating && !rating.magnitudeMode ? -1 : 0
            to: 1
            stepSize: (rating.maximum >= 10 ? 1 : 0.01) / rating.maximum
            snapMode: QtControls.Slider.SnapAlways
            Layout.preferredWidth: 190 * preferencesModel.uiScale
            Accessible.name: rating.ratingLabel
            onFromChanged: rating.synchronize()
            onMoved: rating.recordDisplayed(value * rating.maximum)
            background: Rectangle {
                x: slider.leftPadding
                y: slider.topPadding + slider.availableHeight / 2 - height / 2
                width: slider.availableWidth
                height: 6 * preferencesModel.uiScale
                radius: height / 2
                color: jaspTheme.sliderPartOff
                Rectangle {
                    width: slider.visualPosition * parent.width
                    height: parent.height
                    radius: parent.radius
                    color: jaspTheme.sliderPartOn
                }
                Rectangle {
                    // Every presentation has a visible midpoint. In signed
                    // connection mode this is also zero.
                    anchors.horizontalCenter: parent.horizontalCenter
                    anchors.verticalCenter: parent.verticalCenter
                    width: 2 * preferencesModel.uiScale
                    height: 14 * preferencesModel.uiScale
                    color: jaspTheme.textEnabled
                }
            }
        }
        RowLayout {
            Layout.fillWidth: true
            Label { text: (slider.from * rating.maximum).toString() }
            Item { Layout.fillWidth: true }
            Label { text: ((slider.from + slider.to) * rating.maximum / 2).toString() }
            Item { Layout.fillWidth: true }
            Label { text: rating.maximum.toString() }
        }
    }
    DoubleField {
        id: display
        isBound: false
        defaultValue: 0
        min: slider.from * rating.maximum
        max: rating.maximum
        negativeValues: rating.signedRating && !rating.magnitudeMode
        decimals: 2
        fieldWidth: 65 * preferencesModel.uiScale
        info: rating.ratingLabel
        onEditingFinished: {
            if (initialized && !rating.synchronizing && control.acceptableInput) {
                // TextField emits editingFinished before its value is updated.
                // Its setter performs JASP's native locale-aware conversion.
                value = displayValue
                rating.recordDisplayed(Number(value))
            }
        }
    }
    DropDown {
        id: direction
        isBound: false
        visible: rating.signedRating && rating.magnitudeMode
        values: [{label: qsTr("Increases"), value: "increases"},
                 {label: qsTr("Decreases"), value: "decreases"}]
        onCurrentIndexChanged: {
            if (initialized && !rating.synchronizing && visible && rating.boundField &&
                    Number(rating.boundField.value) !== 0)
                rating.recordDisplayed(Math.abs(Number(rating.boundField.value)) * rating.maximum)
        }
    }
    Button {
        label: rating.signedRating ? qsTr("Set to 0") : qsTr("Midpoint")
        onClicked: rating.recordDisplayed(rating.signedRating ? 0 : rating.maximum / 2)
    }
}
