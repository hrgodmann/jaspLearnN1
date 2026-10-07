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
    Layout.fillWidth: false

    function synchronize() {
        if (!boundField || !slider || !display || !direction || !isFinite(Number(boundField.value))) return
        synchronizing = true
        var number = Number(boundField.value)
        var shown = signedRating && magnitudeMode ? Math.abs(number) : number
        slider.value = shown
        display.value = shown * maximum
        display.userEdited = false
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

    function finishNumericEdit(confirmed) {
        // Focus loss is not a rating. A restored invalid entry is not one either.
        var edited = display.userEdited && display.editedText === display.displayValue
        var confirmedUnchanged = confirmed && !display.userEdited
        display.userEdited = false
        if ((!edited && !confirmedUnchanged) || !display.initialized || synchronizing ||
                !display.control.acceptableInput) return
        // JASP emits editingFinished before updating value; use its locale-aware setter.
        display.value = display.displayValue
        recordDisplayed(Number(display.value))
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
        // Keep the same usable travel in severity, manual and all-pairs rows.
        // Card layouts must not stretch the labels independently of the track.
        Layout.minimumWidth: slider.implicitWidth
        Layout.preferredWidth: slider.implicitWidth
        Layout.maximumWidth: slider.implicitWidth
        Layout.fillHeight: false
        spacing: 0
        TextMetrics {
            id: tickLabelMetrics
            font: minimumLabel.font
            // Reserve room for the widest supported endpoint on every scale.
            text: "-1000"
        }
        QtControls.Slider {
            id: slider
            readonly property real handleWidth: handle ? handle.width : 0
            readonly property real trackStart: leftPadding + handleWidth / 2
            readonly property real trackLength: Math.max(0, availableWidth - handleWidth)
            leftPadding: Math.max(6 * preferencesModel.uiScale,
                                  (Math.max(tickLabelMetrics.width, tickLabelMetrics.advanceWidth) - handleWidth) / 2)
            rightPadding: leftPadding
            implicitWidth: 200 * preferencesModel.uiScale + leftPadding + rightPadding + handleWidth
            from: rating.signedRating && !rating.magnitudeMode ? -1 : 0
            to: 1
            stepSize: (rating.maximum >= 10 ? 1 : 0.01) / rating.maximum
            snapMode: QtControls.Slider.SnapAlways
            Layout.fillWidth: true
            Accessible.name: rating.ratingLabel
            onFromChanged: rating.synchronize()
            onMoved: rating.recordDisplayed(value * rating.maximum)
            background: Rectangle {
                x: slider.trackStart
                y: slider.topPadding + slider.availableHeight / 2 - height / 2
                width: slider.trackLength
                height: 6 * preferencesModel.uiScale
                radius: height / 2
                color: jaspTheme.sliderPartOff
                Rectangle {
                    x: slider.mirrored ? parent.width - width : 0
                    width: slider.position * parent.width
                    height: parent.height
                    radius: parent.radius
                    color: jaspTheme.sliderPartOn
                }
                Rectangle {
                    // Every presentation has a visible midpoint. In signed
                    // connection mode this is also zero.
                    anchors.horizontalCenter: parent.horizontalCenter
                    anchors.verticalCenter: parent.verticalCenter
                    anchors.alignWhenCentered: false
                    width: 2 * preferencesModel.uiScale
                    height: 14 * preferencesModel.uiScale
                    color: jaspTheme.textEnabled
                }
            }
        }
        Item {
            id: tickLabels
            Layout.fillWidth: true
            implicitHeight: Math.max(minimumLabel.implicitHeight, midpointLabel.implicitHeight, maximumLabel.implicitHeight)
            // Label centers follow the handle's actual travel, including its
            // half-width and the style's padding, rather than a separate row.
            Label {
                id: minimumLabel
                x: slider.x - tickLabels.x + slider.trackStart + (slider.mirrored ? slider.trackLength : 0) - width / 2
                text: (slider.from * rating.maximum).toString()
            }
            Label {
                id: midpointLabel
                x: slider.x - tickLabels.x + slider.trackStart + slider.trackLength / 2 - width / 2
                text: ((slider.from + slider.to) * rating.maximum / 2).toString()
            }
            Label {
                id: maximumLabel
                x: slider.x - tickLabels.x + slider.trackStart + (slider.mirrored ? 0 : slider.trackLength) - width / 2
                text: rating.maximum.toString()
            }
        }
    }
    DoubleField {
        id: display
        // Reserve the scale-label row so all inputs center on the track.
        Layout.bottomMargin: tickLabels.implicitHeight
        property bool userEdited: false
        property string editedText: ""
        isBound: false
        defaultValue: 0
        min: slider.from * rating.maximum
        max: rating.maximum
        negativeValues: rating.signedRating && !rating.magnitudeMode
        decimals: 2
        fieldWidth: 65 * preferencesModel.uiScale
        info: rating.ratingLabel
        onTextEdited: {
            if (initialized && !rating.synchronizing) {
                userEdited = true
                editedText = displayValue
            }
        }
        onEditingFinished: rating.finishNumericEdit(false)
    }
    Connections {
        target: display.control
        function onAccepted() { rating.finishNumericEdit(true) }
    }
    DropDown {
        id: direction
        Layout.bottomMargin: tickLabels.implicitHeight
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
        Layout.bottomMargin: tickLabels.implicitHeight
        label: rating.signedRating ? qsTr("Set to 0") : qsTr("Midpoint")
        onClicked: rating.recordDisplayed(rating.signedRating ? 0 : rating.maximum / 2)
    }
}
