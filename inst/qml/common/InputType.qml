//
// Copyright (C) 2025 University of Amsterdam and Netherlands eScience Center
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls

RadioButtonGroup
{
	name: "inputType"
	title: qsTr("Input Type")
	columns: 3
	info: qsTr("Specifies whether to use simulated data or load data from the dataset.")

	RadioButton { value: "simulateData"; id: simulateData; label: qsTr("Simulate data"); checked: true; info: qsTr("Generate data from a simulation model with configurable parameters.") }
	RadioButton { value: "loadData"; id: loadData; label: qsTr("Load data"); info: qsTr("Use variables from the loaded dataset.") }
}
