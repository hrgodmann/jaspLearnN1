import QtQuick		2.12
import JASP.Module	1.0

Description
{
	name		: "jaspLearnN1"
	title		: qsTr("Learn N=1")
	description	: qsTr("Discover analyses for N=1 data.")
	version		: "0.1"
	author		: "JASP Team"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "https://jasp-stats.org"
	license		: "GPL (>= 2)"
	icon		: "learnN1.svg"
	preloadData : false

	Analysis
	{
		title	:	qsTr("Does The Treatment Work?")
		func	:	"Treatment"
		qml		:	"Treatment.qml"
		requiresData:	false
	}

	Analysis
	{
		title	:	qsTr("How Do Symptoms Develop?")
		func	:	"Forecasting"
		qml		:	"Forecasting.qml"
		requiresData:	false
	}

	Analysis
	{
		title	:	qsTr("How Are Symptoms Connected?")
		func	:	"Network"
		qml		:	"Network.qml"
		requiresData:	false
	}
}
