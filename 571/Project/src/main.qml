import QtQuick 2.9
import QtQuick.Window 2.2
import QtQuick.Controls 1.4
import Project 1.0

MainWindow {
	ip: window
	visible: true
	width: 300
	height: 160
	title: qsTr("NetSpectre")
	Column {
		Row {
			padding: 5
			anchors.margins: 5
			Text {
				font.pixelSize: 20
				text: 'Victim IP:'
			}
			TextField {
				height: 25
				width: 150
				placeholderText: 'xxx.xxx.xxx.xxx'
			}
		}
		Button {
			anchors.margins: 5
			width: 300
			text: 'Start'
		}
		Row {
			padding: 5
			Rectangle {
				border.color: 'black'
				width: 30
				height: 30
				Text {
					anchors.centerIn: parent
					font.pixelSize: 25
					text: '?'
				}
			}
			Rectangle {
				border.color: 'black'
				width: 30
				height: 30
				Text {
					anchors.centerIn: parent
					font.pixelSize: 25
					text: '?'
				}
			}
			Rectangle {
				border.color: 'black'
				width: 30
				height: 30
				Text {
					anchors.centerIn: parent
					font.pixelSize: 25
					text: '?'
				}
			}
			Rectangle {
				border.color: 'black'
				width: 30
				height: 30
				Text {
					anchors.centerIn: parent
					font.pixelSize: 25
					text: '?'
				}
			}
			Rectangle {
				border.color: 'black'
				width: 30
				height: 30
				Text {
					anchors.centerIn: parent
					font.pixelSize: 25
					text: '?'
				}
			}
			Rectangle {
				border.color: 'black'
				width: 30
				height: 30
				Text {
					anchors.centerIn: parent
					font.pixelSize: 25
					text: '?'
				}
			}
			Rectangle {
				border.color: 'black'
				width: 30
				height: 30
				Text {
					anchors.centerIn: parent
					font.pixelSize: 25
					text: '?'
				}
			}
			Rectangle {
				border.color: 'black'
				width: 30
				height: 30
				Text {
					anchors.centerIn: parent
					font.pixelSize: 25
					text: '?'
				}
			}
			Text {
				font.pixelSize: 25
				text: '=?'
			}
		}
		Text {
			text: 'Current Bit Index:' + '\nCurrent Iterations:'
		}
	}
}
