# lcp_network_tools_dialog.py
import os
from qgis.PyQt import uic
from qgis.PyQt.QtWidgets import QDialog
from qgis.core import (QgsProject, QgsVectorLayer, QgsRasterLayer, Qgis, QgsMessageLog)
from qgis.gui import QgsMapToolEmitPoint

# This loads the.ui file so that PyQt can populate the plugin with the elements from Qt Designer
FORM_CLASS, _ = uic.loadUiType(os.path.join(
    os.path.dirname(__file__), 'lcp_network_tools_dialog_base.ui'))

class LCPNetworkToolsDialog(QDialog, FORM_CLASS):
    def __init__(self, iface, parent=None):
        """Constructor."""
        super(LCPNetworkToolsDialog, self).__init__(parent)
        self.setupUi(self)
        self.iface = iface
        self.map_tool = None
        self.active_point_target = None # 'start' or 'dest'

        self.setup_ui_connections()
        self.populate_comboboxes()

    def setup_ui_connections(self):
        """Connect UI element signals to methods."""
        self.button_box.accepted.connect(self.run_analysis)
        self.button_box.rejected.connect(self.reject)

        # LCP Tab connections
        self.mMapLayerComboBox_dem.setFilters(QgsMapLayerComboBox.RasterLayer)
        self.pushButton_selectStart.clicked.connect(self.select_start_point_on_map)
        self.pushButton_selectDest.clicked.connect(self.select_dest_point_on_map)

        # Network Tab connections
        self.mMapLayerComboBox_nodes.setFilters(QgsMapLayerComboBox.PointLayer)
        self.mMapLayerComboBox_dem_net.setFilters(QgsMapLayerComboBox.RasterLayer)

    def populate_comboboxes(self):
        """Populate combo boxes with predefined options."""
        # Cost Functions
        self.comboBox_costMethod.addItems()
        # Move Types
        self.comboBox_moveType.addItems()
        # Network Algorithms
        self.comboBox_algo.addItems()

    def log(self, message, level=Qgis.Info):
        """Log messages to the UI and QGIS log."""
        self.textEdit_log.append(message)
        QgsMessageLog.logMessage(message, 'LCP Tools', level)

    def select_point_on_map(self, target):
        """Activate map tool to select a point."""
        self.active_point_target = target
        self.map_tool = QgsMapToolEmitPoint(self.iface.mapCanvas())
        self.map_tool.canvasClicked.connect(self.handle_map_click)
        self.iface.mapCanvas().setMapTool(self.map_tool)
        self.log(f"Click on the map to select a {target} point.")

    def select_start_point_on_map(self):
        self.select_point_on_map('start')

    def select_dest_point_on_map(self):
        self.select_point_on_map('dest')

    def handle_map_click(self, point, button):
        """Handle the map click event from the map tool."""
        if self.active_point_target == 'start':
            self.lineEdit_start.setText(f"{point.x()},{point.y()}")
        elif self.active_point_target == 'dest':
            current_text = self.textEdit_dest.toPlainText()
            new_point_text = f"{point.x()},{point.y()}"
            if current_text:
                self.textEdit_dest.setText(f"{current_text}\n{new_point_text}")
            else:
                self.textEdit_dest.setText(new_point_text)
        
        self.iface.mapCanvas().unsetMapTool(self.map_tool)
        self.map_tool = None
        self.log(f"Selected {self.active_point_target} point: {point.x():.2f}, {point.y():.2f}")

    def run_analysis(self):
        """Determine which analysis to run based on the active tab."""
        current_tab_index = self.tabWidget.currentIndex()
        if current_tab_index == 0:
            self.run_lcp_generation()
        elif current_tab_index == 1:
            self.run_network_generation()

    def run_lcp_generation(self):
        """Prepare and run the LCP generation task."""
        self.log("Starting LCP Generation...")
        # TODO: Implement parameter gathering and task creation for LCP
        self.log("LCP Generation logic not yet implemented.", Qgis.Warning)
        self.accept()

    def run_network_generation(self):
        """Prepare and run the Network generation task."""
        self.log("Starting Network Generation...")
        # TODO: Implement parameter gathering and task creation for Network
        self.log("Network Generation logic not yet implemented.", Qgis.Warning)
        self.accept()

