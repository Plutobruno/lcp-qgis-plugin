# __init__.py
__version__ = '1.0.0'

def classFactory(iface):
    """Load LCPNetworkTools class from file lcp_network_tools.

    :param iface: A QGIS interface instance.
    :type iface: QgsInterface
    """
    from.lcp_network_tools import LCPNetworkTools
    return LCPNetworkTools(iface)
