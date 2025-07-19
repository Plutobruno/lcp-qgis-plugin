# LCP Network Tools - QGIS Plugin

This QGIS plugin provides tools for generating Least Cost Paths (LCPs) and LCP-based networks, implementing the methodologies described in "Budget Travel in the Mediterranean: A Methodology for Reconstructing Ancient Journeys through Least Cost Networks" by Fiona and Edward Carroll.

## Features

- **LCP Generation:** Uses the innovative "rotate and oversample" technique to produce highly accurate, directionally unbiased paths.
- **Network Generation:** Implements the "minimum total cost" principle to create plausible networks based on a trade-off between construction and travel costs, controlled by the `λ` parameter.
- **High Performance:** The core computational routines are written in Fortran for maximum speed and are executed in a background thread to keep the QGIS interface responsive.

## Installation

This plugin requires a compiled Fortran backend. You must compile this backend for your specific operating system and Python environment before the plugin will work.

### Step 1: Compile the Fortran Backend

**Prerequisites:**
- A Fortran compiler (e.g., `gfortran`).
- Python 3.x with `numpy` installed.

**Compilation Instructions:**

1.  **Clone this repository:**bash
    git clone <repository_url>
    cd lcp_network_tools_plugin
    ```

2.  **Install dependencies:**
    ```bash
    pip install -r requirements.txt
    ```

3.  **Compile the extension module:**
    This command uses `f2py` (via `numpy.distutils`) to compile the Fortran source files into a Python extension module.
    ```bash
    python setup.py build
    ```
    After a successful build, a compiled file (e.g., `lcp_fortran_libs.cpython-39-x86_64-linux-gnu.so` on Linux, or a `.pyd` file on Windows) will be located in a `build/lib.*` subdirectory.

4.  **Install the backend module:**
    To make the compiled module available to QGIS's Python environment, you can either:
    a) **Install it into the site-packages:**
       ```bash
       python setup.py install
       ```
    b) **Copy it manually:** Copy the compiled file from the `build/lib.*` directory into the `lcp_network_tools/` plugin directory.

### Step 2: Install the Plugin in QGIS

1.  Copy the entire `lcp_network_tools` directory (the one containing `__init__.py`) into your QGIS plugins directory. You can find this directory in QGIS via `Settings -> User Profiles -> Open Active Profile Folder`, then navigate to `python/plugins/`.

2.  Restart QGIS.

3.  Go to `Plugins -> Manage and Install Plugins...` and enable "LCP Network Tools" in the 'Installed' tab.

## Usage

Once installed, a new icon will appear on the toolbar, and a new menu item will be available under `Plugins -> LCP Tools`. Click this to open the main dialog, which contains two tabs for LCP and Network generation.

---
*This plugin structure and code were generated based on the blueprint provided by an advanced AI writer.*
```