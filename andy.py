#!/usr/bin/env python

import sys
import os
from functools import partial
import PyQt5.QtWidgets as widgets
import PyQt5.QtGui as gui

#  ICON = '/usr/share/icons/breeze-dark/devices/22/network-vpn.svg'
ICON = '/home/adv_zxy/.local/share/icons/vpn.png'

def toggleWG(_, chk):
    if chk:
        os.system('sudo wg-control up')
    else:
        os.system('sudo wg-control down')

def initWG(act):
    if not os.system('sudo wg | grep interface'):
        act.setChecked(True)

MENUS = {
    'Connected': (toggleWG, initWG),
    }

class Indicator(widgets.QSystemTrayIcon):
    def __init__(self):
        super().__init__()
        self.setIcon(gui.QIcon(ICON))

        self.menu = widgets.QMenu()
        for m, f in MENUS.items():
            act = self.menu.addAction(m)
            act.triggered.connect(partial(f[0], act))
            act.setCheckable(True)
            if f[1]:
                f[1](act)
        self.menu.addSeparator()
        self.menu.addAction('Quit').triggered \
          .connect(widgets.QApplication.quit)
        self.setContextMenu(self.menu)

def main():
    app = widgets.QApplication(sys.argv)
    ind = Indicator()
    ind.show()
    sys.exit(app.exec_())

if __name__ == "__main__":
    main()
