#!/usr/bin/env python

import sys
import argparse

import PyQt5.QtCore as core
import PyQt5.QtGui as gui
import PyQt5.QtWidgets as widget
try:
    import PyQt5.QtDBus as dbus
except ModuleNotFoundError:
    dbus = None


# pylint: disable=too-many-instance-attributes,attribute-defined-outside-init
class Annotator(widget.QGraphicsView):
    colorTable = {
        'R': gui.QColor(255, 0, 0),
        'G': gui.QColor(0, 255, 0),
        'B': gui.QColor(0, 0, 255),
        'Y': gui.QColor(255, 255, 0),
        'P': gui.QColor(255, 0, 255),
        'C': gui.QColor(0, 255, 255),
        'W': gui.QColor(255, 255, 255),
        'K': gui.QColor(0, 0, 0),
        }
    ind = None

    def __init__(self, args):
        super().__init__()
        self.args = args
        self.lastPoint = None
        self.actions = {
            ord('S'): self.save,
            ord('Q'): widget.QApplication.quit,
        }
        self.restore = False
        self.keyState = False
        self.initGui()

    def save(self):
        if self.ind:
            self.gs.removeItem(self.ind)
        # TODO: problem when image too large && filename
        self.grab().save("test.png")
        if self.ind:
            self.gs.addItem(self.ind)

    def setPenColor(self, color):
        self.pen.setColor(self.colorTable[color])
        self.refresh_indicator()

    def refresh_indicator(self):
        if self.ind:
            self.gs.removeItem(self.ind)
            self.mkIndicator()
            self.gs.addItem(self.ind)

    def mousePressEvent(self, e):
        self.lastPoint = self.mapToScene(e.pos())
        self.keyState = True

    def mouseMoveEvent(self, e):
        p = self.mapToScene(e.pos())
        if self.keyState and self.lastPoint:
            self.gs.addLine(core.QLineF(self.lastPoint, p), self.pen)
        self.lastPoint = p
        #  print(self.lastPoint)

    def mouseReleaseEvent(self, e):
        self.lastPoint = self.mapToScene(e.pos())
        self.keyState = False
        #  print(self.lastPoint)

    def keyPressEvent(self, e):
        k = e.key()
        #  print(k)
        if k in self.actions.keys():
            self.actions[k]()
        elif k in map(ord, self.colorTable.keys()):
            self.setPenColor(chr(k))
        elif k > ord('0') and k <= ord('9'):
            self.pen.setWidth(k - ord('0'))
            self.refresh_indicator()

    def do_cleanup(self):
        if self.restore:
            self.interface.call("loadEffect", "diminactive")

    def mkIndicator(self):
        self.ind = widget.QGraphicsTextItem(f'{self.pen.width()}')
        self.ind.setDefaultTextColor(self.pen.color())
        font = gui.QFont()
        font.setPixelSize(100)
        self.ind.setFont(font)
        self.ind.setPos(core.QPoint(0, 0))

    def initGui(self):
        self.setWindowTitle("Anna")

        self.setFrameStyle(widget.QFrame.NoFrame)
        self.setStyleSheet("background-color: transparent")
        self.setAttribute(core.Qt.WA_TranslucentBackground)

        self.pen = gui.QPen()
        self.pen.setWidth(3)
        self.pen.setColor(gui.QColor(255, 0, 0))
        self.gs = widget.QGraphicsScene()
        self.setScene(self.gs)

        f = getattr(self.args, 'from')
        if f:
            reader = gui.QImageReader(f)
            reader.setAutoTransform(True)
            self.pixmap = gui.QPixmap.fromImage(reader.read())
            if self.pixmap.isNull():
                print('image read failed')
            else:
                self.gs.addPixmap(self.pixmap)
                pw, ph = self.pixmap.size().width(), \
                    self.pixmap.size().height()
                self.setGeometry(0, 0, pw, ph)
                self.setSceneRect(0, 0, pw, ph)
        else:
            self.setWindowFlags(core.Qt.X11BypassWindowManagerHint)
            rect = widget.QApplication.desktop().screenGeometry()
            self.setGeometry(rect)
            self.setSceneRect(core.QRectF(rect))
            self.showFullScreen()

            self.mkIndicator()
            self.gs.addItem(self.ind)
            if dbus:
                self.interface = dbus.QDBusInterface("org.kde.KWin",
                                                     "/Effects")
                resp = self.interface.call("Get", "org.kde.kwin.Effects",
                                           "activeEffects")
                if resp.type() == 2:
                    if "diminactive" in resp.arguments()[0]:
                        self.interface.call("unloadEffect", "diminactive")
                        self.restore = True
# pylint: enable=too-many-instance-attributes,attribute-defined-outside-init


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('from', nargs='?', default=None)
    args = parser.parse_args(sys.argv[1:])
    app = widget.QApplication(sys.argv)

    w = Annotator(args)
    app.aboutToQuit.connect(w.do_cleanup)
    w.show()

    sys.exit(app.exec_())


if __name__ == "__main__":
    main()
# qdbus org.kde.KWin /Effects loadEffect diminactive
