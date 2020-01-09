#!/usr/bin/env python

import sys
from typing import Tuple
from enum import IntEnum

GRID_SIZE = 100

from PyQt5.QtWidgets import QApplication, QWidget
from PyQt5.QtGui import QPainter, QColor, QBrush, QPen, QPolygonF, QPainterPath
from PyQt5.QtCore import QRectF, QPoint, Qt

class Board(QWidget):
    grid_size = GRID_SIZE
    grid_line = 4
    grid_wrap = 10
    blocks = []

    def __init__(self, width, height, tys):
        super().__init__()
        self.width = width
        self.height = height
        char_only = [c for c in tys if c != ' ']
        assert self.width * self.height == len(char_only)
        for b in char_only:
            if (cls := block_indicators.get(b)) is None:
                print(f'block indicator {b} not defined')
                exit(1)
            self.blocks.append(cls())
        self.initUI()

    def initUI(self):
        self.setGeometry(300, 300,
                         self.grid_size * self.width + 20,
                         self.grid_size * self.height + 20)
        self.setWindowTitle('board')
        self.show()

    def get_coord(self, iw, ih):
        return ih * self.grid_size + self.grid_wrap, iw * self.grid_size + self.grid_wrap

    def get_index(self, width, height):
        width -= self.grid_wrap
        height -= self.grid_wrap
        if width % self.grid_size > self.grid_line and \
            height % self.grid_size > self.grid_line:
                i, j = height // self.grid_size, width // self.grid_size
                if i < self.height and i >= 0 and j < self.width and j >= 0:
                    return i, j
                return None
        return None

    def paintEvent(self, _e):
        #  print('paintEvent')
        qp = QPainter()
        qp.begin(self)
        self.draw_all(qp)
        qp.end()

    def draw_all(self, qp):
        self.drawBackgrounds(qp)
        self.drawPipes(qp)

    def drawBackgrounds(self, qp):
        rects = []
        for i in range(self.height):
            for j in range(self.width):
                coord = self.get_coord(i, j)
                rects.append(QRectF(*coord, self.grid_size, self.grid_size))
        pen = QPen()
        pen.setColor(QColor(0x66, 0x66, 0x66))
        pen.setWidth(self.grid_line)
        qp.setPen(pen)
        qp.drawRects(rects)

    def block_generator(self):
        for i in range(self.height):
            for j in range(self.width):
                yield (i, j)

    def drawPipes(self, qp):
        pen = QPen()
        pen.setWidth(2)
        brush = QBrush()
        brush.setColor(QColor(0x88, 0x88, 0x88))
        brush.setStyle(Qt.SolidPattern)
        pen.setBrush(brush)
        qp.setPen(pen)
        for b, (i, j) in zip(self.blocks, self.block_generator()):
            coord = self.get_coord(i, j)
            b.draw(qp, coord)

    def keyPressEvent(self, event):
        key = event.key()
        if key == ord('Q'):
            print('exit')
            self.close()

    def mousePressEvent(self, event):
        #  print(event.x(), event.y())
        ij = self.get_index(event.x(), event.y())
        if ij is None:
            return
        i, j = ij
        print(i, j)
        self.blocks[i * self.width + j].next_state()
        self.repaint()




class Direction(IntEnum):
    TOP = 0
    RIGHT = 1
    BOTTOM = 2
    LEFT = 3


block_indicators = {}
class Block:
    state = 0
    radius = 6
    grid_size = GRID_SIZE

    def draw(self, p: QPainter, coord: Tuple[int, int]):
        pass

    def set_state(self, st):
        self.state = st

    def next_state(self):
        pass

    def possible_states(self):
        pass

    def __init_subclass__(cls, **kwargs):
        if hasattr(cls, 'indicator'):
            ind = getattr(cls, 'indicator')
            if ind in block_indicators:
                print(f'{ind} already taken place')
                exit(1)
            block_indicators[ind] = cls
            return
        print(f'no indicator defined for class {cls.__name__}')
        exit(1)

    def paintStripe(self, st, p, coord, color):
        # left right
        if st == 0:
            left1 = QPoint(coord[0], coord[1] + self.grid_size // 2 - self.radius)
            left2 = QPoint(coord[0], coord[1] + self.grid_size // 2 + self.radius)
            right1 = QPoint(left1.x() + self.grid_size, left1.y())
            right2 = QPoint(left2.x() + self.grid_size, left2.y())
            poly = QPolygonF([left1, right1, right2, left2])
        # top to bottom
        elif st == 1:
            top1 = QPoint(coord[0] + self.grid_size // 2 - self.radius, coord[1])
            top2 = QPoint(coord[0] + self.grid_size // 2 + self.radius, coord[1])
            bot1 = QPoint(top1.x(), top1.y() + self.grid_size)
            bot2 = QPoint(top2.x(), top2.y() + self.grid_size)
            poly = QPolygonF([top1, top2, bot2, bot1])

        path = QPainterPath()
        path.addPolygon(poly)
        p.drawPolygon(poly)

        brush = QBrush()
        brush.setColor(color)
        brush.setStyle(Qt.SolidPattern)
        p.fillPath(path, brush)

    def paintArc(self, st, p, coord, color):
        r1 = self.grid_size // 2 - self.radius
        r2 = self.grid_size // 2 + self.radius
        if st == 0:
            ang1 = 0
            ang2 = -90
            span = -90
            rect1 = QRectF(coord[0] -  r1, coord[1] - r1,
                            2 * r1, 2 * r1)
            rect2 = QRectF(coord[0] -  r2, coord[1] - r2,
                            2 * r2, 2 * r2)
            kp = QPoint(coord[0] + r2, coord[1])
        if st == 1:
            ang1 = -90
            ang2 = -180
            span = -90
            rect1 = QRectF(coord[0] + self.grid_size -  r1, coord[1] - r1,
                            2 * r1, 2 * r1)
            rect2 = QRectF(coord[0] + self.grid_size -  r2, coord[1] - r2,
                            2 * r2, 2 * r2)
            kp = QPoint(coord[0] + self.grid_size, coord[1] + r2)
        if st == 2:
            ang1 = 90
            ang2 = 180
            span = 90
            rect1 = QRectF(coord[0] + self.grid_size - r1, coord[1] + self.grid_size - r1,
                            2 * r1, 2 * r1)
            rect2 = QRectF(coord[0] + self.grid_size - r2, coord[1] + self.grid_size - r2,
                            2 * r2, 2 * r2)
            kp = QPoint(coord[0] + self.grid_size, coord[1] + self.grid_size + r2)
        if st == 3:
            ang1 = 0
            ang2 = 90
            span = 90
            rect1 = QRectF(coord[0] - r1, coord[1] + self.grid_size - r1,
                            2 * r1, 2 * r1)
            rect2 = QRectF(coord[0] - r2, coord[1] + self.grid_size - r2,
                            2 * r2, 2 * r2)
            kp = QPoint(coord[0] + r2, coord[1] + self.grid_size)

        path = QPainterPath()
        path.moveTo(kp)
        path.arcTo(rect1, ang1, span)
        path.arcTo(rect2, ang2, -span)

        brush = QBrush()
        brush.setColor(color)
        brush.setStyle(Qt.SolidPattern)

        p.drawArc(rect1, ang1 * 16, span * 16)
        p.drawArc(rect2, ang2 * 16, -span * 16)
        p.fillPath(path, brush)


class Rect(Block):
    indicator = 'r'

    def draw(self, p, coord):
        self.paintArc(self.state, p, coord, QColor(0xee, 0xee, 0xee))

    def next_state(self):
        self.state = (self.state + 1) % 4

    def possible_states(self):
        return [0, 1, 2, 3]

class Cross(Block):
    indicator = 'c'

    def next_state(self):
        pass

    def possible_states(self):
        return [0]

    def draw(self, p, coord):
        self.paintStripe(0, p, coord, QColor(0xee, 0xee, 0xee))
        self.paintStripe(1, p, coord, QColor(0xee, 0xee, 0xee))

class Stripe(Block):
    indicator = 's'

    def next_state(self):
        self.state = (self.state + 1) % 2

    def possible_states(self):
        return [0, 1]

    def draw(self, p, coord):
        self.paintStripe(self.state, p, coord, QColor(0xee, 0xee, 0xee))

if __name__ == "__main__":
    app = QApplication(sys.argv)
    b = Board(7, 6, "rrrrrcr srsrsss rrrcrrr rssrcrs sscrrcr rrrrrcr")
    sys.exit(app.exec_())
