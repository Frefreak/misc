#!/usr/bin/env python3

import os
import queue
import subprocess
import sys
import time
from concurrent.futures import ThreadPoolExecutor
from enum import Enum
from functools import partial
from hashlib import md5
from http.client import IncompleteRead
from threading import Thread, current_thread
from threading import enumerate as thread_enumerate

# pyqt5
import requests
from PyQt5.QtCore import Qt, pyqtSignal
from PyQt5.QtWidgets import (QApplication, QCheckBox, QFileDialog, QFormLayout,
                             QHBoxLayout, QHeaderView, QInputDialog, QLabel,
                             QLineEdit, QListWidget, QListWidgetItem,
                             QMainWindow, QMessageBox, QProgressBar,
                             QPushButton, QSpacerItem, QTableWidget,
                             QTableWidgetItem, QTextEdit, QVBoxLayout, QWidget)

JOB = 7
USER_AGENT = 'Mozilla/5.0 (X11; Linux x86_64; rv:127.0) Gecko/20100101 Firefox/127.0'

def parse(txt: str, base: str):
    lns = txt.splitlines()
    new_file = []   # for ffmpeg, local file
    files = []      # for download, full path
    total_time = 0.
    i = 0
    cnt = 0
    keyfile = None
    while i < len(lns):
        if lns[i].startswith('#EXTINF:'):
            new_file.append(lns[i])
            time = lns[i].lstrip('#EXTINF:').split(',')[0]
            total_time += float(time)

            url = lns[i + 1]
            filename = url.split('?')[0].split('/')[-1] + '.' + str(cnt)
            cnt += 1
            new_file.append(filename)
            if url.startswith('http://') or url.startswith('https://'):
                files.append((url, filename))
            else:
                if base[-1] == '/':
                    inter = ''
                else:
                    inter = '/'
                if url[0] == '/':
                    url = url[1:]
                files.append((base + inter + url, filename))
            i += 1
        elif lns[i].startswith('#EXT-X-KEY'):
            #EXT-X-KEY:METHOD=AES-128,URI="xxx.key",IV=0xdeadbeef...
            attrs_text = ':'.join(lns[i].split(':')[1:])
            attrs = {}
            for kv in attrs_text.split(','):
                if '=' in kv:
                    t = kv.split('=', 1)
                    if len(t) == 2:
                        attrs[t[0]] = t[1]
            if attrs.get('METHOD') and (uri := attrs.get('URI')):
                uri = uri.strip('"')
                if '://' in uri:
                    keyfile = uri
                else:
                    keyfile = base + '/' + uri
            # construct EXT-X-KEY line
            l = '#EXT-X-KEY:'
            vals = []
            for k, v in attrs.items():
                if k == 'URI':
                    vals.append(f'URI="keyfile"')
                else:
                    vals.append(f'{k}={v}')
            l += ','.join(vals)
            new_file.append(l)

        else:
            new_file.append(lns[i])
        i += 1
    return new_file, files, keyfile, total_time


class AlertMixin():
    popup = pyqtSignal(str)

    def __init__(self):
        super().__init__()
        self.popup.connect(self._alert)

    def _alert(self, msg):
        QMessageBox.warning(self, 'Warning', msg, QMessageBox.Ok)

    def alert(self, msg):
        self.popup.emit(msg)


class Cookie(QWidget, AlertMixin):
    def __init__(self):
        super().__init__()
        self.init()

    def init(self):
        layout = QVBoxLayout()
        self.setLayout(layout)

        btn_layout = QHBoxLayout()

        self.checkbox = QCheckBox("Need Cookie?")
        self.checkbox.clicked.connect(self.toggle_cookie_display)
        self.input_btn = QPushButton("paste")
        self.input_btn.setDisabled(True)
        self.input_btn.clicked.connect(self.paste_cookie)

        self.btn_plus = QPushButton("+")
        self.btn_plus.setDisabled(True)

        self.btn_minus = QPushButton("-")
        self.btn_minus.setDisabled(True)

        btn_layout.addWidget(self.checkbox)
        btn_layout.addWidget(self.input_btn)
        btn_layout.addWidget(self.btn_plus)
        btn_layout.addWidget(self.btn_minus)
        btn_layout.addStretch(1)

        layout.addLayout(btn_layout)

        self.cookie_tab = QTableWidget(1, 2)
        self.cookie_tab.setHorizontalHeaderLabels(['Key', 'Value'])
        header = self.cookie_tab.horizontalHeader()
        header.setSectionResizeMode(0, QHeaderView.ResizeToContents)
        header.setSectionResizeMode(1, QHeaderView.Stretch)

        self.cookie_tab.setVisible(False)
        layout.addWidget(self.cookie_tab)

        self.btn_plus.clicked.connect(lambda:
                self.cookie_tab.setRowCount(self.cookie_tab.rowCount() + 1))
        self.btn_minus.clicked.connect(lambda:
                self.cookie_tab.setRowCount(self.cookie_tab.rowCount() - 1))

    def toggle_cookie_display(self, evt):
        for w in [self.input_btn, self.btn_plus, self.btn_minus]:
            w.setEnabled(evt)
        self.cookie_tab.setVisible(evt)

    def paste_cookie(self, evt):
        txt, ok = QInputDialog.getText(self, "input cookie text", "cookie")
        if not ok:
            return
        if not txt:
            self.cookie_tab.setRowCount(1)
            self.cookie_tab.setItem(0, 0, QTableWidgetItem(''))
            self.cookie_tab.setItem(0, 1, QTableWidgetItem(''))
            return

        if (txt := txt.strip())[:7].lower() == 'cookie:':
            txt = txt[7:].strip()
        kv_collect = []
        for kv in txt.split('; '):
            kvs = kv.strip().split('=', maxsplit=1)
            if len(kvs) != 2:
                self.alert(f'error parsing cookie key-value: {kv}')
                continue
            kv_collect.append((kvs[0], kvs[1]))
        self.cookie_tab.setRowCount(len(kv_collect))
        for i, (k, v) in enumerate(kv_collect):
            self.cookie_tab.setItem(i, 0, QTableWidgetItem(k))
            self.cookie_tab.setItem(i, 1, QTableWidgetItem(v))

    def get_cookie(self):
        cookies = []
        for i in range(self.cookie_tab.rowCount()):
            k = self.cookie_tab.item(i, 0)
            v = self.cookie_tab.item(i, 1)
            if k and v:
                k = k.text()
                v = v.text()
                if k and v:
                    cookies.append(k + '=' + v)
        if cookies:
            return '; '.join(cookies)
        return None


class DownloadStatus(Enum):
    ReqParseError   = "E_REQPARSE   (F) "
    NetworkError    = "E_NETWORK    (R) "
    IncompReadError = "E_INCOMPREAD (R) "
    ReadError       = "E_READ       (R) "
    WriteError      = "E_WRITE      (F) "
    UnknownError    = "E_UNKNOWN    (F) "
    NotFoundError   = "E_404        (F) "
    OK              = "OK           (T) "
    INFO            = "INFO             "


class JobStatus(QWidget):
    progress = pyqtSignal(int)
    stat_sig = pyqtSignal(int, int, str)
    phase_sig = pyqtSignal(str)
    def __init__(self, task_id, total, output='output.mp4'):
        super().__init__()
        self.task_id = task_id
        self.total = total
        self.finished = 0
        self.ok = 0
        self.target = output
        self.init()

    def init(self):
        row_layout = QHBoxLayout()
        self.setLayout(row_layout)

        self.prog_bar = QProgressBar()
        self.prog_bar.setMaximum(self.total)
        self.prog_bar.setTextVisible(True)
        self.progress.connect(self._update_progressbar)

        self.dest = QLineEdit()
        self.dest.setText(self.target)
        self.dest.setMaximumWidth(100)
        self.stat = QLabel()
        self.phase = QLabel()
        self.task_label = QLabel()
        self.task_label.setText(self.task_id)

        self.stat.setEnabled(False)
        self.stat.setMinimumWidth(48)
        self.phase.setEnabled(False)
        self.phase.setMinimumWidth(32)

        self.stat_sig.connect(self._update_stat)
        self.phase_sig.connect(self._update_phase)

        row_layout.addWidget(self.task_label)
        row_layout.addWidget(self.dest)
        row_layout.addWidget(self.prog_bar)
        row_layout.addWidget(self.stat)
        row_layout.addWidget(self.phase)

    def get_target(self):
        return self.dest.text()

    def _update_progressbar(self, val):
        self.prog_bar.setValue(val)

    def _update_stat(self, ok, finished, phase):
        self.stat.setText(f'{ok}/{finished - ok}/{self.total}')
        self.phase.setText(phase)

    def update_stat(self, ok, finished, phase):
        self.progress.emit(finished)
        self.stat_sig.emit(ok, finished, phase)

    def set_total(self, total):
        self.total = total
        self.prog_bar.setMaximum(total)

    def _update_phase(self, phase):
        self.phase.setText(phase)

    def update_phase(self, phase):
        self.phase_sig.emit(phase)


class MainWindow(QMainWindow, AlertMixin):

    job_sig = pyqtSignal(JobStatus)

    def __init__(self, title):
        super().__init__()
        self.setWindowTitle(title)
        self.resize(800, 600)
        self.init()

    def init(self):
        self.widget = QWidget(self)
        self.layout = QFormLayout()
        self.widget.setLayout(self.layout)
        self.setCentralWidget(self.widget)

        url_layout = QHBoxLayout()
        url_layout.addWidget(QLabel("URL"))
        self.url = QLineEdit()
        url_layout.addWidget(self.url)
        #  select_file_btn = QPushButton('..')
        #  select_file_btn.clicked.connect(self.select_file)
        #  url_layout.addWidget(select_file_btn)

        url_layout.setContentsMargins(30, 10, 30, 10)

        self.cookie = Cookie()

        save_layout = QHBoxLayout()
        save_layout.addWidget(QLabel("Location"))

        self.save = QLineEdit()
        #  self.save.setReadOnly(True)

        save_layout.addWidget(self.save)

        select_dest_btn = QPushButton('..')
        select_dest_btn.clicked.connect(self.select_folder)
        save_layout.addWidget(select_dest_btn)

        do = QPushButton('Download')
        do.clicked.connect(self.download)

        submit_layout = QHBoxLayout()
        submit_layout.addStretch(1)
        submit_layout.addWidget(do)
        submit_layout.addStretch(1)

        self.job_list = QListWidget()
        self.job_sig.connect(self._add_job)

        #  progressbar_layout = QHBoxLayout()
        #  self.pbar = JobStatus(100)
        #  #  self.pbar.setVisible(False)
        #  progressbar_layout.addWidget(self.pbar)

        self.layout.addRow(url_layout)
        self.layout.addRow(self.cookie)
        self.layout.addRow(save_layout)
        self.layout.addRow(submit_layout)
        self.layout.addRow(self.job_list)


    def keyPressEvent(self, event):
        if event.key() == Qt.Key_Escape:
            if (focus := self.focusWidget()):
                focus.clearFocus()
            else:
                self.close()
        elif event.key() == Qt.Key_Q:
            self.close()

    def select_file(self):
        url, _filter = QFileDialog.getOpenFileUrl()
        self.url.setText(url.path())

    def select_folder(self):
        url = QFileDialog.getExistingDirectoryUrl()
        self.save.setText(url.path())

    def _add_job(self, status):
        item = QListWidgetItem()
        item.setSizeHint(status.sizeHint())
        self.job_list.addItem(item)
        self.job_list.setItemWidget(item, status)

    def add_job(self, status):
        self.job_sig.emit(status)

    def download(self):
        url = self.url.text()
        cookie = self.cookie.get_cookie()
        dest = self.save.text()

        if not url or not dest:
            self.alert('url or dest must not be empty')
            return

        task_id = md5(url.encode()).hexdigest()
        dest = os.path.join(dest, task_id)

        if os.path.exists(dest):
            self.alert('job already submitted?')
            return
        status = JobStatus(task_id, 1)
        Thread(target=self.real_download, args=(url, cookie, dest, status)).start()

    def real_download(self, url, cookie, dest, status):
        if not url.startswith('http://') and not url.startswith('https://'):
            url = 'http://' + url
        base = '/'.join(url.split('/')[:-1])

        header = {
            'User-Agent': USER_AGENT,
        }
        if cookie:
            header['Cookie'] = cookie
        try:
            with open('.header', 'r') as f:
                for line in f:
                    line = line.strip()
                    if line:
                        k, v = line.split(' ', 1)
                        header[k.strip()] = v.strip()
        except FileNotFoundError:
            print('.header missing')
        try:
            resp = requests.get(url, headers=header)
        except Exception as e:
            self.alert(f'm3u8 file request error: {repr(e)}')
            return
        if (code := resp.status_code) != 200:
            self.alert(f'm3u8 file request status code: {code}')
            return

        os.makedirs(dest)

        with open(os.path.join(dest, 'orig.m3u8'), 'wb') as f:
            f.write(resp.content)
        m3u8 = resp.text

        try:
            new_file, files, keyfile, total_time = parse(m3u8, base)
        except Exception as e:
            self.alert(f'parse failed: {repr(e)}')
            return
        if not files:
            self.alert('no file parsed, giving up')
            print(m3u8)
            return

        if keyfile:
            try:
                resp = requests.get(keyfile, headers=header, timeout=10)
            except Exception as e:
                self.alert(f'failed to get keyfile {keyfile}')
            with open(os.path.join(dest, 'keyfile'), 'wb') as f:
                f.write(resp.content)


        # start downloading, add new status
        status.set_total(len(files))
        status.update_phase('download')
        self.add_job(status)

        final_m3u8 = os.path.join(dest, 'translated.m3u8')
        with open(final_m3u8, 'w') as f:
            f.write('\n'.join(new_file))
        with open(os.path.join(dest, 'file.list'), 'w') as f:
            for file_url, _ in files:
                f.write(file_url + '\n')
        print(f'start dowloading, total time: {total_time}')

        all_finished = self.download_split_files(url, files, dest, header, status)
        if not all_finished:
            status.update_phase("partial")
            return
        status.update_phase('merge')
        print('merging')
        output = status.get_target()
        cmd = ['ffmpeg', '-allowed_extensions', 'ALL',
               '-i', final_m3u8, '-bsf:a', 'aac_adtstoasc',
               '-vcodec', 'copy', os.path.join(dest, output)]
        try:
            subprocess.run(cmd)
        except Exception as e:
            print('ffmpeg command failed', repr(e))
        print(subprocess.list2cmdline(cmd))
        print('all finished')
        status.update_phase('finished')

    def download_split_files(self, m3u8_url, files, dest, header, job_status):

        def worker(job, status):
            while True:
                try:
                    url, fn = job.get_nowait()
                except queue.Empty:
                    break
                status.put((DownloadStatus.INFO, (current_thread().name, "JOB", url,)))

                try:
                    resp = requests.get(url, timeout=10)
                except Exception as e:
                    status.put((DownloadStatus.NetworkError, (url, repr(e))))
                    job.put((url, fn))
                    continue
                else:
                    if resp.status_code == 404:
                        status.put((DownloadStatus.NotFoundError, (url,)))
                        continue

                cont = resp.content

                try:
                    with open(os.path.join(dest, fn), 'wb') as f:
                        f.write(cont)
                        status.put((DownloadStatus.OK, (url,)))
                except Exception as e:
                    status.put((DownloadStatus.WriteError, (url, repr(e))))


        total = len(files)
        job = queue.Queue()
        status = queue.Queue()
        for f in files:
            job.put(f)

        for _i in range(JOB):
            Thread(target=worker, args=(job, status)).start()

        finished = 0
        ok = 0

        with open(os.path.join(dest, 'download_status.log'), 'w') as f:
            f.write(m3u8_url + "\n")
            while finished < total:
                batch = []
                while True:
                    try:
                        item = status.get_nowait()
                        batch.append(item)
                    except queue.Empty:
                        break
                for stat, args in batch:
                    f.write(f"{stat.value} {' '.join(args)}\n")
                    if stat is DownloadStatus.OK:
                        finished += 1
                        ok += 1
                    elif stat in [DownloadStatus.NotFoundError,
                                  DownloadStatus.UnknownError,
                                  DownloadStatus.WriteError,]:
                        finished += 1
                f.write(f'STAT   {finished/total*100}% {ok}/{finished-ok}/{total}\n')
                job_status.update_stat(ok, finished, 'download')

                #  cur_threads = ' '.join([x.name for x in list(thread_enumerate())])
                #  f.write(f'THREAD {cur_threads}\n')
                f.flush()
                time.sleep(2.5)
            f.write('finished\n')
            return ok == total


def main():
    app = QApplication(sys.argv)
    window = MainWindow('M3U8 Downloader')
    window.show()
    app.exec()

if __name__ == "__main__":
    main()
