import os
import sys
import time
import json
import requests
from multiprocessing import Process, Lock
from typing import Dict
from fastapi import FastAPI
from pydantic import BaseModel, ValidationError

SEND_KEY = os.getenv("SEND_KEY", None)
if not SEND_KEY:
    print("SEND_KEY")
    sys.exit(1)


class EventDataCommon(BaseModel):
    RoomId: int
    ShortId: int
    Name: str
    Title: str
    AreaNameParent: str
    AreaNameChild: str
    #Recording: bool
    #Streaming: bool
    #DanmakuConnected: bool

class EventDataSessionStarted(EventDataCommon):
    SessionId: str

class EventDataFileOpening(EventDataCommon):
    RelativePath: str
    FileOpenTime: str
    SessionId: str

class EventDataFileClosed(EventDataCommon):
    RelativePath: str
    FileSize: int
    Duration: float
    FileOpenTime: str
    FileCloseTime: str
    SessionId: str

class EventDataSessionEnded(EventDataCommon):
    SessionId: str

# class EventDataStreamStarted(EventDataCommon):
#     pass
# 
# class EventDataStreamEnded(EventDataCommon):
#     pass

class Event(BaseModel):
    EventId: str
    EventType: str
    EventTimestamp: str
    EventData: Dict


app = FastAPI()


def send_msg(title: str, desp: str):
    url = f"https://sctapi.ftqq.com/{SEND_KEY}.send"
    r = requests.post(
        url,
        data={
            "title": title,
            "desp": desp,
        },
    )
    print(r.text)

def handle_session_started(evt: EventDataSessionStarted):
    title = f"录播状态-SessionStarted"
    desp = "\n".join(
        [
            f"Title: {evt.Title}",
            f"Name: {evt.Name}",
            f"EventType: SessionStarted",
            f"Area: {evt.AreaNameParent} / {evt.AreaNameChild}",
            f"SessionId: {evt.SessionId}",
        ]
    )
    send_msg(title, desp)
    return ""

def handle_session_ended(evt: EventDataSessionEnded):
    title = f"录播状态-SessionEnded"
    desp = "\n".join(
        [
            f"Title: {evt.Title}",
            f"Name: {evt.Name}",
            f"EventType: SessionEnded",
            f"Area: {evt.AreaNameParent} / {evt.AreaNameChild}",
            f"SessionId: {evt.SessionId}",
        ]
    )
    send_msg(title, desp)
    Process(target=upload_judge, args=(evt.RoomId,)).start()

    return ""

file_state_lock = Lock()

file_state = {}

def upload_judge(roomid: int):
    time.sleep(10)
    if roomid not in file_state:
        send_msg('Upload Judge Error', f'can not find {roomid} in file_state')

    with file_state_lock:
        total = 0
        filelist = []
        for (file, duration) in file_state[roomid]:
            filelist.append(file)
            total += duration
        if total > 600: # 10 min
            # prepare to upload
            send_msg('Upload Judge Decision', f'total: {total}, ready to merge and send {filelist}')
            file_state.pop(roomid)
        else:
            send_msg('Upload Judge Decision', f'less than 10m {filelist}')

def handle_file_opening(evt: EventDataFileOpening):
    title = f"录播状态-FileOpening"
    desp = "\n".join(
        [
            f"Title: {evt.Title}",
            f"Name: {evt.Name}",
            f"EventType: FileOpening",
            f"OpenTime: {evt.FileOpenTime}",
            f"Path: {evt.RelativePath}",
            f"Area: {evt.AreaNameParent} / {evt.AreaNameChild}",
            f"SessionId: {evt.SessionId}",
        ]
    )
    send_msg(title, desp)
    return ""

def handle_file_closed(evt: EventDataFileClosed):
    room_id = evt.RoomId
    with file_state_lock:
        if room_id in file_state:
            file_state[room_id].append((evt.RelativePath, evt.Duration))
        else:
            file_state[room_id] = [(evt.RelativePath, evt.Duration)]

    title = f"录播状态-FileClosed"
    desp = "\n".join(
        [
            f"Title: {evt.Title}",
            f"Name: {evt.Name}",
            f"EventType: FileClosed",
            f"OpenTime: {evt.FileOpenTime}",
            f"CloseTime: {evt.FileCloseTime}",
            f"Duration: {evt.Duration}",
            f"Path: {evt.RelativePath}",
            f"Area: {evt.AreaNameParent} / {evt.AreaNameChild}",
            f"SessionId: {evt.SessionId}",
        ]
    )
    send_msg(title, desp)
    return ""

@app.post("/bililive/event")
def event(evt: Event):
    print(evt)
    match evt.EventType:
        case 'SessionStarted':
            try:
                handle_session_started(EventDataSessionStarted(**evt.EventData))
            except ValidationError:
                send_msg('Bililive Webhook EventData Error', json.dumps(evt.EventData))
        case 'SessionEnded':
            try:
                handle_session_ended(EventDataSessionEnded(**evt.EventData))
            except ValidationError:
                send_msg('Bililive Webhook EventData Error', json.dumps(evt.EventData))
        case 'FileOpening':
            try:
                handle_file_opening(EventDataFileOpening(**evt.EventData))
            except ValidationError:
                send_msg('Bililive Webhook EventData Error', json.dumps(evt.EventData))
        case 'FileClosed':
            try:
                handle_file_closed(EventDataFileClosed(**evt.EventData))
            except ValidationError:
                send_msg('Bililive Webhook EventData Error', json.dumps(evt.EventData))
        case ty:
            print(f'unknown event type: {ty}: {evt.EventData}')

