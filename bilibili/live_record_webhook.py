import os
import sys
import requests
from fastapi import FastAPI
from pydantic import BaseModel

SEND_KEY = os.getenv("SEND_KEY", None)
if not SEND_KEY:
    print("SEND_KEY")
    sys.exit(1)


class EventData(BaseModel):
    RoomId: int
    Name: str
    Title: str
    AreaNameParent: str
    AreaNameChild: str


class Event(BaseModel):
    EventType: str
    EventTimestamp: str
    EventData: EventData


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


@app.post("/bililive/event")
def event(evt: Event):
    print(evt)
    title = f"录播状态-{evt.EventData.Name}"
    desp = "\n".join(
        [
            f"Title: {evt.EventData.Title}",
            f"EventType: {evt.EventType}",
            f"Timestamp: {evt.EventTimestamp}",
            f"Area: {evt.EventData.AreaNameParent} / {evt.EventData.AreaNameChild}",
        ]
    )
    send_msg(title, desp)
    return ""
