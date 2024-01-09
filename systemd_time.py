import ctypes
from ctypes import POINTER, pointer
from typing import Tuple, Optional


class CalendarComponent(ctypes.Structure):
    def __repr__(self):
        return f"{self.start}, {self.stop}, {self.repeat}"


CalendarComponent._fields_ = [
    ("start", ctypes.c_int),
    ("stop", ctypes.c_int),
    ("repeat", ctypes.c_int),
    ("next", ctypes.POINTER(CalendarComponent)),
]


class CalendarSpec(ctypes.Structure):
    def __del__(self):
        if ctypes.addressof(self):
            ret = calendar_spec_free(self)
            if ret != 0:
                print("free failed")


CalendarSpec._fields_ = [
    ("weekdays_bits", ctypes.c_int),
    # it seems we need to use the same type for those bit field for now
    # see https://stackoverflow.com/a/75796392/4533142 and
    # https://github.com/python/cpython/pull/97702
    ("end_of_month", ctypes.c_int, 1),
    ("utc", ctypes.c_int, 1),
    ("dst", ctypes.c_int, 2),
    ("timezone", ctypes.c_char_p),
    ("year", ctypes.POINTER(CalendarComponent)),
    ("month", ctypes.POINTER(CalendarComponent)),
    ("day", ctypes.POINTER(CalendarComponent)),
    ("hour", ctypes.POINTER(CalendarComponent)),
    ("minute", ctypes.POINTER(CalendarComponent)),
    ("microsecond", ctypes.POINTER(CalendarComponent)),
]

LIB = ctypes.CDLL("/usr/lib/systemd/libsystemd-shared-255.2-1.so")


def calendar_spec_from_string(p: str) -> Tuple[int, Optional[CalendarSpec]]:
    LIB.calendar_spec_from_string.argtypes = [
        ctypes.c_char_p,
        POINTER(POINTER(CalendarSpec)),
    ]
    LIB.calendar_spec_from_string.restype = ctypes.c_int

    arg1 = p.encode()
    arg2 = pointer(POINTER(CalendarSpec)())
    ret = LIB.calendar_spec_from_string(arg1, arg2)
    if ret == 0 and arg2.contents:
        spec_ptr = arg2.contents
        return (ret, spec_ptr.contents)
    return (ret, None)


def calendar_spec_to_string(spec: CalendarSpec) -> Tuple[int, str]:
    LIB.calendar_spec_to_string.argtypes = [
        POINTER(CalendarSpec),
        ctypes.POINTER(ctypes.c_char_p),
    ]
    LIB.calendar_spec_to_string.restype = ctypes.c_int

    str_ret = ctypes.c_char_p()
    arg1 = pointer(spec)
    arg2 = ctypes.pointer(str_ret)
    ret = LIB.calendar_spec_to_string(arg1, arg2)

    if ret == 0 and str_ret.value:
        return (ret, str_ret.value.decode())
    return ret, ""


def calendar_spec_free(spec: CalendarSpec) -> int:
    LIB.calendar_spec_free.argtypes = [
        POINTER(CalendarSpec),
    ]
    LIB.calendar_spec_free.restype = ctypes.c_int
    arg1 = pointer(spec)
    ret = LIB.calendar_spec_free(arg1)
    return ret


def calendar_spec_valid(spec: CalendarSpec) -> bool:
    LIB.calendar_spec_valid.argtypes = [
        POINTER(CalendarSpec),
    ]
    LIB.calendar_spec_valid.restype = ctypes.c_bool
    arg1 = pointer(spec)
    ret = LIB.calendar_spec_valid(arg1)
    return ret


def calendar_spec_next_usec(spec: CalendarSpec, usec: int) -> Tuple[int, int]:
    LIB.calendar_spec_next_usec.argtypes = [
        POINTER(CalendarSpec),
        ctypes.c_uint64,
        POINTER(ctypes.c_uint64),
    ]
    LIB.calendar_spec_next_usec.restype = ctypes.c_int

    arg1 = pointer(spec)
    arg2 = usec
    n = ctypes.c_uint64()
    arg3 = pointer(n)
    ret = LIB.calendar_spec_next_usec(arg1, arg2, arg3)

    if ret == 0 and arg3:
        return (ret, arg3.contents.value)
    return ret, 0

# on error return negative int
def parse_timestamp(s: str) -> int:
    LIB.parse_timestamp.argtypes = [
        ctypes.c_char_p,
        POINTER(ctypes.c_uint64),
    ]
    LIB.parse_timestamp.restype = ctypes.c_int
    arg1 = s.encode()
    arg2 = pointer(ctypes.c_uint64())
    ret = LIB.parse_timestamp(arg1, arg2)
    if ret == 0:
        return arg2.contents.value
    return ret

USEC_PER_SEC = 1_000_000

def parse_time(s: str) -> int:
    LIB.parse_time.argtypes = [
        ctypes.c_char_p,
        POINTER(ctypes.c_uint64),
        ctypes.c_uint64,
    ]
    LIB.parse_time.restype = ctypes.c_int
    arg1 = s.encode()
    arg2 = pointer(ctypes.c_uint64())
    ret = LIB.parse_time(arg1, arg2, USEC_PER_SEC)
    if ret == 0:
        return arg2.contents.value
    return ret



def print_component(name: str, ptr, layer: int):
    if name:
        print("%s" % name)
    if ptr:
        comp = ptr.contents
        print("%s %d %d %d" % (layer * "\t", comp.start, comp.stop, comp.repeat))
        if comp.next:
            print_component("", comp.next, layer + 1)


def main():
    print(parse_time("2h"))
    # ret, spec = calendar_spec_from_string(sys.argv[1])
    # pass
    # if ret != 0:
    #     exit(1)
    # print(spec)
    # if spec is None:
    #     return
    # print(calendar_spec_valid(spec))

    # ret, next = calendar_spec_next_usec(spec, int(time.time() * 1000000))
    # print(ret, next)
    # print(time.ctime(next / 10**6))
    # ret, r = calendar_spec_to_string(spec)
    # if ret != 0:
    #     exit(1)
    # print(r)

    # print("weekdays_bits: %d" % spec.weekdays_bits)
    # print("end_of_month: %d" % spec.end_of_month)
    # print("utc: %d" % spec.utc)
    # print("dst: %d" % spec.dst)
    # if spec.timezone:
    #     print("timezone: %s" % spec.timezone)
    # print_component("year", spec.year, 0)
    # print_component("month", spec.month, 0)
    # print_component("day", spec.day, 0)
    # print_component("hour", spec.hour, 0)
    # print_component("minute", spec.minute, 0)
    # print_component("microsecond", spec.microsecond, 0)

    # print(hex(ctypes.addressof(spec)))
    # import IPython
    # IPython.embed()
    # ret, rep = calendar_spec_to_string(spec)
    # print(ret)
    # print(rep)
    # import IPython
    # IPython.embed()


if __name__ == "__main__":
    main()
