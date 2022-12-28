# install a shadowsocks server with v2ray-plugin as plugin
# root is required
import os
import stat
import time
import tarfile
import tempfile
import random
import string
from urllib.request import urlopen
from shutil import rmtree
import argparse

TEMP_DIR = "/tmp/"
SS_URL = "https://github.com/shadowsocks/shadowsocks-rust/releases/download/v1.15.2/shadowsocks-v1.15.2.x86_64-unknown-linux-gnu.tar.xz"
V2RAY_URL = "https://github.com/teddysun/v2ray-plugin/releases/download/v5.1.0/v2ray-plugin-linux-amd64-v5.1.0.tar.gz"

PERM_755 = stat.S_IRWXU | stat.S_IRGRP | stat.S_IXGRP | stat.S_IROTH | stat.S_IXOTH

ss_template = """
{{
  "server": "0.0.0.0",
  "server_port": 443,
  "local_port":1080,
  "password":"{password}",
  "timeout":600,
  "method":"chacha20-ietf-poly1305",
  "fast_open": true,
  "mode": "tcp_only",
  "plugin": "v2ray-plugin",
  "plugin_opts": "server;tls;host={domain};cert={cert_path};key={key_path}"
}}
"""

systemd_template = """
[Unit]
Description=ssserver
After=network.target

[Service]
Type=simple
ExecStart={bin_dir}/ssserver -c /etc/shadowsocks/ss.json

[Install]
WantedBy=multi-user.target
"""

parser = argparse.ArgumentParser()
parser.add_argument("-d", "--domain", required=True, help="domain name")
parser.add_argument("-p", "--python", default="python3", help="python binary")
parser.add_argument(
    "--bin-dir", default="/usr/local/bin", help="bin dir to store binaries"
)
parser.add_argument(
    "--cert", default=None, help="cert path, default to use Let's Encrypt dir"
)
parser.add_argument(
    "--key", default=None, help="cert key path, default to use Let's Encrypt dir"
)


# 50% symbol, 50% alphanumuric
def mk_random_password(size: int) -> str:
    s = ["" for _ in range(size)]
    symbol_set = """!@#$%^&*()-_+={}[];:"""
    for i in range(size):
        if random.random() < 0.5:
            s[i] = random.choice(symbol_set)
        elif random.random() < 0.7:
            s[i] = random.choice(string.digits)
        else:
            s[i] = random.choice(string.ascii_letters)
    return "".join(s)


def prepare_shadowsocks(args):
    xz = "shadowsocks.tar.xz"
    with open(xz, "wb") as f:
        resp = urlopen(SS_URL)
        f.write(resp.read())
    tar = tarfile.open(xz, "r:xz")
    ssserver = tar.extractfile("ssserver")
    if ssserver is None:
        print('failed to extract "ssserver"')
        exit(1)
    ssserver_bin = os.path.join(args.bin_dir, "ssserver")
    with open(ssserver_bin, "wb") as f:
        f.write(ssserver.read())
        os.chmod(ssserver_bin, PERM_755)

    os.makedirs("/etc/shadowsocks", exist_ok=True)
    password = mk_random_password(32)
    print(f"password: \x1b[31;1m{password}\x1b[0m")
    if args.cert is None:
        cert_path = f"/etc/letsencrypt/live/{args.domain}/fullchain.pem"
    else:
        cert_path = args.cert
    if args.key is None:
        key_path = f"/etc/letsencrypt/live/{args.domain}/privkey.pem"
    else:
        key_path = args.key
    with open("/etc/shadowsocks/ss.json", "w") as f:
        f.write(
            ss_template.format(
                password=password,
                domain=args.domain,
                cert_path=cert_path,
                key_path=key_path,
            )
        )

    with open("/etc/systemd/system/ssserver.service", "w") as f:
        f.write(systemd_template.format(bin_dir=os.path.abspath(args.bin_dir)))


def prepare_v2ray_plugin(args):
    gz = "v2ray-plugin.tar.gz"
    with open(gz, "wb") as f:
        resp = urlopen(V2RAY_URL)
        f.write(resp.read())
    tar = tarfile.open(gz, "r:gz")
    name = tar.members[0].name  # type: ignore
    assert name.startswith("v2ray-plugin")
    v2ray_plugin = tar.extractfile(name)
    if v2ray_plugin is None:
        print(f'failed to extract "{name}"')
        exit(1)
    v2ray_plugin_bin = os.path.join(args.bin_dir, "v2ray-plugin")
    with open(v2ray_plugin_bin, "wb") as f:
        f.write(v2ray_plugin.read())
        os.chmod(v2ray_plugin_bin, PERM_755)


def main():
    if os.geteuid() != 0:
        print("need root permission")
        exit(1)

    args = parser.parse_args()
    print(args)
    time.sleep(3)

    d = tempfile.mkdtemp(prefix=TEMP_DIR)
    print(f"created temp dir {d}")
    os.chdir(d)

    print("downloading shadowsocks-rust...")
    prepare_shadowsocks(args)
    print("downloading v2ray-plugin...")
    prepare_v2ray_plugin(args)

    rmtree(d)


if __name__ == "__main__":
    main()
