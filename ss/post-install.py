import os
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("-d", "--domain", required=True, help="domain name")

renew_template = """
#!/usr/bin/bash

LAST_CERT="/home/{user}/.cache/cert_last_renew"
LOG_FILE="/home/{user}/cert_renew.log"

[[ -f $LAST_CERT ]] && \\
          [[ $(($(date '+%s') - $(cat $LAST_CERT))) -le 5270400 ]] && exit # 61 day

echo "$(date)" >> $LOG_FILE
echo "{domain}: Let's Encrypt certificate renew starting..." >> $LOG_FILE
systemctl stop ssserver
export TENCENTCLOUD_SECRET_ID={secret_id} TENCENTCLOUD_SECRET_KEY={secret_key}
certbot certonly -a dns-tencentcloud -d {domain} >> $LOG_FILE
echo "{domain}: Let's Encrypt certificate renew done, rebooting..." >> $LOG_FILE
echo $(date '+%s') > $LAST_CERT
reboot
"""

renew_service_template = """
[Unit]
Description=Let's encrypt certificate auto renew

[Service]
ExecStart=/home/{user}/bin/renew.sh
Type=simple
"""

renew_timer_template = """
[Unit]
Description=timer for cert-renew.service

[Timer]
OnCalendar=*-*-* 20:00:00
AccuracySec=1min
Persistent=true

[Install]
WantedBy=multi-user.target
"""

user = os.getlogin()
secret_id = os.environ.get("TENCENTCLOUD_SECRET_ID")
secret_key = os.environ.get("TENCENTCLOUD_SECRET_KEY")
if secret_id is None or secret_key is None:
    print("TENCENTCLOUD_SECRET_ID && TENCENTCLOUD_SECRET_KEY")
    exit(1)


def main():
    args = parser.parse_args()
    bin_path = os.path.join("/home", user, "bin")
    os.makedirs(bin_path, exist_ok=True)
    renew_path = os.path.join(bin_path, "renew.sh")
    with open(renew_path, "w") as f:
        f.write(
            renew_template.format(
                user=user,
                secret_id=secret_id,
                secret_key=secret_key,
                domain=args.domain,
            )
        )
    os.chmod(renew_path, 775)

    with open("/etc/systemd/system/cert-renew.service", 'w') as f:
        f.write(renew_service_template.format(user=user))
    with open("/etc/systemd/system/cert-renew.timer", 'w') as f:
        f.write(renew_timer_template)

    os.system("systemctl enable ssserver")
    os.system("systemctl enable cert-renew.timer")
    os.system("systemctl start ssserver")
    os.system("systemctl start cert-renew.timer")


if __name__ == "__main__":
    main()
