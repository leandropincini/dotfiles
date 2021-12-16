# Arch Linux Setup

## Installation
### Download
Download the most recent ISO from archlinux official [dowload page](https://archlinux.org/download/).

```bash
dd if=~/Downloads/my-new-arch-iso.iso of=/media/my-usb
```

reboot.

### Internet
First, call the `iwctl` command:

```bash
iwctl
```
Then you can search for your device with:

```bash
device list
```

P.e `wlan 0` will be returned.
After this we can search for available networks with:

```bash
station wlan0 scan
station wlan0 get-networks
```

Since the network was discovered by the iwctl we can finally connect to the network:

```bash
station wlan0 connect "my-wireless-network-name"
```

We will be ask for the passphrase, input it and then call `Ctrl-D` for exit the iwtcl prompt.
Maybe it take some seconds to connect, after it we can call:

```bash
ip addr show
```

and then we could find our inet address.
We can also call

```bash
ping www.google.com
```

for verify that we already have an internet connection configurated.

### Partitioning

## Firewall
We can follow [this](https://wiki.archlinux.org/title/simple_stateful_firewall#Firewall_for_a_single_machine) article from arch's wiki.

<h3 id=single-machine>Single Machine</h3>
TLDR:

Clean everything:

```bash
iptables-restore < /etc/iptables/empty.rules
```

Create necessary chains

```bash
iptables -N TCP
iptables -N UDP
```

Drop the FORWARD chain:

```bash
iptables -P FORWARD DROP
```

Accept the OUTPUT chain:

```bash
iptables -P OUTPUT ACCEPT
```

Drop the INPUT chain:

```bash
iptables -P INPUT DROP
```

Accept RELATED and ESTABLISHED traffic:

```bash
iptables -A INPUT -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT
```

Accept all "loopback" traffic:

```bash
iptables -A INPUT -i lo -j ACCEPT
```

Accept ICMPv6 neighbor discovery packets:

```bash
iptables -A INPUT -p 41 -j ACCEPT
```

Drop all packets with invalid headers or checksums, invalid TCP flags, invalid ICMP messages, and out of sequence packets which can be caused by sequence prediction or other similiar attacks:

```bash
iptables -A INPUT -m conntrack --ctstate INVALID -j DROP
```

Accept all new incoming ICMP echo requests (pings). Only the first packet will count as NEW, the others will be handled by the REALTED,ESTABLISHED rule.

```bash
iptables -A INPUT -p icmp --icmp-type 8 -m conntrack --ctstate NEW -j ACCEPT
```

Prepare SSH bruteforce mitigation:

```bash
iptables -N IN_SSH
iptables -A INPUT -p tcp --dport ssh -m conntrack --ctstate NEW -j IN_SSH
```

Attach the TCP and UDP chains to the INPUT chain to handle all new incoming connects:

```bash
iptables -A INPUT -p udp -m conntrack --ctstate NEW -j UDP
iptables -A INPUT -p tcp --syn -m conntrack --ctstate NEW -j TCP
```

Accept SSH connects (adjust port as required):

```bash
iptables -A TCP -p tcp --dport 22 -j ACCEPT
```

Add TCP SYN scans to denylist for 60 seconds:

```bash
iptables -I TCP -p tcp -m recent --update --rsource --seconds 60 --name TCP-PORTSCAN -j REJECT --reject-with tcp-reset
iptables -A INPUT -p tcp -m recent --set --rsource --name TCP-PORTSCAN -j REJECT --reject-with tcp-reset
```

Add UDP SYN scans to denylist for 60 seconds:

```bash
iptables -I UDP -p udp -m recent --update --rsource --seconds 60 --name UDP-PORTSCAN -j REJECT --reject-with icmp-port-unreachable
iptables -A INPUT -p udp -m recent --set --rsource --name UDP-PORTSCAN -j REJECT --reject-with icmp-port-unreachable
```

Reject all remaining incoming traffic with icmp protocol unreachable messges:

```bash
iptables -A INPUT -j REJECT --reject-with icmp-proto-unreachable
```

Mitigate SSH bruteforce:

```bash
iptables -N LOG_AND_DROP
iptables -A IN_SSH -m recent --name sshbf --rttl --rcheck --hitcount 3 --seconds 10 -j LOG_AND_DROP
iptables -A IN_SSH -m recent --name sshbf --rttl --rcheck --hitcount 4 --secounds 1800 -j LOG_AND_DROP
iptables -A IN_SSH -m recent --name sshbf --set -j ACCEPT
iptables -A LOG_AND_DROP -j LOG --log-prefix "iptables deny: " --log-level 7
iptables -A LOG_AND_DROP -j DROP
```

Before save all the configurations, let's backup the old iptables rule's file:

```bash
cp /etc/iptables/iptables.rules /etc/iptables/iptables.rules.bak
```

Save the iptables configurations into the default's file:

```bash
iptables-save -f /etc/iptables/iptables.rules
```

### Server machine
Before start this section, please, follow the [### Single Machine](#single-machine) section.

Accept incoming TCP connections on port 80 (HTTP):

```bash
iptables -A TCP -p tcp --dport 80 -j ACCEPT
```

Accept incoming TCP connections on port 443 (HTTPS):

```bash
iptables -A TCP -p tcp --dport 443 -j ACCEPT
```

Accept incoming TCP/UDP requests on port 53 (DNS):

```bash
iptables -A TCP -p tcp --dport 53 -j ACCEPT
iptables -A UDP -p udp --dport 53 -j ACCEPT
```

### Protection against spoofing attacks

Since the `rp_filter` is currently set to `2` by default in `/usr/lib/sysctl.d/50-default.conf` there is no action needed.
