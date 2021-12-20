# Arch Linux Setup

## Download
Download the most recent ISO from archlinux official [dowload page](https://archlinux.org/download/).

```bash
dd if=~/Downloads/my-new-arch-iso.iso of=/media/my-usb
```

reboot.

## Internet
First, call the `iwctl` command:

```bash
iwctl
```
Then you can search for your device with:

```
[iwd]# device list
```

P.e `wlan0` will be returned.
After this we can search for available networks with:

```
[iwd]# station wlan0 scan
[iwd]# station wlan0 get-networks
```

Since the network was discovered by the iwctl we can finally connect to the network:

```
[iwd]# station wlan0 connect "my-wireless-network-name"
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

## Partition the disks

First of all, we need to find out what the device name is for the hard disk that we intend to be working with.

```bash
fdisk -l
```

In my case `/dev/sda`, then:

```bash
fdisk /dev/sda
```

Create the new partition table:

```
Command (m for help): g
```

Create the first UEFI new partition:

```
Command (m for help): n
Partition number (1-128, default 1):
First sector (2048-1048575966, default 2048):
Last sector, +/-sectors or +/-size{K,M,G,T,P} (2048-1048575966, default 1048575966): +500M
```

Setting the type for the new partition:

```
Command (m for help): t
Selected partition 1
Partition type or alias (type L to list all): 1
```

Lets create the second partition, for boot:

```
Command (m for help): n
Partition number (2-128, default 2):
First sector (1026048-1048575966, default 1026048):
Last sector, +/-sectors or +/-size{K,M,G,T,P} (1026048-1048575966, default 1048575966): +500M
```

Create the third partition for the LVM:

```
Command (m for help): n
Partition number (3-128, default 3):
First sector (2040048-104875966, default 2050058):
Last sector, +/-secotrs or +/-size{K,M,G,T,P} (2050048-1048575966, default 1048575966):
```

Setting the type for the third partition:

```
Command (m for help): t
Partition number (1-3, default 3):
Partition type or alias (type L to list all): 30
```

Now, lets verify the partitions created:

```
Command (m for help): p
```

| Name      | Size | Type             |
| --------- |:----:|:----------------:|
| /dev/sda1 | 500M | EFI System       |
| /dev/sda2 | 500M | Linux filesystem |
| /dev/sda3 | 499G | Linux LVM        |

Last but not least, we need to write the partition table into the disk:

```
Command (m for help): w
```

Now if we run the `fdisk -l` again we should have the same output that we had at the `p` command in the fdisk prompt.

## Formatting

Setup a FAT32 partition for the UEFI partition:

```bsah
mkfs.fat -F32 /dev/sda1
```

Setup a EXT4 partition for the /boot partition:

```bash
mkfs.ext4 /dev/sda2
```

### Encpypt the LVM partition

Since we will use the third partition for the LVM, we need to setup it before formating it... just encrypt and setup the LVM. Then:

```
cryptsetup luksFormat /dev/sda3

WARNING!
========
This will overwrite data on /dev/sda3 irrevocably.

Are you sure? (Type 'yes' in capital letters): YES
Enter passphrase for /dev/sda3: <choose your passphrase that you will remember>
Verify passphrase: <type the passphrase again>
```

Since the partition was encrypted we need to unlock it:

```
cryptsetup open --type luks /dev/sda3 lvm
Enter the passphrase for /dev/sda3: <see... you'll need to remember it>
```

### Setup the LVM

Create the physical volume:

```bash
pvcreate --dataalignment 1m /dev/mapper/lvm
```

Next, lets create the volume group:

```bash
vgcreate volgroup0 /dev/mapper/lvm
```

Now, lets create two logical volumes, one for the root filesystem and other for our home partition:

```bash
lvcreate -L 30GB volgroup0 -n lv_root
lvcreate -l 100%FREE volgroup0 -n lv_home
```

### Activate the LVM

First we need to enable the kernel module:

```bash
modprobe dm_mod
```

Now, we can scan for the volume groups:

```bash
vgscan
```

And finally to active the volume group:

```bash
vgchange -ay
```

Now, finally we can format our new LVM volumes, first we will format the root LVM volume:

```bash
mkfs.ext4 /dev/volgroup0/lv_root
mount /dev/volgroup0/lv_root /mnt
```

Lets create then, a directory for our boot partition:

```bash
mkdir /mnt/boot
```

Let's now mount our second partition (boot) into the new folder that we created:

```bash
mount /dev/sda2 /mnt/boot
```

And then, we can format our home LVM volume:

```bash
mkfs.ext /dev/volgroup0/lv_home
mkdir /mnt/home
mount /dev/volgroup0/lv_home /mnt/home
```

### Creating the fstab file

First we need to create our /etc directory:

```bash
mkdir /mnt/etc
```

Then we can use the genfstab for create our file:

```bash
genfstab -U -p /mnt >> /mnt/etc/fstab
```

The fstab file should be like:

```
# /dev/mapper/volgropup0-lv_root
UUID=1140bb76-da34-4868-aec7-6cd0a17b1b57	/	ext4	rw,relatime	0 1

#/dev/sda2
UUID=1545cc07-cfb2-4008-846b-cd41f75d4319	/boot	ext4	rw,relatime	0 2

#/dev/mapper/volgroup0-lv_home
UUID=1b663a7a-d5a3-493d-b807-a22cd705fe4e	/home	ext4	rw,relatime	0 2
```

## Installing

After setup our disks, we need to install the base package group:

```bash
pacstrap -i /mnt base
```

Next, lets start to configure our installation:

### Kernel and tools

```bash
arch-chroot /mnt
pacman -Sy linux-lts linux-lts-headers base-devel zsh vim
```

### Network
```bash
pacman -Sy networkmanager wpa_suplicant wireless_tools netctl dialog
systemctl enable NetworkManager
```

To connect to network use `nmtui-connect`.

### Enable LVM and crypto support
```bash
pacman -Sy lvm2
```

Then edit the `/etc/mkinitcpio.conf` file. We need to add `encrypt` and `lvm2` at the first `HOOK` session between `block` and `filesystems`:

```
HOOKS=(base udev autodetect modconf block encrypt lvm2 filesystems keyboard fsck)
```

After it we need to recompile the kernel with:

```bash
mkintcpio -p linux-lts
```

### Locale
Edit the `/etc/locale.gen` file and remove:

```
en_US.UTF-8 UTF-8
pt_BR.UTF-8 UTF-8
```

Then call the `locale-gen` command:

```bash
locale-gen
```

and finally add the following at the `/etc/locale.conf` file:

```
LANG=en_US.UTF-8
```

### Users
First, setup the root password, just call:

```bash
passwd
```

Then create a regular user:

```bash
useradd -m -g users -G wheel,storage,power,audio,video -s /usr/bin/zsh leandro
passwd leandro
```

Then lets point the sudo command to wheel group:

```bash
EDITOR=vim visudo
```

Then uncomment the line:

```
%whell ALL=(ALL) ALL
```

### Bootloader - GRUB

Lets install the needed packages:

```bash
pacman -Sy grub efibootmgr dosfstools os-prober mtools
```

Setup our EFI partition and install grub:

```bash
mkdir /boot/EFI
mount /dev/sda1 /boot/EFI
grub-install --target=x86_64-efi --bootloader-id=grub_uefi --recheck
cp /usr/share/locale/en\@quot/LC_MESSAGES/grub.mo /boot/grub/locale/en.mo
```

Then we need to edit the `/etc/default/grub` file and:

- uncomment the `GRUB_ENABLE_CRYPTODISK=y` line.
- add `cryptdevice=/dev/sda3:volgroup0:allow-discards` option to the `GRUB-CMDLINE_LINUX_DEFAULT` line.

```
GRUB_CMDLINE_LINUX_DEFAULT="cryptdevice=/dev/sda3:volgroup0:allow-discards loglevel=3 quiet"
```

Then finally, generate the grub configuration file:

```bash
grub-mkconfig -o /boot/grub/grub.cfg
```

### Finishing and rebooting

```bash
exit
umount -a
reboot
```

## SSH
```bash
pacman -Sy openssh
```

Edit the `/etc/ssh/sshd_config` file and uncomment the following lines:

```
Port 22 (change to another port)
LoginGraceTime 1m
PermitRootLogin no
StrictModes yes
MaxAuthTries 2
MaxStartups 5:80:10
PermitTunnel no
```

Edit the `/etc/hosts.deny` file and comment the following line:

```
ALL:ALL:DENY
```

```bash
systemctl enable sshd
```

## Firewall
We can follow [this](https://wiki.archlinux.org/title/simple_stateful_firewall#Firewall_for_a_single_machine) article from arch's wiki.

<h3 id=single-machine>Single Machine</h3>
TLDR:

Clean everything:

```bash
iptables-restore < /etc/iptables/empty.rules
```

Create necessary chains:

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

Accept all new incoming ICMP echo requests (pings). Only the first packet will count as NEW, the others will be handled by the RELATED,ESTABLISHED rule.

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

Add TCP SYN scans to denylist for 1800 seconds:

```bash
iptables -I TCP -p tcp -m recent --update --rsource --seconds 1800 --name TCP-PORTSCAN -j REJECT --reject-with tcp-reset
iptables -A INPUT -p tcp -m recent --set --rsource --name TCP-PORTSCAN -j REJECT --reject-with tcp-reset
```

Add UDP SYN scans to denylist for 1800 seconds:

```bash
iptables -I UDP -p udp -m recent --update --rsource --seconds 1800 --name UDP-PORTSCAN -j REJECT --reject-with icmp-port-unreachable
iptables -A INPUT -p udp -m recent --set --rsource --name UDP-PORTSCAN -j REJECT --reject-with icmp-port-unreachable
```

Reject all remaining incoming traffic with icmp protocol unreachable messges:

```bash
iptables -A INPUT -j REJECT --reject-with icmp-proto-unreachable
```

Mitigate SSH bruteforce:

```bash
iptables -N LOG_AND_DROP
iptables -A IN_SSH -m recent --name sshbf --rttl --rcheck --hitcount 3 --seconds 60 -j LOG_AND_DROP
iptables -A IN_SSH -m recent --name sshbf --rttl --rcheck --hitcount 4 --secounds 1800 -j LOG_AND_DROP
iptables -A IN_SSH -m recent --name sshbf --set -j ACCEPT
iptables -A LOG_AND_DROP -j LOG --log-prefix "iptables deny: " --log-level 7
iptables -A LOG_AND_DROP -j DROP
```

Before save all the configurations, lets backup the old iptables rule's file:

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

### IPv6

TLDR:

Clean everything:

```bash
ip6tables-restore < /etc/iptables/empty.rules
```

Create necessary chains:

```bash
ip6tables -N TCP
ip6tables -N UDP
```

Drop the FORWARD chain:

```bash
ip6tables -P FORWARD DROP
```

Accept the OUTPUT chain:

```bash
ip6tables -P OUTPUT ACCEPT
```

Drop the INPUT chain:

```bash
ip6tables -P INPUT DROP
```

Accept all "loopback" traffic:

```bash
ip6tables -A INPUT -i lo -j ACCEPT
```

Accept ICMPv6 neighbor discovery packets:

```bash
ip6tables -A INPUT -p 41 -j ACCEPT
```

Drop all packets with invalid headers or checksums, invalid TCP flags, invalid ICMP messges, and out of sequence packets which can be caused by sequence prediction or other similar attacks:

```bash
ip6tables -A INPUT -m conntrack --ctstate INVALID -j DROP
```

Accept ICMPv6 traffic regardless of state for all directly attached subnets:

```bash
ip6tables -A INPUT -s fe80::/10 -p ipv6-icmp -j ACCEPT
```

Enable DHCPv6 accepting incoming connections on UDP port 546:

```bash
ip6tables -A INPUT -p udp --sport 547 --dport 546 -j ACCEPT
```

Accept all new incoming ICMP echo requests (pings). Oly the first packat will count as NEW, the others will be handled by the RELATED,ESTABLISHED rule.

```bash
ip6tables -A INPUT -p ipv6-icmp --icmpv6-type 128 -m conntrack --ctstate NEW -j ACCEPT
```

Prepare SSH bruteforce mitigation:

```bash
ip6tables -N IN_SSH
ip6tables -A INPUT -p tcp --dport ssh -m conntrack --ctstate NEW -j IN_SSH
```

Attach the TCP and UDP chains to the INPUT chain to handle all new incoming connects:

```bash
ip6tables -A INPUT -p udp -m conntrack --ctstate NEW -j UDP
ip6tables -A INPUT -p tcp --syn -m conntrack --ctstate NEW -j TCP
```

Accept SSH connects (adjust port as required):

```bash
ip6tables -A TCP -p tcp --dport 22 -j ACCEPT
```

Add TCP SYN scans to denylist for 1800 seconds:

```bash
ip6tables -I TCP -p tcp -m recent --update --rsource --seconds 1800 --name TCP-PORTSCAN -j REJECT --reject-with tcp-reset
ip6tables -A INPUT -p tcp -m recent --set --rsource --name TCP-PORTSCAN -j REJECT --reject-with tcp-reset
```

ADD UDP SYN scans to denylist for 1800 seconds:

```bash
ip6tables -I UDP -p udp -m recent --update --rsource --seconds 60 --name UDP-PORTSCAN -j REJECT
ip6tables -A INPUT -p udp -m recent --set --rsource --name UDP-PORTSCAN -j REJECT
```

Reject all remaining incoming traffic with icmp protocol unreachable messages:

```bash
ip6tables -A INPUT -j REJECT
```

Mitigate SSH bruteforce:

```bash
ip6tables -N LOG_AND_DROP
ip6tables -A IN_SSH -m recent --name sshbf --rttl --rcheck --hitcount 3 --secounds 60 -j LOG_AND_DROP
ip6tables -A IN_SSH -m recent --name sshbf --rttl --rcheck --hitcount 4 --seconds 1800 -j LOG_AND_DROP
ip6tables -A IN_SSH -m recent --name sshbf --set -j ACCEPT
ip6tables -A LOG_AND_DROP -j LOG --log-prefix "ip6tables deny: " --log-level 7
ip6tables -A LOG_AND_DROP -j DROP
```

Enable reverse path filter for IPv6:

```bash
ip6tables -t raw -A PREROUTING -m rpfilter -j ACCEPT
ip6tables -t raw -A PREROUTING -j DROP
```

Before save, lets backup the old ip6tables default's file:

```bash
cp /etc/iptables/ip6tables.rules /etc/iptables/ip6tables.rules.back
```

Save the ip6table's configuration into the default's file:

```bash
ip6tables-save -f /etc/iptables/ip6tables.rules
```
