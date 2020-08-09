# Haskell Universal Config Manager

This package helps updating config files. It aims to support every
config file syntax and when applicable provides semantic updates.
Although for now only Systemd Service files are supported.

It provides a CLI executable so it can be easily integrated in bash
scripts, for example. It also provides a library to allow users to
create more abstract config changes using the library.

# Install from github tarballs

To download the binary from the github, follow these steps:
1. Go to the latest github action run
2. Download the artifact.
3. Unzip it with `unzip haskell-uconfig.zip`
4. set the executable permission bit with `chmod a+x haskell-uconfig/uconfig`
5. install it with `cp ./haskell-uconfig/uconfig ~/bin/`
6. run it with `uconfig ARGS`

# Usage

Print help usage.

```
$ uconfig --help
Usage: uconfig [-d|--debug] FILETYPE FILENAME [UPDATE...]
  Update config files while keeping formatting and comments.

Available options:
  -d,--debug               Enable debug output (shows intended updates if any
                           and internal representation)
  FILETYPE                 File type of the file to update, currently only
                           'systemdservice' is the only one supported.
  FILENAME                 Path to file that will get modified.
  UPDATE...                Updates of the form field-operator-value (ex:
                           Unit.Description='my service')
  -h,--help                Show this help text
```

Print the config file untouched after having parsed the config file.
Unhandled fields are silently dropped.

```
$ uconfig systemdservice /etc/systemd/system/multi-user.target.wants/ntpd.service
[Unit]
Description=Network Time Service
After=network.target nss-lookup.target
Conflicts=systemd-timesyncd.service

[Service]
Type=forking
PrivateTmp=true
ExecStart=/usr/bin/ntpd -g -u ntp:ntp
Restart=always

[Install]
WantedBy=multi-user.target
```

Print the new config file after applying several modifications, in
order:
- Replace `Unit.Description`'s value
- Change value of `Service.PrivateTmp` to `no`
- Add a target to `Install.WantedBy`
- Remove `Unit.Conflicts` field

```
$ uconfig systemdservice /etc/systemd/system/multi-user.target.wants/ntpd.service \
  Unit.Description='My NTPD Service' \
  Service.PrivateTmp=no \
  Install.WantedBy+=network.target \
  Unit.Conflicts=
[Unit]
Description=My NTPD Service
After=network.target nss-lookup.target

[Service]
Type=forking
PrivateTmp=false
ExecStart=/usr/bin/ntpd -g -u ntp:ntp
Restart=always

[Install]
WantedBy=multi-user.target
WantedBy=network.target
```

# Building

Building and running the app is done with:

```
stack build --haddock --haddock-deps
stack run
```

The haddock arguments are used to generate the documentation.

Running the tests is done with:

```
stack test
```

Looking at the documentation is best done through using hoogle, to do
that, run:

```
stack hoogle -- server --local --port=65000
```

Then point your browser at http://localhost:65000.

# TODO

- Support generic filetype with updates
- Support updates for known filetypes (make Unit.After-=XXX work as expected)
